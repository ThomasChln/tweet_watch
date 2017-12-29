fetch_df = function() {
  RSQLite::dbConnect(RSQLite::SQLite(), 'tw.db') %>%
    RSQLite::dbSendQuery('select * from m_tws') %>% DBI::fetch(-1)
}

get_df = function() {
  fetch_df() %>% merge(get(load('geo.rds')), by.x = 'city',
      by.y = 'cities', sort = FALSE) %>% get_trans %>% get_hour %>% get_country
}

get_trans = function(df) {
  df$en_trans %<>% gsub('(# )+', '# ', .) %>% gsub('([#@]) ', '\\1', .) %>%
    gsub('^RT[^:]+: |…', '', .)
  df %>% subset(!duplicated(en_trans)) %>%
    subset(sapply(en_trans, pmatch_set, en_trans) %>% is.na)
}

as_hour = function(df) {
  df$created_at %>% range %>% append(list(length.out = 51)) %>%
    do.call(seq, .) %>% `[<-`(1, .[1] - 1) %>%
    { rbind.data.frame(head(., -1), .[-1]) } %>%
    { setNames(sapply(., time_sum, df), .[1, ] + round(diff(.[, 1]))) } %>%
    { factor(rep(names(.), .), names(.)) } %>% `levels<-`(as_time(levels(.))) %>% rev
}

get_hour = function(df) {
  df %<>% `[`(order(.$created_at) %>% rev, ) %>%
    cbind(created_h = as_hour(.))
  df$created_at %<>% as_time
  df
}

time_sum = function(i, df) {
  with(df, i[1] < created_at & created_at <= i[2]) %>% sum
}

get_country = function(df) {
  df$withheld_in_cn = strsplit(df$withheld, ';') %>%
    lapply(function(i) strsplit(i, '_') %>% sapply(`[`, -1) %>% as.vector %>% unique) %>%
    sapply(function(i) if (length(i) > 1) paste(i, collapse = '_') else i) %>%
    factor
  df
}

pmatch_set = function(i, set) pmatch(i, setdiff(set, i))

get_strs = function(docs) {
  docs %>% setNames(seq_along(.)) %>% tolower %>%
    gsub('[bd]*quot[;]*|http[^ ]+|[^[:alnum:]#@_]', ' ', .)
}

get_idf = function(text, ..., n_terms = 1e3) {
  idf = get_spm(text, ...) %>% stem_spm %>% tm_idf

  terms_weights = apply(idf, 1, sum)
  idf = idf[which(rank(-terms_weights) <= n_terms), ]

  idf[terms_weights != 0, apply(idf, 2, sum) != 0] %>% as.matrix %>% t
}

get_spm = function(text, n_word = 3, n_toot = 3) {
  tabs = strsplit(text, ' ') %>% lapply(table)
  nams = factor(names(unlist(setNames(tabs, NULL))))

  library(Matrix)
  spm = sapply(tabs, length) %>% rep(seq_along(.), .) %>%
    sparseMatrix(as.numeric(nams), x = unlist(tabs),
      dimnames = list(names(text), levels(nams))) %>%
    get_clean_m

  spm = spm[, colSums(spm) >= n_word]
  spm = spm[rowSums(spm) >= n_toot, ]
}

get_clean_m = function(m) {
  stop_words = tm::stopwords() %>% c(gsub("'", '', .), gsub("'.*", '', .)) %>%
    unique

  m %>%
    `[`(, !colnames(.) %in% stop_words) %>%
    `[`(, !grepl('^@', colnames(.))) %>%
    `[`(, !grepl('^[0-9]+$', colnames(.))) %>%
    `[`(, nchar(colnames(.)) > 4)
}

stem_spm = function(spm) {
  corp = colnames(spm) %>% gsub('y$', 'yzzz', .) %>%
    tm::VectorSource() %>% tm::Corpus()
  corp_stem = tm::tm_map(corp, tm::stemDocument)
  corp = tm::tm_map(corp_stem, tm::stemCompletion, dictionary = corp) %>%
    sapply(`[[`, 1) %>% gsub('yzzz$', 'y', .)
  stems = split(colnames(spm), corp)

  for (i in which(sapply(stems, length) > 1)) {
    idxs = match(stems[[i]][-1], colnames(spm))
    spm[, match(stems[[i]][1], colnames(spm))] %<>% '+'(rowSums(spm[, idxs, drop = FALSE]))
    spm = spm[, -idxs]
  }
  spm
}

tm_idf = function(spm) {
  t(spm) %>% tm::as.TermDocumentMatrix(weighting = function(x) {
        wts = tm::weightTfIdf(x)
      })
}

get_clust = function(idf, n = Inf) {
  library(mclust)
  amap::Dist(idf, 'euclidean') %>% Mclust(10) %$% classification %>%
    cbind.data.frame(clust = ., term = rownames(idf), tfidf = rowSums(idf),
      stringsAsFactors = FALSE) %>%
    split(.$clust) %>%
    lapply(function(df) df[order(df$tfidf, decreasing = TRUE) %>% head(n), ])
}

pearson_residuals <- function(contingency_table) {
  expecteds <- apply(contingency_table, 2,
    function(i, n_row, n) n_row * sum(i) / n,
    rowSums(contingency_table), sum(contingency_table))
  residuals <- (contingency_table - expecteds) / sqrt(expecteds)
}

name_clusters <- function(clust, labels, append_unique = FALSE,
  residual_threshold = 2) {
  tab <- table(labels, clust)
  residuals <- pearson_residuals(tab)
  lvls <- levels(labels)[apply(residuals, 2, which.max)]

  not_enriched <- sapply(seq_len(ncol(tab)),
    function(i) residuals[lvls[i], i]) < residual_threshold
  lvls[not_enriched] <- seq_len(sum(not_enriched))

  for (lvl in unique(lvls)) {
    if (append_unique || sum(lvl == lvls) > 1) {
      lvl_rank <- rank(-residuals[lvl, lvls %in% lvl], ties.method = 'first')
      lvls[lvls %in% lvl] <- paste0(lvl, '_', lvl_rank)
    }
  }

  clust <- setNames(lvls[clust], names(clust))

  factor(clust, lvls)
}

threshold_recode = function(vec, min_ratio) {
  table(vec) %>% `[`(order(names(.))) %>%
    `[`(`>`(., sum(.) / length(.) * min_ratio)) %>% names %>% factor(vec, .)
}

get_df_clust = function(df, n_topics, ...) {
  idf = get_strs(df$en_trans) %>% get_idf

  df_clust = df[rownames(idf) %>% as.numeric, ]
  #df_clust = get_clust(idf)
  #fit = amap::Dist(idf, 'euclidean') %>% Mclust(10)
  #t(idf) %*% t(fit$parameters$mean %>% t %>% `/`(rowMeans(.))) %>% apply(1, which.max) %>% table

  nmf <- rNMF::rnmf(idf, k = n_topics, showprogress = FALSE, my.seed = 100)

  df_clust$clust = apply(nmf$W, 1, which.max) %>%
    name_clusters(df_clust$withheld_in_cn)

  m_proj = `rownames<-`(nmf$H, df_clust$clust %>% levels)

  df_clust$city %<>% threshold_recode(...)
  df_clust$clust %<>% threshold_recode(...)

  m_proj %>% t %>% data.frame %>% cbind(idf_ids = colnames(idf)) %>%
    plyr::rbind.fill(df_clust, .)
}

get_df_groups_sum = function(df, ...) {
  get_topics(df, ...) %>% t %>%
    cbind.data.frame(n = table(df$clust) %>% as.vector, .) %>%
    `[`(order(.$n) %>% rev, )
}

get_df_groups = function(df, ...) {
  df %$% lapply(list(withheld_in_cn, city, lang), table, clust) %>%
    append(list(get_topics(df, ...))) %>% do.call(rbind, .)
}

.get_topics = function(i, df, n_words = 5) {
  na.omit(df$idf_ids)[order(i) %>% tail(n_words) %>% rev]
}

get_topics = function(df, ...) {
  levels(df$clust) %>% matrix(ncol = length(.), dimnames = list(NULL, .)) %>%
    data.frame %>% names %>% `[`(df, .) %>% na.omit %>%
    apply(2, .get_topics, df, ...)
}

get_table = function(df, small = TRUE) {
  df$withheld_at = gsub('_.*', '', df$withheld)
  df$link = with(df,
    paste('https://twitter.com', user, 'status', id_str, sep = '/'))

  df[c('withheld_in_cn', 'city', 'lang')] %<>% lapply(factor)
  df$text %<>% gsub('[^[:alnum:][:punct:]]+', ' ', .)
  df
}

as_time = function(time) {
  time %>% as.character %>% `class<-`('POSIXct') %>% format(tz = 'GMT') %>%
    lubridate::ymd_hms()
}
