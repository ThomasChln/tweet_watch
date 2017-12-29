translate_tors = function() {
  setwd('dash')
  source('groups.R')
  df_all = fetch_df()
  df_fr = get_df_fr(fetch = FALSE)
  setwd('..')

  fr_idx = match(df_fr$id_str, df_all$id_str)

  df_all$en_trans[fr_idx] = df_fr %>% `[`(-match('en_trans', colnames(.))) %>%
    translate %$% en_trans %>% as.character

  save_df(df_all, 'dash/tw.db', append = FALSE, overwrite = TRUE)
}

get_df_fr = function(df, fetch = TRUE) {
  if (missing(df)) df = get_df()
  df %<>% subset(grepl('FR', withheld_in_cn))
  if (fetch) {
    df %$% id_str %>%
      fetch_tweets %>% sapply(`[`, c('id_str', 'text')) %>% t %>%
      as.data.frame
  } else df
}

get_tors = function() {

  setup()
  setwd('dash')
  source('groups.R')

  df_all = fetch_df()
  df = get_df()

  df_fr = get_df_fr(df)
  fr_idx = match(df_fr$id_str, df_all$id_str)

  df_all$text[fr_idx] = df_fr$text %>% unlist

  save_df(df_all, 'tw.db', append = FALSE, overwrite = TRUE)
}

save_df = function(df, path = 'dash/tw.db', append = TRUE, ...) {
  RSQLite::dbConnect(RSQLite::SQLite(), path) %T>%
    RSQLite::dbWriteTable('m_tws', df, append = append, ...) %>%
    RSQLite::dbDisconnect()
}

library(magrittr)
monit = function() {
  on.exit(traceback())

  setup()
  geocmds = get_geo()
  m = load_or_search(geocmds)
  times = format(Sys.time(), tz = 'UTC') %>% lubridate::ymd_hms() %>% rep(4) 

  force_backup = FALSE
  wait_backup = 60 * 60 * 4
  wait_save = 60 * 20
  wait_empty = 60 * 5
  wait_check = 60 * 3
  wait_search = 60 * 4
  n_search = 8e3

  while (1) {
    if (diffsecs(times[1:2]) > wait_check) {
      m$df %<>% check_status(n_search, times[1])
      times[2] = times[1]
    }
    if (force_backup || diffsecs(times[c(1, 4)]) > wait_backup) {
      m$df %<>% save_backup(wait_backup, times[1])
      force_backup = FALSE
      times[4] = times[1]
    }
    times[3] = save_tw(times[c(1, 3)], m, wait_save)

    time_idx = sapply(m$times, city_time, times[1]) %>% which
    if (length(time_idx)) m = get_m_tws(geocmds, m, time_idx, wait_empty)

    time = lubridate::ymd_hms(format(Sys.time(), tz = 'UTC'))
    time_wait = wait_search - as.numeric(time - times[1])
    if (time_wait > 0) {
      write(Sys.time(), 'log.txt', append = TRUE)
      write(paste('Wait', time_wait), 'log.txt', append = TRUE)
      Sys.sleep(time_wait)
    }
    times[1] = time + max(0, time_wait)
  }
}

fetch_translate = function(text) {
  gsub('"', '&dquot;', text) %>%
    gsub('`', '&bquot;', .) %>%
    paste0('nodejs translate.js "', ., '"') %>% system(TRUE)
}

build_translate = function(resp) {
  resp[-length(resp)] %>% paste(collapse = ' ') %>%
    cbind(en_trans = ., lang = resp[length(resp)])
}

translate = function(df) {
  df_trans = subset(df, !duplicated(text))

  m_resp = df_trans$text %>% setNames(., .) %>%
    lapply(fetch_translate) %>%
    lapply(build_translate) %>% {
      cbind(text = names(.), do.call(rbind, .))
    }

  merge(m_resp, df, by = 'text', sort = FALSE)
}

save_backup = function(df, wait_backup, time) {
  time_diffs = lapply(df$created_at, function(i) diffsecs(c(time, i))) 
  backup_idxs = which(time_diffs > wait_backup)

  backup = df[backup_idxs, ] %>% subset(!sapply(withheld, is.null))

  if (nrow(backup) > 0) {
    backup[, 6:8] %<>% lapply(sapply, paste, collapse = ';')
    backup %<>% translate
 
    save_df(backup)
  }

 df[-backup_idxs, ]
}
###
get_m_tws = function(geocmds, m, time_idx, ...) {
  new_m = search_cities(geocmds[time_idx, ], m$id[time_idx])
  empties = seq_along(time_idx) %>%
    sapply(function(i) m$id[time_idx[i]] == new_m$id[i])

  m$times[time_idx][empties] %<>% lapply(delay, ...)
  time_idx = time_idx[!empties]

  if (length(time_idx)) {
    nulls = sapply(new_m$times[!empties], is.null)
    m$times[time_idx[!nulls]] = new_m$times[!empties][!nulls]
    m$times[time_idx[nulls]] %<>% lapply(delay, ...)
    m$id[time_idx] = new_m$id
  }
  m$df %<>% plyr::rbind.fill(new_m$df, .)
  m
}
###
delay = function(times, wait) {
  times[2] = times[2] - wait
  times
}
###
diffsecs = function(x) {
  x = x[1] - x[2]
  x = x * switch(attr(x, 'units'),
    secs = 1, mins = 60, hours = 3600, days = 3600 * 24)
  attr(x, 'units') = 'secs'
  x
}
###
city_time = function(city_times, time) {
  diffsecs(c(time, city_times[1])) > diffsecs(city_times) * .9
}
###
###
search_geocode = function(geo, sinceID = NULL, n = 100, q = '') {
  params = paste0(geo, collapse = ',') %>%
    list(geocode = ., q = q, result.type = 'recent', include_entities = FALSE) 
  if (!is.null(sinceID)) params$since_id = sinceID

  tweets = try(twitteR:::doRppAPICall('search/tweets', n, params, retryOnRateLimit = 0))

  fields = c('id_str', 'text', 'created_at')
  if (is(tweets, 'try-error')) {
    matrix(nrow = 0, ncol = length(fields) + 1,
      dimnames = list(NULL, c(fields, 'user')))
  } else {
    sapply(tweets,
      function(tw) c(unlist(tw[fields]), user = tw$user$screen_name)) %>% t
  }
}
###
get_times = function(city_, df) {
  if (nrow(df) < 2) return(NULL)
  df %<>% subset(city == city_)
  n = min(100, nrow(df))
  times = df$created_at[c(1, n)]
  times[2] = times[1] - diffsecs(times) * 100 / n
  times
}
###
.search_cities = function(i, geocmds, since_ids, ...) {
  m = search_geocode(geocmds[i, -1], sinceID = since_ids[i], ...)
  paste(geocmds$cities[i], length(m) / 4) %>% write('log.txt', append = TRUE)
  
  if (length(m)) {
    cbind(city = geocmds$cities[i], m) %>%
      list(m = ., i = sort(m[, 'id_str'], TRUE)[1])
  } else {
    list(m = NULL, i = since_ids[i])
  }
}    
###
search_cities = function(geocmds, ...) {
  m_tws = seq_len(nrow(geocmds)) %>% lapply(.search_cities, geocmds, ...)
  df = lapply(m_tws, '[[', 'm') %>% do.call(rbind, .) %>%
    data.frame(stringsAsFactors = FALSE)

  if (nrow(df)) {
    df$created_at %<>% unlist %>% strsplit(' ') %>%
      sapply(function(i) paste(c(i[6], i[-c(1, 6)]), collapse = ' ')) %>%
      lubridate::ymd_hms()
  }

  lapply(geocmds$cities, get_times, df) %>%
    list(df = df, id = sapply(m_tws, '[[', 'i'), times = .)
}
###
###
veclist <- function(ids, size = 100) {
  len = seq(0, length(ids), size) + 1
  if (length(ids) %% size != 0) len = c(len, length(ids) + 1)
  lapply(seq_along(len)[-1], function(i) ids[len[i - 1]:(len[i] - 1)])
}
###
fetch_tweets <- function(ids) {
  url = twitteR:::getAPIStr('statuses/lookup')

  config = httr::config(token = twitteR:::get_oauth_sig())

  query = list(id = paste(ids, collapse = ',')) %>%
    lapply(as.character) %>% lapply(URLencode)

  try(twitteR:::tw_from_response(httr::GET(url, config, query = query)))
}
###
append_time = function(m_tws, v_append, name, time, append_vec_str) {
  if (missing(append_vec_str)) {
    m_tws[[name]][v_append] %<>% lapply(append, time)
  } else {
    time %<>% paste0('_', append_vec_str)
    m_tws[[name]][v_append] %<>% {
        lapply(seq_along(.), function(i) append(.[[i]], time[i]))
      }
  }
  m_tws
}
###
check_status = function(m_tws, n = 100, time = Sys.time()) {
  samp = seq_along(m_tws[, 1]) %>% sample(min(nrow(m_tws), n)) 

  tws_samp = m_tws$id_str[samp]
  ids =  veclist(tws_samp)

  tweets = lapply(ids, fetch_tweets)
  errs = which(sapply(tweets, is, 'try-error'))
  if (length(errs)) {
    samp = samp[-match(unlist(ids[errs]), tws_samp)]
    tweets = tweets[-errs]
  }
  chks = unlist(tweets, FALSE)

        
  ord = sapply(chks, '[[', 'id_str') %>% match(tws_samp)
  missmatch = (!sapply(seq_along(chks),
    function(i) identical(chks[[i]]$text, m_tws$text[samp][ord[i]]))) %>% which
        
  if (length(missmatch)) {
    withheld = sapply(chks[missmatch], `[[`, 'text') %>%
      grepl('withheld in: France|report fron copyright holder', .) %>% all

    if (!withheld) {
      paste('Mismatch:\n', chks[missmatch]) %>% write('log.txt', append = TRUE)
      return(m_tws)
    }
  }

  build_df_status(m_tws, chks, samp, ord, time)
}

build_df_status = function(m_tws, chks, samp, ord, time) {

  if (!'checked' %in% names(m_tws)) {
    m_tws[c('checked', 'missed', 'withheld')] = list(list(NULL))
  }
        
  m_tws %<>% append_time(samp[ord], 'checked', time)
  m_tws %<>% append_time(samp[-ord], 'missed', time)
        
  paste(length(samp[-ord]), 'missing') %>% write('log.txt', append = TRUE)

        
  withheld = chks %>%
    sapply(function(i) 'withheld_in_countries' %in% names(i)) %>% which
  if (length(withheld)) {
    countries = sapply(chks[withheld],
      function(i) paste(unlist(i$withheld_in_countries), collapse = '_'))
    m_tws %<>% append_time(samp[ord[withheld]], 'withheld', time, countries)
    paste(rep('-', 50), collapse = '') %>%
      paste(length(withheld), 'withheld') %>% write('log.txt', append = TRUE)
  }

  m_tws
}
###
load_or_search = function(geocmds) {
  if (file.exists('m_tws.rds')) {
    get(load('m_tws.rds'))
  } else {
    search_cities(geocmds, NULL)
  }
}
save_tw = function(times, m, diff = 120) {
  if (diffsecs(times) > diff) {
    times[2] = times[1]
    save(m, file = 'm_tws.rds')
    'save' %>% write('log.txt', append = TRUE)
  }
  times[2]
}
get_geo = function() {
  filename = 'geo.rds'
  if (!file.exists(filename)) {
    radius = '10km'

    df_cities = 'https://en.wikipedia.org/wiki/List_of_urban_areas_in_Europe' %>%
      RCurl::getURL() %>% XML::readHTMLTable() %>% `[[`(1) %>%
      setNames(c('city', NA, NA, NA, 'pop'))

    df_cities$pop %<>% gsub('[[].*', '', .) %>% gsub('.*[^0-9,]|,', '', .) %>%
      as.numeric

    subset(df_cities, pop > 1.45e6)$city %>%
      gsub('[–/].*|[^[:alpha:] –-]*| urban area| Rhine.*', '', .) %>%
      cbind(cities = ., ggmap::geocode(., source = 'dsk')[2:1], radius,
        stringsAsFactors = FALSE) %>%
      na.omit %>% save(file = filename)

    file.copy(filename, file.path('dash', filename))
  }
  get(load(filename))
}
setup  = function() {
  load('twitterCred.rda')
  require(ROAuth)
  cred %$% twitteR::setup_twitter_oauth(consumerKey, consumerSecret, oauthKey,
      oauthSecret)
}
