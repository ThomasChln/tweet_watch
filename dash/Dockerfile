from rocker/shiny

run apt-get update && apt-get install -y libssl-dev libssh2-1-dev libxml2-dev
run R -e "install.packages('devtools')"
run R -e "devtools::install_github(c('rstudio/flexdashboard', 'rstudio/DT', 'rstudio/dygraphs', 'rstudio/leaflet'))"
run R -e "install.packages(c('RSQLite', 'tm', 'SnowballC', 'rNMF', 'lubridate'))"
