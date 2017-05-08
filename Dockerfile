from thomaschln/r-devtools

run R -e "install.packages('lubridate')"
run R -e "install.packages('twitteR')"
run R -e "install.packages('RSQLite')"
run R -e "install.packages('ROAuth')"
run R -e "install.packages('plyr')"

run apt-get update 
run apt-get install -t unstable -y libxml2-dev
run R -e "install.packages('XML')"
run R -e "install.packages('ggmap')"

run apt-get install npm -y
run npm install git+https://git@github.com/arthurlacoste/google-translate-api.git
