## load rtweet package
library(rtweet)
library(dplyr)
library(ggplot2)
library(lubridate)

data <- system("date +\"%Y%m%d%H%M\"",intern=TRUE) 
termos <- c("coronavirus", "covid", "cloroquina", "quarentena", "covid19", "corona")

token <- create_token(
  app = "my_twitter_research_app",
  consumer_key = "3I8JehozX8N4Bojg0qSdmDFLX",
  consumer_secret = "QBg2E2TcqtvlK0vrRIwIWZGSXjlPoYaLXwjrePRDLztepAU6cg",
  access_token = "2433072234-seryr8c6OCyNU5veonMKf5hvX8JKCUOiA20TcoC",
  access_secret = "rHOyUzdhL5GN6lqWh3LOxmuKD2hb0IvpeJm4hrs24O4nx"
)   


for(i in 1:length(termos)){
  termo <- termos[i]
  tweets <- search_tweets(termo, n = 100000, type="recent",
                          retryonratelimit = TRUE,
                          include_rts = TRUE, 
                          lang = "pt")
  tweets$termo <- rep(termo, nrow(tweets));
  saveRDS(tweets, file = paste0("data/tweets2_",termo,"_",data,".rds"))
}
