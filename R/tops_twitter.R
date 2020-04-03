####  Determina os 50 twitters mais citados, mais rt, 
#### mais like, mais publicacoes:


# Require

library(quanteda)
library(tidytext)
library(tidyverse)
library(stringi)
library(quanteda)

# Determinando users:

## Carregando token com os objetos textuais:

load(file = "token.RData")

# Mais citados: 

all_dfm <- dfm(toks_news) %>%
  dfm_trim(min_termfreq = 10)

user_dfm <- dfm_select(all_dfm, ('@*'))  #Busca pelos @

user_fcm <- fcm(user_dfm)

topuser <- names(topfeatures(user_dfm, 50)) # Retorna os 50

mais_citados <- head(topuser,50)


# Mais curtidos

# Carregando todos os dados

folder <- c("data/")

k <- list.files("data/")

# Read: 

df_create <- function(x){
  
  aux <- readRDS(file = paste0(folder,k[x])) %>% 
    select(screen_name, text, created_at, favorite_count, retweet_count, 
           reply_to_screen_name, quote_count, reply_count, is_retweet)
  
}


df <- 1:length(k) %>% 
  map_dfr(df_create)

# Ajustes: 

tbl <- df %>% 
  mutate(difusao = retweet_count, 
         aprovacao = favorite_count, 
         impressao = difusao + aprovacao) %>% 
  select(screen_name, text, created_at, difusao, aprovacao, impressao, 
         is_retweet)

tbl_ajuste <- tbl %>% 
  mutate(time = lubridate::ymd_hms(created_at, tz = "America/Bahia")) %>% 
  mutate(time = lubridate::round_date(created_at, unit = "hour")) %>% 
  select(-created_at) %>% 
  filter(is_retweet == FALSE)

# Usuarios mais publicacoes: 

pub <- tbl_ajuste %>% 
  group_by(screen_name) %>% 
  summarise(n = n()) %>% 
  arrange(-n)

pub <- head(pub, 50)


# Usuarios mais rt

rt <- tbl_ajuste %>% 
  group_by(screen_name) %>% 
  summarise(n = sum(difusao)) %>% 
  arrange(-n)

rt <- head(rt, 50)



# Usuarios mais like


like <- tbl_ajuste %>% 
  group_by(screen_name) %>% 
  summarise(n = sum(aprovacao)) %>% 
  arrange(-n)

like <- head(like, 50)


# GErando csv: 

top_twitter <- cbind(mais_citados, pub$screen_name, 
                     rt$screen_name, like$screen_name) %>% as.tibble()


names(top_twitter) <- c("Citações", "Publicações", 
                         "Rt", "Likes")

# write.csv(top_twitter, file = "top_twitter.csv")
