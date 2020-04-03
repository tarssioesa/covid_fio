# Script cria o acompanhamento geral das publicações ao longo do tempo


# Library

require(rjson)
require(tidyverse)
require(lubridate)
require(tm)
require(nls2)
require(boot)
require(gmodels)

# lCarregando os arquivos da pasta:

folder <- c("data/")

k <- list.files("data/")

# Lendo arquivos: 


df_create <- function(x){
  
  aux <- readRDS(file = paste0(folder,k[x])) %>% 
    select(screen_name, text, created_at, favorite_count, retweet_count, 
           reply_to_screen_name, quote_count, reply_count)
  
}


df <- 1:length(k) %>% 
  map_dfr(df_create)


### Selecionando variaveis e ajustando tempo:

tbl <- df %>% 
  mutate(difusao = retweet_count, 
         aprovacao = favorite_count, 
         impressao = difusao + aprovacao) %>% 
  select(screen_name, text, created_at, difusao, aprovacao, impressao)

tbl_ajuste <- tbl %>% 
  mutate(time = lubridate::ymd_hms(created_at, tz = "America/Bahia")) %>% 
  mutate(time = lubridate::round_date(created_at, unit = "hour")) %>% 
  select(-created_at)

#### Grafico acumulados para todas publicações 
#### dada a primeira aparição de determinado usuário para escala temporal: Diáriag


tbl_acc <- tbl_ajuste %>% 
  distinct(screen_name, .keep_all = TRUE) %>% 
  group_by(time) %>% 
  summarise(n = n()) %>% 
  mutate(n = cumsum(n)) %>% 
  mutate(t = as.numeric((time - min(time))/3600)+ 1)


tbl_acc %>% 
  ggplot(aes(x = t, y = n)) +
  geom_line() + 
  labs(x = "Tempo em horas a partir do dia 17/03/2020", 
       y = "Quantidade de novos perfis") +
  theme_minimal()


# Fitting a exponental curve using bootstrap:


exp_ <- function(t, R, d){

  (R/((1+d)^t))^t
} 

exp_(1,1000,0.01)

Ro <- function(data){
  
data <- tbl_acc  %>% 
  sample_frac(0.7)
  

aux <- nls2(n ~ exp_(t, R, d), data = data, start = list(R = 1000, d = 0.05), 
    trace = F, 
    control = nls.control(maxiter = 1000, minFactor=2^-12))

Ro <- summary(aux)[["coefficients"]][1,1]

}

results <- boot(tbl_acc, Ro, 1000, "parametric")

ci(results$t, 0.95)

# Plotting final: 


aux <- nls2(n ~ exp_(t, R, d), data = data, start = list(R = 1000, d = 0.05), 
            trace = F, 
            control = nls.control(maxiter = 1000, minFactor=2^-12))

Ro <- summary(aux)[["coefficients"]][1,1]

d <- summary(aux)[["coefficients"]][1,2]



tbl_acc %>% 
  mutate(stat = predict(aux,tbl_acc)) %>% 
  ggplot() +
  geom_line(aes(x = t, y = n, col = "black")) +
  geom_line(aes(x = t, y = stat, col = "red")) +
  ggplot2::annotate("text",  label = paste0("Ro =", "", round(Ro,3)), 
           x = 60, y = 0.7*max(tbl_acc$n)) +
  scale_color_manual(values = c("black", "red"), 
                     labels = c("Observado", "Modelado")) +
  labs(col = "", title = "Ajuste exponencial para todas publicações coletadas") +
  theme_bw()

  

