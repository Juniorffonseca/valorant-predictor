# Instalando (se necessário) e carregando pacotes ----------------------------------------------------------
remotes::install_github('Juniorffonseca/r-pacote-valorant')
library(caret)
library(dplyr)
library(tidyr)
library(rvest)
library(rsample)
library(readr)
library(quantmod)
library(httr)
library(tibble)
library(stringr)
library(neuralnet)
library(nnet)
library(caret)
library(ggplot2)
library(ModelMetrics)
library(beepr)
library(purrr)
library(plotly)
library(pROC)
library(ROCR)
library(kableExtra)
library(glmnet)
library(valorant)
setwd('C:/Users/anonb/Documents/TCC_Pós/Scripts')

# Carregando partidas diarias e unindo em um df ------------------------------------------------------------
datas <- seq(as.Date('2023-02-19'), as.Date('2023-04-09'), by = 'day')
nomes_arquivos <- paste0('csv/catalogacao_diaria/', format(datas, '%Y-%m-%d'), '_urls.csv')

urls_lista <- list()

for (arquivo in nomes_arquivos) {
  urls_lista[[arquivo]] <- possibly(read.csv2, otherwise = NULL)(arquivo)
}

urls_lista <- lapply(urls_lista, function(df) {
  df$x <- as.character(df$x)
  return(df)
})

urls <- bind_rows(urls_lista) %>% select(-X)

write.csv2(urls, 'csv/urls_utilizados.csv')

nomes_times <- matrix(nrow = 0, ncol = 2)

for (url in urls[,]){
  nomes_times <- read_html(url) %>% html_nodes("div.wf-title-med") %>% 
    html_text() %>% str_replace_all("\n", "") %>% str_replace_all("\t", "") %>%
    rbind(nomes_times)
}

nomes_times <- as.data.frame(nomes_times) %>%
  `rownames<-`(NULL) %>% slice(rev(row_number()))

write.csv2(nomes_times, 'csv/times_catalogados.csv')

nomes_jogadores <- matrix(nrow = 0, ncol = 10)

for(url in urls[,]){
  nomes_jogadores <- read_html(url) %>% html_nodes("td.mod-player a") %>% 
    html_text() %>% str_replace_all("\n", "") %>% str_replace_all("\t", "") %>%
    .[1:10] %>% rbind(nomes_jogadores)
}

nomes_jogadores <- as.data.frame(nomes_jogadores) %>%
  `rownames<-`(NULL) %>% slice(rev(row_number()))

write.csv2(nomes_jogadores, 'csv/jogadores_catalogados.csv')

# Número de times únicos:
count(unique(nomes_times)) #628/674

# Número de jogadores únicos:
sum(sapply(nomes_jogadores, function(x) length(unique(x)))) #5466/6740

# Tentando pegar países de cada jogador na amostra
paises_jogadores <- matrix(nrow = 0, ncol = 10)

for(url in urls[,]){
  paises_jogadores <- read_html(url) %>% html_nodes('td.mod-player') %>% html_nodes('.flag') %>% 
    html_attr('title') %>% .[1:10] %>% rbind(paises_jogadores)
}

# Tentando pegar países de cada time na amostra
for(url in urls[,]){
  nomes_jogadores <- read_html(url) %>% html_nodes("div.match-header-vs a") %>% 
    html_text() %>% str_replace_all("\n", "") %>% str_replace_all("\t", "") %>%
    .[1:10] %>% rbind(nomes_jogadores)
}