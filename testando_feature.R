# Carregando pacotes --------------------------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(rvest)
library(quantmod)
library(httr)
library(tibble)
library(stringr)
library(reshape2)
library(readr)
library(purrr)

# Url da partida
string_url <- 'https://www.vlr.gg/167037/tropicaos-vs-keyd-stars-challengers-league-brazil-split-1-w3'

# Pegando os dados no link da partida ----------------------------------------------------------------------
links_jogadores <- read_html(string_url) %>% 
  html_nodes('td.mod-player a') %>% 
  html_attr('href')

timeA <- links_jogadores[1:5]
timeB <- links_jogadores[6:10]
#  -----------------------
n <- 1
for (i in timeA){
  timeA[n] <- paste('https://www.vlr.gg', '/?timespan=all', sep = i)
  n = n + 1
}

n <- 1
for(i in timeB){
  timeB[n] <- paste('https://www.vlr.gg', '/?timespan=all', sep = i)
  n = n + 1
}

rm(links_jogadores, n, string_url)

pegarMedias <- function (url_jogador) {
  html_lido <- read_html(as.character(url_jogador))
  
  dados_jogador <- html_nodes(html_lido, 'table') %>%
    html_table()
  dados_jogador <- dados_jogador %>% map_df(as_tibble, .name_repair = 'minimal') %>%
    dplyr::select(Rating, ACS, 'K:D', ADR, KAST)
  
  dados_jogador$KAST <- parse_number(dados_jogador$KAST)
  
  means_jogador <- round(colMeans(dados_jogador, na.rm = T), 2)
  
  means_jogador[['KAST']] <- round(means_jogador[['KAST']], 0)
  
  return(means_jogador)
}

timeA_medias <- list()
timeB_medias <- list()

for (i in timeA){
  timeA_medias[[length(timeA_medias)+1]] <- pegarMedias(i)
}

for (i in timeB){
  timeB_medias[[length(timeB_medias)+1]] <- pegarMedias(i)
}
