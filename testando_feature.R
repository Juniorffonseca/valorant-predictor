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

medias_jogadores <- function (url_jogador) {
  html_lido <- read_html(as.character(url_jogador))
  
  dados_jogador <- html_nodes(html_lido, 'table') %>%
    html_table()
  dados_jogador <- dados_jogador %>% map_df(as_tibble, .name_repair = 'minimal') %>%
    dplyr::select(Rating, ACS, KAST, 'K:D', ADR)
  
  dados_jogador$KAST <- parse_number(dados_jogador$KAST)
  
  means_jogador <- round(colMeans(dados_jogador, na.rm = T), 2)
  
  means_jogador[['KAST']] <- round(means_jogador[['KAST']], 0)
  
  return(means_jogador)
}

# Url da partida
medias_times('https://www.vlr.gg/167037/tropicaos-vs-keyd-stars-challengers-league-brazil-split-1-w3')

medias_times <- function (string_url){
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
  
  timeA_medias <- list()
  timeB_medias <- list()
  
  for (i in timeA){
    timeA_medias[[length(timeA_medias)+1]] <- medias_jogadores(i)
  }
  
  for (i in timeB){
    timeB_medias[[length(timeB_medias)+1]] <- medias_jogadores(i)
  }
  
  timeA_medias <- round(rowMeans(as.data.frame(timeA_medias)), 2) %>% t %>% as.data.frame
  timeA_medias$KAST <- round(timeA_medias$KAST, 0)
  
  timeB_medias <- round(rowMeans(as.data.frame(timeB_medias)), 2) %>% t %>% as.data.frame
  
  partida <- cbind(timeA_medias, timeB_medias)
  
  colnames(partida) <- c('time1R', 'time1ACS', 'time1KAST', 'time1KD', 'time1ADR', 
                         'time2R', 'time2ACS', 'time2KAST', 'time2KD', 'time2ADR')
  
  partida <- select(partida, 'time1R', 'time2R', 'time1ACS', 'time2ACS', 'time1KAST', 'time2KAST', 'time1KD', 'time2KD',
                    'time1ADR', 'time2ADR')
  
  return(partida)
  
}
