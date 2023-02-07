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

# Criando um laço for que armazenará o url de cada página dentro da variável paginas -----------------------
for (i in timeA){
  teste <- paste('https://www.vlr.gg', '/?timespan=all', sep = i)
}

teste_1 <- 'https://www.vlr.gg/player/1586/v1nny/?timespan=all'

infos_jogadores <- read_html(teste_1) %>% 
  html_nodes('table') %>% 
  html_table()

infos_jogadores <- infos_jogadores %>%  map_df(as_tibble, .name_repair = 'minimal')
