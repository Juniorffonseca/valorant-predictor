# Carregando pacotes -------------------------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(rvest)
library(httr)
library(ggplot2)

# Carregando o dataframe -----------------------------------------------------------------------------------
jogos <- read.csv2('csv/df.csv') %>% dplyr::select(-X)
jogadores <- read.csv2('csv/jogadores.csv') %>% dplyr::select(-X)

ggplot(jogos, aes(x, y))
