# Carregando pacotes --------------------------------------------------------------------------------------
library(rvest)
library(quantmod)
library(httr)
library(tibble)
library(stringr)
library(reshape2)
library(tidyverse)
library(neuralnet)
library(readr)
library(purrr)
library(valorant)
library(lubridate)
setwd('C:/Users/anonb/Documents/TCC_Pós/Scripts')

# Carregando arquivos csv ---------------------------------------------------------------------------------
nome_arquivo_urls <- paste(Sys.Date(), '_urls.csv', sep = '')
nome_arquivo_partidas <- paste(Sys.Date(), '_partidas.csv', sep = '')

b <- read.csv2(paste('csv/catalogacao_diaria/', nome_arquivo_urls, sep = '')) %>% select(-X) %>% unlist()
dff <- read.csv2(paste('csv/catalogacao_diaria/', nome_arquivo_partidas, sep = '')) %>% select(-X)

ganhador <- '' %>% .[0]

for (i in b){
  ganhador[length(ganhador)+1] <- get_Ganhadores(i)
}

dff <- cbind(dff, ganhador)

write.csv2(dff, paste('csv/catalogacao_diaria/', nome_arquivo_partidas, sep = ''))
