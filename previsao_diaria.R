# Instalando pacotes (se necessário) e carregando ----------------------------------------------------------
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
library(caret)
library(ggplot2)
library(ModelMetrics)
library(beepr)
library(purrr)
library(valorant)
setwd('C:/Users/anonb/Documents/TCC_Pós/Scripts')

load(file = 'rede_neural_10_04_2023.rda')

nome_arquivo_urls <- paste(Sys.Date(), '_urls.csv', sep = '')
nome_arquivo_previsoes <- paste(Sys.Date(), '_previsoes.csv', sep = '')
b <- read.csv2(paste('csv/catalogacao_diaria/', nome_arquivo_urls, sep = '')) %>% select(-X) %>% unlist()

previsoes <- matrix(nrow = 0, ncol = 2)

for (url in b){
  # prever as variáveis
  prev <- prever(url)
  
  # criar matriz temporária com duas colunas para armazenar as previsões
  temp <- matrix(0, nrow = 1, ncol = 2)
  temp[1, ] <- prev
  
  # adicionar as previsões à matriz
  previsoes <- rbind(previsoes, temp)
}

write.csv2(previsoes, paste('csv/previsao_diaria/', nome_arquivo_previsoes, sep = ''))
