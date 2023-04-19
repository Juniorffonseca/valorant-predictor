# Instalando pacotes (se necessário) e carregando ----------------------------------------------------------
library(devtools)
#install_github('Juniorffonseca/r-pacote-valorant')
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
library(valorant)
setwd('C:/Users/anonb/Documents/TCC_Pós/Scripts')

# Carregando partidas diarias e unindo em um df ------------------------------------------------------------
datas <- seq(as.Date('2023-04-13'), Sys.Date() - 1, by = 'day')
nomes_arquivos <- paste0('csv/previsao_diaria/', format(datas, '%Y-%m-%d'), '_acuracia.csv')

acuracia <- list()

for (arquivo in nomes_arquivos) {
  acuracia[[arquivo]] <- possibly(read.csv2, otherwise = NULL)(arquivo)
}

acuracia <- bind_rows(acuracia) %>% select(-X)

acertos <- sum(acuracia$acertos)
erros <- sum(acuracia$erros)
print(acuracia_total <- acertos/(acertos+erros))
