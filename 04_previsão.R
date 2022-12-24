# Carregando pacotes --------------------------------------------------------------------------------------
library(tidyverse)
library(dplyr)
library(tidyr)
library(rvest)
library(quantmod)
library(httr)
library(tibble)
library(stringr)
library(neuralnet)
library(reshape2)

# Carregando o modelo
load(file = "model_nnet.rda")

# Carregando o dataframe jogadores
dados_gerais <- read.csv2('csv/jogadores.csv')

# Arrumando as colunas 
dados_gerais <- dplyr::select(dados_gerais, Player, R, ACS, K.D, KAST, ADR)
row.names(dados_gerais) <- make.names(dados_gerais[,1], unique = T)
dados_gerais <- dplyr::select(dados_gerais, -Player)
dados_gerais$KAST <- parse_number(dados_gerais$KAST)

# Time A
timeA = c('WerlasS', 'ALL3Y', 'reazy', 'fred', 'CyderX')
timeA <- paste0('\\b', timeA, '\\b') 
dados_gerais$timeA <- ifelse(grepl(paste(timeA, collapse = '|'), rownames(dados_gerais), useBytes = T), 1, 0)
dados_gerais['nobody.1',]$timeA <- 0

# Time B
timeB = c('DeepMans', 'Masic', 'XiSTOU', 'skylen', 'cacan')
timeB <- paste0('\\b', timeB, '\\b') 
dados_gerais$timeB <- ifelse(grepl(paste(timeB, collapse = '|'), rownames(dados_gerais), useBytes = T), 1, 0)

timeA_df <- filter(dados_gerais, dados_gerais$timeA == 1)
timeA_df <- dplyr::select(timeA_df, R, ACS, K.D, KAST, ADR)
timeB_df <- filter(dados_gerais, dados_gerais$timeB == 1) 
timeB_df <- dplyr::select(timeB_df, R, ACS, K.D, KAST, ADR)

# Médias
timeA_R <- mean(timeA_df$R)
timeA_ACS <- mean(timeA_df$ACS)
timeA_KAST <- mean(timeA_df$KAST)
timeA_KD <- mean(timeA_df$K.D)
timeA_ADR <- mean(timeA_df$ADR)
timeB_R <- mean(timeB_df$R)
timeB_ACS <- mean(timeB_df$ACS)
timeB_KAST <- mean(timeB_df$KAST)
timeB_KD <- mean(timeB_df$K.D)
timeB_ADR <- mean(timeB_df$ADR)

partida <- c(timeA_R, timeB_R, timeA_ACS, timeB_ACS, timeA_KAST, timeB_KAST, timeA_KD, timeB_KD,
                           timeA_ADR, timeB_ADR)

partida <- scale(partida)

partida <- t(partida)

partida <- as.data.frame(partida)

partida <- dcast(partida, timeA_R ~ timeB_ADR, fill = 0)

colnames(partida) <- c('timeA_R', 'timeB_R', 'timeA_ACS', 'timeB_ACS', 'timeA_KAST', 'timeB_KAST', 'timeA_KD', 'timeB_KD',
                       'timeA_ADR', 'timeB_ADR')

previsao <- compute(n, partida)

previsao$net.result

