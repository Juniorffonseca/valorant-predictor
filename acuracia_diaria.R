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
nome_arquivo_urls <- paste(Sys.Date() - 1, '_urls.csv', sep = '')
nome_arquivo_previsoes <- paste(Sys.Date() - 1, '_previsoes.csv', sep = '')
nome_arquivo_acuracia <- paste(Sys.Date() - 1, '_acuracia.csv', sep = '')

b <- read.csv2(paste('csv/catalogacao_diaria/', nome_arquivo_urls, sep = '')) %>% select(-X) %>% unlist()
previsoes <- read.csv2(paste('csv/previsao_diaria/', nome_arquivo_previsoes, sep = '')) %>% select(-X)

ganhador <- '' %>% .[0]

for (i in b){
  tryCatch({
  ganhador[length(ganhador)+1] <- get_Ganhadores(i)
  }, error = function(e) {
    # caso ocorra um erro, imprimir mensagem de erro e continuar o loop
    cat("Erro ao checar a URL", url, ":", conditionMessage(e), "\n")
    return(NULL)
  })
}

df <- cbind(b, previsoes)

df$ganhador <- ganhador

df <- df[!df$ganhador %in% "empate", ]

df$V1_n <- as.numeric(ifelse(is.na(str_extract(df$V1, "\\d{1,2}[.,]\\d{1,2}")), 
                             str_extract(df$V1, "\\d{1,2}"), 
                             str_extract(df$V1, "\\d{1,2}[.,]\\d{1,2}")))
df$V2_n <- as.numeric(ifelse(is.na(str_extract(df$V2, "\\d{1,2}[.,]\\d{1,2}")), 
                             str_extract(df$V2, "\\d{1,2}"), 
                             str_extract(df$V2, "\\d{1,2}[.,]\\d{1,2}")))

df$prev <- ifelse(as.numeric(df$V1_n)>as.numeric(df$V2_n), 1, 0)

row.names(df) <- NULL

acertos <- sum(df$ganhador == df$prev)

erros <- sum(df$ganhador != df$prev)

acuracia <- acertos/nrow(df)

acuracia_diaria <- as.data.frame(cbind(acertos, erros, acuracia))

write.csv2(df, paste('csv/previsao_diaria/', nome_arquivo_previsoes, sep = ''))

write.csv2(acuracia_diaria, paste('csv/previsao_diaria/', nome_arquivo_acuracia, sep = ''))
