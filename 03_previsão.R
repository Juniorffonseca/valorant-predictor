# Carregando pacotes --------------------------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(rvest)
library(quantmod)
library(httr)
library(tibble)
library(stringr)
library(neuralnet)
library(reshape2)

load(file = "rede_neural.rda")

preverResultado <- function(link){
  
  # Pegando os dados no link da partida ----------------------------------------------------------------------
  partida <- medias_Times(link)
  
  jogos_scale <- read.csv2('csv/partidas.csv') %>% select(-X, -ganhador)
  
  jogos_scale <- rbind(jogos_scale, partida)
  
  jogos_scale <- scale(jogos_scale)
  
  partida <- jogos_scale[nrow(jogos_scale),]
  
  partida <- t(partida)
  
  partida <- as.data.frame(partida)
  
  colnames(partida) <- c('time1R', 'time2R', 'time1ACS', 'time2ACS', 'time1KAST', 'time2KAST', 'time1KD', 'time2KD',
                         'time1ADR', 'time2ADR')
  
  previsao <- compute(n, partida)
  
  previsao <- previsao$net.result[1]
  
  partida_reversa <- partida
  
  partida_reversa$time1R <- partida$time2R
  partida_reversa$time2R <- partida$time1R
  partida_reversa$time1ACS <- partida$time2ACS
  partida_reversa$time2ACS <- partida$time1ACS
  partida_reversa$time1KAST <- partida$time2KAST
  partida_reversa$time2KAST <- partida$time1KAST
  partida_reversa$time1KD <- partida$time2KD
  partida_reversa$time2KD <- partida$time1KD
  partida_reversa$time1ADR <- partida$time2ADR
  partida_reversa$time2ADR <- partida$time1ADR
  
  previsao2 <- compute(n, partida_reversa)
  
  previsao2 <- previsao2$net.result[1]
  
  a <- previsao
  b <- previsao2
  
  transforma_positivo <- function (x){
    y = atan(x) + pi/2
    return (y)
  }
  
  transforma_probabilidade <- function (y, x){
    z = y / (y + x)
    w = x / (x + y)
    c = as.matrix(c(z,w))
    return(c)
  }
  
  a <- transforma_positivo(a)
  b <- transforma_positivo(b)
  previsao <- transforma_probabilidade(a,b)
  
  previsao <- previsao * 100
  
  return(previsao)
  
}

return <- preverResultado(
  'https://www.vlr.gg/130685/loud-vs-optic-gaming-valorant-champions-2022-gf'
)

