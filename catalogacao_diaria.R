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

funcaoPagina <- function(pagina){
  
  matchs <- read_html(pagina) %>% 
    html_nodes('a') %>% html_attr('href')
  
  matchs <- matchs[15:64]
  
  n <- 1
  
  for (i in matchs){
    matchs[n] <- paste('https://www.vlr.gg', matchs[n], sep = '')
    n = n + 1
    
  }
  
  return(matchs)
  
}

a <- funcaoPagina('https://www.vlr.gg/matches')
n <- 1
b <- '' %>% .[0]

for (i in a){
  tryCatch({
    dia <- read_html(i) %>% 
      html_nodes('div.moment-tz-convert') %>% html_text(trim = T) %>% .[1] %>%
      parse_date_time(., orders = "%A, %B %d", locale = "en_US")
    tbd <- read_html(i) %>% html_nodes('table') %>% html_table() %>% 
      .[1:2] %>% map_df(as_tibble, .name_repair = 'minimal')

    if(dia == Sys.Date() & sum(tbd[1] == 'TBD') < 1){
      b[length(b)+1] <- i
    }
    else{}
  }
  , error = function(e){cat('error:', conditionMessage(e), '\n')})
}

nome_arquivo_urls <- paste(Sys.Date(), '_urls.csv', sep = '')
write.csv2(b, paste('csv/catalogacao_diaria/', nome_arquivo_urls, sep = ''))

m <- 1
dff <- list()

for (i in b){
  tryCatch({
      dff[[length(dff)+1]] <- medias_Times(b[m])
      m = m + 1
  }
  
  , error = function(e){cat('error:', conditionMessage(e), '\n')})
}

dff <- dff %>% map_df(as_tibble)

nome_arquivo_partidas <- paste(Sys.Date(), '_partidas.csv', sep = '')
write.csv2(dff, paste('csv/catalogacao_diaria/', nome_arquivo_partidas, sep = ''))

