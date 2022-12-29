# Carregando pacotes --------------------------------------------------------------------------------------
library(tidyverse)
library(dplyr)
library(tidyr)
library(rvest)
library(quantmod)
library(httr)
library(tibble)
library(stringr)
library(reshape2)

paginas <- ''
p <- 1

for (i in 1:33){
  paginas[p] <- paste('https://www.vlr.gg/matches/results/?page=', p, sep = '')
  p = p + 1
}
# Até aqui Okay!

c <- 1

matchs <- 'a'

funcaoPagina <- function(pagina){
  
  matchs <- read_html(pagina) %>% 
    html_nodes('a') %>% html_attr('href')
  
  matchs <- matchs[15:64]
  
  n <- 1
  
  for (i in matchs){
    matchs[n] <- paste('www.vlr.gg', matchs[n], sep = '')
    n = n+1
    
  }
  
  return(matchs)
  
}

f <- 1

a <- list()

for (i in paginas){
  a[[length(a)+1]] = funcaoPagina(paginas[f])
  f = f + 1

}




