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

c <- 1

matchs <- 'a'

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

f <- 1

a <- list()

for (i in paginas){
  a[[length(a)+1]] = funcaoPagina(paginas[f])
  f = f + 1

}

rm (c, f, i, p, matchs, paginas)

m <- 1

dff <- list()

for (i in a){
  tryCatch({
  dff[[length(dff)+1]] <- catalogarporUrl(a[m])
  m = m + 1
  }, error = function(e){cat('error:', conditionMessage(e), '\n')})
}

teste <- unlist(salvando)

salvando <- dff
save(salvando, file = 'salvando.rds')

c <- 1

dfc <- as.data.frame(salvando)
# Preciso organizar em colunas direitinho 
# Está ocorrendo algum problema de ler algum comando como "connection" e tá dando erros por causa disso. Preciso descobrir onde e resolver.


