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


# Criando variável páginas e criando variável 'p' que será a parte final do url (o número da página) -------
paginas <- 'https://www.vlr.gg/matches/results'
p <- 1

links <- read_html(paginas) %>% 
  html_nodes('a') %>% html_attr('href')

last_page <- str_extract(links[68], '\\d+')

paginas <- ''

p <- 1

for (i in 1:){
  paginas[p] <- paste('https://www.vlr.gg/matches/results/?page=', p, sep = '')
  p = p + 1
}
