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

url <- 'https://www.vlr.gg/matches/results'

matchs <- read_html(url) %>% 
  html_nodes('a') %>% html_attr('href')

url2 <- 'https://www.vlr.gg/matches/results/?page=2'

matchs2 <- read_html(url2) %>% 
  html_nodes('a') %>% html_attr('href')

matchs <- matchs[15:64]
matchs2 <- matchs2[15:64]


