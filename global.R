# Carregando pacotes
library(shiny)
library(shinythemes)
library(dplyr)
library(tidyr)
library(rvest)
library(quantmod)
library(httr)
library(tibble)
library(stringr)
library(neuralnet)
library(reshape2)
library(data.table)
library(readr)
library(ggplot2)
library(DT)

# Carregando ui e server
source('ui.R')
source('server.R')

# Carregando a rede neural e o dataframe Jogadores
load(file = "model_nnet.rda")

# Coletando os dados dos jogadores de forma atualizada
link <- "https://www.vlr.gg/stats/?event_group_id=all&event_id=all&region=all&country=all&min_rounds=50&min_rating=1550&agent=all&map_id=all&timespan=all"

players <- read_html(link) %>% 
  html_node("table") %>% 
  html_table() %>% 
  separate(Player, into = c("Player", "Team"), sep = "\\s+", extra = "merge") %>% 
  select('Player', 'Team', 'R', 'ACS', 'K:D', 'KAST', 'ADR') %>% 
  as.data.frame()

# Criando o app
shinyApp(ui = ui, server = server)