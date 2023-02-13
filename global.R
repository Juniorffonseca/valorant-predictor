# Carregando pacotes
library(devtools)
#install_github("Juniorffonseca/r-pacote-valorant")
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
library(valorant)
library(purrr)

# Carregando ui e server
source('ui.R')
source('server.R')

# Carregando a rede neural e o dataframe Jogadores
load(file = "model_4_nnet.rda")

# Criando o app
shinyApp(ui = ui, server = server)