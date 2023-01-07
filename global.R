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

# Carregando a rede neural
load(file = "model_nnet.rda")

# Criando o app
shinyApp(ui = ui, server = server)