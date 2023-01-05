# Carregamento de pacotes, dataframes, rede neural e sources
# Pacotes
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

#Sources
source('ui.R')
source('server.R')

#Rede neural
load(file = "model_nnet.rda")

#Criando o app
shinyApp(ui = ui, server = server)