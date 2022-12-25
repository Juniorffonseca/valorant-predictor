# Carregamento de pacotes, dataframes, rede neural e sources
library(shiny)
library(shinythemes)
library(tidyverse)
library(dplyr)
library(tidyr)
library(rvest)
library(quantmod)
library(httr)
library(tibble)
library(stringr)
library(neuralnet)
library(reshape2)

source('04_previsão.R')

load(file = "model_nnet.rda")

dados_gerais <- read.csv2('csv/jogadores.csv')


# Define UI
ui <- fluidPage(theme = shinytheme("cerulean"),
                navbarPage(
                  # theme = "cerulean",  # <--- To use a theme, uncomment this
                  "Valorant Prediction",
                  tabPanel("Prediction",
                           sidebarPanel(
                             tags$h3("Analisar partidas"),
                             textInput("url", "Url da partida:", ""),
                             
                             
                           ), # sidebarPanel
                           mainPanel(
                             h1("Resultado"),
                             
                             h4("Probabilidade de vitória do Time1"),
                             verbatimTextOutput("txtout"),
                             
                           ) # mainPanel
                           
                  ), # Navbar 1, tabPanel
                  tabPanel("Navbar 2", "Em breve mais funções"),
                  tabPanel("Navbar 3", "Em breve mais funções")
                  
                ) # navbarPage
) # fluidPage

# Arrumando as colunas -------------------------------------------------------------------------------------
dados_gerais <- dplyr::select(dados_gerais, Player, R, ACS, K.D, KAST, ADR)
row.names(dados_gerais) <- make.names(dados_gerais[,1], unique = T)
dados_gerais <- dplyr::select(dados_gerais, -Player)
dados_gerais$KAST <- parse_number(dados_gerais$KAST)

# Link da partida ------------------------------------------------------------------------------------------
url <- as.character(reactive(input$url)) #cannot coerce type 'closure' to vector of type 'character' pesquisar

# Pegando os dados no link da partida ----------------------------------------------------------------------
info <- read_html(url) %>% 
  html_nodes("table") %>% 
  html_table()

timeA <- info[[1]]
timeB <- info[[2]]

timeA <- lapply(timeA, str_replace_all, '\n', '') %>% 
  lapply(str_replace_all, '\t', '')
timeB <- lapply(timeB, str_replace_all, '\n', '') %>% 
  lapply(str_replace_all, '\t', '')

timeA <- as.data.frame(timeA[1])
timeB <- as.data.frame(timeB[1])

colnames(timeA) <- '1'
colnames(timeB) <- '1'

timeA <- separate(timeA, '1', into = c("Player", "Team"), sep = "\\s+", extra = "merge")
timeB <- separate(timeB, '1', into = c("Player", "Team"), sep ="\\s+", extra = "merge")

timeA <- timeA$Player
timeB <- timeB$Player

rm(info, partida, url)

# Time A
#timeA = c('nome1', 'nome2', 'nome3', 'nome4', 'nome5') # se preferir passar de forma manual
timeA <- paste0('\\b', timeA, '\\b') 
dados_gerais$timeA <- ifelse(grepl(paste(timeA, collapse = '|'), rownames(dados_gerais), useBytes = T), 1, 0)
dados_gerais['nobody.1',]$timeA <- 0
dados_gerais['Laz.1',]$timeA <- 0 

# Time B
#timeB = c('nome1', 'nome2', 'nome3', 'nome4', 'nome5') # se preferir passar de forma manual
timeB <- paste0('\\b', timeB, '\\b') 
dados_gerais$timeB <- ifelse(grepl(paste(timeB, collapse = '|'), rownames(dados_gerais), useBytes = T), 1, 0)
dados_gerais['Shiro.1',]$timeB<- 0

timeA_df <- filter(dados_gerais, dados_gerais$timeA == 1)
timeA_df <- dplyr::select(timeA_df, R, ACS, K.D, KAST, ADR)
timeB_df <- filter(dados_gerais, dados_gerais$timeB == 1) 
timeB_df <- dplyr::select(timeB_df, R, ACS, K.D, KAST, ADR)

# Médias
timeA_R <- mean(timeA_df$R)
timeA_ACS <- mean(timeA_df$ACS)
timeA_KAST <- mean(timeA_df$KAST)
timeA_KD <- mean(timeA_df$K.D)
timeA_ADR <- mean(timeA_df$ADR)
timeB_R <- mean(timeB_df$R)
timeB_ACS <- mean(timeB_df$ACS)
timeB_KAST <- mean(timeB_df$KAST)
timeB_KD <- mean(timeB_df$K.D)
timeB_ADR <- mean(timeB_df$ADR)

partida <- c(timeA_R, timeB_R, timeA_ACS, timeB_ACS, timeA_KAST, timeB_KAST, timeA_KD, timeB_KD,
             timeA_ADR, timeB_ADR)

partida <- scale(partida)

partida <- t(partida)

partida <- as.data.frame(partida)

colnames(partida) <- c('timeA_R', 'timeB_R', 'timeA_ACS', 'timeB_ACS', 'timeA_KAST', 'timeB_KAST', 'timeA_KD', 'timeB_KD',
                       'timeA_ADR', 'timeB_ADR')

previsao <- compute(n, partida)

previsao$net.result



# Define server function  
server <- function(input, output) {

  previsao <- reactive(preverResultado(input$url))
  
  output$txtout <- renderText(previsao())
} # server


# Create Shiny object
shinyApp(ui = ui, server = server)
