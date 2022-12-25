# Pacotes
library(shiny)
library(shinythemes)

source("./04_previsão.R")

# Define UI
ui <- fluidPage(theme = shinytheme("cerulean"),
                navbarPage(
                  # theme = "cerulean",  # <--- To use a theme, uncomment this
                  "Valorant Prediction",
                  tabPanel("Prediction",
                           sidebarPanel(
                             tags$h3("Analisar partidas"),
                             textInput("url", "Url da partida:", ""),
                             h4("Caso prefira digitar o nome dos jogadores de cada time:"),
                             textInput("txt1", "Jogador1 time1:", ""),
                             textInput("txt2", "Jogador2 time1:", ""),
                             textInput("txt3", "Jogador3 time1:", ""),
                             textInput("txt4", "Jogador4 time1:", ""),
                             textInput("txt5", "Jogador5 time1:", ""),
                             textInput("txt6", "Jogador1 time2", ""),
                             textInput("txt7", "Jogador2 time2", ""),
                             textInput("txt8", "Jogador3 time2", ""),
                             textInput("txt9", "Jogador4 time2:", ""),
                             textInput("txt10", "Jogador5 time2:", ""),
                             
                             
                           ), # sidebarPanel
                           mainPanel(
                             h1("Resultado"),
                             
                             h4("Probabilidade de vitória do Time1"),
                             verbatimTextOutput("txtout"),
                             h4("Probabilidade de vitória do Time2"),
                             verbatimTextOutput("txtout"),
                             
                           ) # mainPanel
                           
                  ), # Navbar 1, tabPanel
                  tabPanel("Navbar 2", "Em breve mais funções"),
                  tabPanel("Navbar 3", "Em breve mais funções")
                  
                ) # navbarPage
) # fluidPage

# Define server function  
server <- function(input, output) {

  previsao <- reactive(preverResultado(input$url))
  
  output$txtout <- renderText(
    previsao())
} # server


# Create Shiny object
shinyApp(ui = ui, server = server)
