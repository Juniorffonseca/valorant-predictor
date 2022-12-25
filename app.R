# Pacotes
library(shiny)
library(shinythemes)

source('04_previsão.R')

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

# Define server function  
server <- function(input, output) {

  previsao <- reactive(preverResultado(input$url))
  
  output$txtout <- renderText(previsao())
} # server


# Create Shiny object
shinyApp(ui = ui, server = server)
