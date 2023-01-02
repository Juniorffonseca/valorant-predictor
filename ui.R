# Define UI
ui <- fluidPage(theme = shinytheme("cyborg"),
                navbarPage(
                  "Valorant Prediction",
                  tabPanel("Prediction",
                           sidebarPanel(
                             tags$h3("Analisar partidas"),
                             textInput("texturl", "url da partida:", ""),
                             actionButton('submitbutton', 'Prever', 
                                          class = 'btn btn-primary')
                             
                             
                           ),
                           mainPanel(
                             h1("Resultado"),
                             
                             h4("Probabilidade de vitória de cada Time"),
                             verbatimTextOutput("txtout"),
                             
                           )
                           
                  ),
                  tabPanel("Navbar 2", "Em breve mais funções"),
                  tabPanel("Navbar 3", "Em breve mais funções")
                  
                )
)
