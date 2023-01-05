# Define UI
ui <- fluidPage(theme = shinytheme("cyborg"),
                navbarPage(
                  "Valorant",
                  tabPanel("Prever partidas",
                           sidebarPanel(
                             textInput("texturl", "Url da partida:", ""),
                             actionButton('submitbutton', 'Prever', 
                                          class = 'btn btn-primary'),
                             
                             
                           ),
                           mainPanel(
                             h5("Probabilidade de vitória de cada time:"),
                             verbatimTextOutput("txtout"),
                             
                           )
                  )
                )
)
