# Define UI
ui <- fluidPage(theme = shinytheme('superhero'),
                navbarPage(
                  'Valorant',
                  tabPanel('Por link',
                           sidebarPanel(
                             textInput('texturl', 'Url da partida:', ''),
                             actionButton('submitbutton', 'Prever', 
                                          class = 'btn btn-primary'),
                             h4('Probabilidade de vitória de cada time, respectivamente:'),
                             verbatimTextOutput('txtout'),
                             h6('Deploy atualizado em: 13/02/2023'),
                             h6('Rede neural atualizada em: 12/02/2023')
                           )
                  ),
                  tabPanel(
                    'Por nicks',
                    sidebarPanel(
                      h1('Opção desativada temporariamente, colocarei de volta em breve'),
                      textInput('t1j1', 'Jogador1 (time1):', ''),
                      textInput('t1j2', 'Jogador2 (time1):', ''),
                      textInput('t1j3', 'Jogador3 (time1):', ''),
                      textInput('t1j4', 'Jogador4 (time1):', ''),
                      textInput('t1j5', 'Jogador5 (time1):', ''),
                      textInput('t2j1', 'Jogador1 (time2):', ''),
                      textInput('t2j2', 'Jogador2 (time2):', ''),
                      textInput('t2j3', 'Jogador3 (time2):', ''),
                      textInput('t2j4', 'Jogador4 (time2):', ''),
                      textInput('t2j5', 'Jogador5 (time2):', ''),
                      actionButton('submitbutton2', 'Prever', 
                                   class = 'btn btn-primary'),
                      h4('Probabilidade de vitória de cada time, respectivamente:'),
                      verbatimTextOutput('txtout2')
                    )
                  )
                )
)
