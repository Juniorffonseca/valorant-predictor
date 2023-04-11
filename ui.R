# Define UI
ui <- fluidPage(theme = bs_theme(version = 5, base_font = font_google('Yanone Kaffeesatz'), font_scale = 1.2,
                                 `enable-shadows` = TRUE, bg = 'rgb(182, 239, 190)', fg = '#000'),
                tags$head(tags$link(rel = "shortcut icon",
                                    href = "https://cdn-icons-png.flaticon.com/512/9621/9621029.png")),
                navbarPage(
                  'Valorant predictor',
                  tabPanel('Por link',
                           sidebarPanel(
                             h5('Encontre o url da partida em: vlr.gg'),
                             textInput('texturl', 'Url da partida:', ''),
                             actionButton('submitbutton', 'Prever', 
                                          class = 'btn btn-primary'),
                             h4(''),
                             verbatimTextOutput('txtout'),
                             h6('Deploy atualizado em: 10/04/2023'),
                             h6('Rede neural atualizada em: 10/04/2023')
                           )
                  ),
                  tabPanel(
                    'Por nicks',
                    sidebarPanel(
                      h1('Opção desativada temporariamente, colocarei de volta em breve'),
                      div(style='display:inline-block;',textInput('t1j1', 'Jogador1 (time1):', ''),
                      textInput('t1j2', 'Jogador2 (time1):', ''),
                      textInput('t1j3', 'Jogador3 (time1):', ''),
                      textInput('t1j4', 'Jogador4 (time1):', ''),
                      textInput('t1j5', 'Jogador5 (time1):', '')),
                      div(style='display:inline-block;',textInput('t2j1', 'Jogador1 (time2):', ''),
                      textInput('t2j2', 'Jogador2 (time2):', ''),
                      textInput('t2j3', 'Jogador3 (time2):', ''),
                      textInput('t2j4', 'Jogador4 (time2):', ''),
                      textInput('t2j5', 'Jogador5 (time2):', '')),
                      h4(''),
                      actionButton('submitbutton2', 'Prever', 
                                   class = 'btn btn-primary'),
                      h4(''),
                      verbatimTextOutput('txtout2')
                    )
                  ),
                  tabPanel(
                    'Metodologia',
                    sidebarPanel(
                      div(style='display:inline-block;',
                          h3('* Treinada com 674 partidas.'),
                          h3('* 78,32% de acurácia na base de testes.'),
                          h3('* Teste F1-Score 0,763.'),
                          h3('* Valor AUC da curva ROC 0,8257 com um Gini de 0.6514.'),
                          h5('* '))
                    )
                  )
                )
)
