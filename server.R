# Define server function  
server <- function(input, output) {
  
  previsaoInput <- reactive({
    
    partida <- medias_Times(input$texturl)
    
    jogos_scale <- read.csv2('csv/partidas_5.csv') %>% select(-X, -ganhador)
    
    jogos_scale <- rbind(jogos_scale, partida)
    
    jogos_scale <- scale(jogos_scale)
    
    partida <- jogos_scale[nrow(jogos_scale),]
    
    partida <- t(partida)
    
    partida <- as.data.frame(partida)
    
    colnames(partida) <- c('time1R', 'time2R', 'time1ACS', 'time2ACS', 'time1KAST', 'time2KAST', 'time1KD', 'time2KD',
                           'time1ADR', 'time2ADR')
    
    previsao <- compute(n, partida)
    
    previsao <- previsao$net.result[1]
    
    partida_reversa <- partida
    
    partida_reversa$time1R <- partida$time2R
    partida_reversa$time2R <- partida$time1R
    partida_reversa$time1ACS <- partida$time2ACS
    partida_reversa$time2ACS <- partida$time1ACS
    partida_reversa$time1KAST <- partida$time2KAST
    partida_reversa$time2KAST <- partida$time1KAST
    partida_reversa$time1KD <- partida$time2KD
    partida_reversa$time2KD <- partida$time1KD
    partida_reversa$time1ADR <- partida$time2ADR
    partida_reversa$time2ADR <- partida$time1ADR
    
    previsao2 <- compute(n, partida_reversa)
    
    previsao2 <- previsao2$net.result[1]
    
    a <- previsao
    b <- previsao2
    
    transforma_positivo <- function (x){
      y = atan(x) + pi/2
      return (y)
    }
    
    transforma_probabilidade <- function (y, x){
      z = y / (y + x)
      w = x / (x + y)
      c = as.matrix(c(z,w))
      return(c)
    }
    
    a <- transforma_positivo(a)
    b <- transforma_positivo(b)
    previsao <- transforma_probabilidade(a,b)
    
    previsao <- previsao * 100
    
    return(previsao)
  })

  previsaoInput2 <- reactive({
    
    partida <- medias_Times(input$texturl)
      
    jogos_scale <- read.csv2('csv/partidas_5.csv') %>% select(-X, -ganhador)
      
      jogos_scale <- rbind(jogos_scale, partida)
      
      jogos_scale <- scale(jogos_scale)
      
      partida <- jogos_scale[nrow(jogos_scale),]
      
      partida <- t(partida)
      
      partida <- as.data.frame(partida)
      
      colnames(partida) <- c('time1R', 'time2R', 'time1ACS', 'time2ACS', 'time1KAST', 'time2KAST', 'time1KD', 'time2KD',
                             'time1ADR', 'time2ADR')
      
      previsao <- compute(n, partida)
      
      previsao <- previsao$net.result[1]
      
      partida_reversa <- partida
      
      partida_reversa$time1R <- partida$time2R
      partida_reversa$time2R <- partida$time1R
      partida_reversa$time1ACS <- partida$time2ACS
      partida_reversa$time2ACS <- partida$time1ACS
      partida_reversa$time1KAST <- partida$time2KAST
      partida_reversa$time2KAST <- partida$time1KAST
      partida_reversa$time1KD <- partida$time2KD
      partida_reversa$time2KD <- partida$time1KD
      partida_reversa$time1ADR <- partida$time2ADR
      partida_reversa$time2ADR <- partida$time1ADR
      
      previsao2 <- compute(n, partida_reversa)
      
      previsao2 <- previsao2$net.result[1]
      
      a <- previsao
      b <- previsao2
      
      transforma_positivo <- function (x){
        y = atan(x*10) + pi/2
        return (y)
      }
      
      transforma_probabilidade <- function (y, x){
        z = y / (y + x)
        w = x / (x + y)
        c = as.matrix(c(z,w))
        return(c)
      }
      
      a <- transforma_positivo(a)
      b <- transforma_positivo(b)
      previsao <- transforma_probabilidade(a,b)
      
      previsao <- previsao * 100
      
      return(previsao)
  })
  
  output$txtout <- renderText({
    if (input$submitbutton>0) { 
      if (is.numeric(previsaoInput())){
        paste(round(previsaoInput(), 2), '%', '')}
      else {
        paste(previsaoInput())}} 
    else {
      return("A rede neural está pronta para calcular as probabilidades.")
    }
  })
  
  output$txtout2 <- renderText({
    if (input$submitbutton2>0) { 
      if (is.numeric(previsaoInput2())){
        paste(round(previsaoInput2(), 2), '%', '')}
      else {
        paste(previsaoInput2())}} 
    else {
      return("A rede neural está pronta para calcular as probabilidades.")
    }
  })
  
}




