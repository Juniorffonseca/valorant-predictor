# Define server function  
server <- function(input, output) {
  
  previsaoInput <- reactive({
    
    previsao_f <- prever(input$texturl)
    
    return(previsao_f)
    
  })
  
  previsaoInput2 <- reactive({
    
    #previsao_f <- prever(input$texturl)
    
    return(previsao_f)
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




