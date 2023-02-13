# Carregando pacotes --------------------------------------------------------------------------------------
library(rvest)
library(quantmod)
library(httr)
library(tibble)
library(stringr)
library(reshape2)
library(tidyverse)
library(neuralnet)
library(readr)
library(purrr)

# Criando variável páginas e criando variável 'p' que será a parte final do url (o número da página) -------
paginas <- ''
p <- 1

for (i in 1:9){
  paginas[p] <- paste('https://www.vlr.gg/matches/results/?page=', p, sep = '')
  p = p + 1
}

c <- 1
matchs <- 'a'

funcaoPagina <- function(pagina){
  
  matchs <- read_html(pagina) %>% 
    html_nodes('a') %>% html_attr('href')
  
  matchs <- matchs[15:64]
  
  n <- 1
  
  for (i in matchs){
    matchs[n] <- paste('https://www.vlr.gg', matchs[n], sep = '')
    n = n + 1
    
  }
  
  return(matchs)
  
}

f <- 1
a <- list()

for (i in paginas){
  a[[length(a)+1]] = funcaoPagina(paginas[f])
  f = f + 1
  
}

a <- unlist(a)
m <- 1
dff <- list()

for (i in a){
  tryCatch({
    dff[[length(dff)+1]] <- medias_Times(a[m], resultado = T)
    m = m + 1
  }, error = function(e){cat('error:', conditionMessage(e), '\n')})
}

dff <- dff %>% map_df(as_tibble)

write.csv2(dff, 'csv/outras_partidas_2.csv') #apenas de janeiro 2023 em diante

load(file = "rede_neural.rda")

# Testando a acurácia -------------------------------------------------------------------------------------

jogos <- read.csv2('csv/partidas.csv') %>% dplyr::select(-X, -ganhador)

outras_partidas <- read.csv2('csv/partidas_2.csv') %>% dplyr::select(-X, -ganhador)

jogos_scale <- rbind(jogos, outras_partidas)

jogos_scale <- scale(jogos_scale)

partidas <- jogos_scale[-1:-nrow(jogos),]

partidas <- as.data.frame(partidas)

previsao <- compute(n, partidas)

previsao <- previsao$net.result

partidas_reversas <- partidas

partidas_reversas$time1R <- partidas$time2R
partidas_reversas$time2R <- partidas$time1R
partidas_reversas$time1ACS <- partidas$time2ACS
partidas_reversas$time2ACS <- partidas$time1ACS
partidas_reversas$time1KAST <- partidas$time2KAST
partidas_reversas$time2KAST <- partidas$time1KAST
partidas_reversas$time1KD <- partidas$time2KD
partidas_reversas$time2KD <- partidas$time1KD
partidas_reversas$time1ADR <- partidas$time2ADR
partidas_reversas$time2ADR <- partidas$time1ADR

previsao2 <- compute(n, partidas_reversas)

previsao2 <- previsao2$net.result

previsoes <- cbind(previsao, previsao2)

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

a <- transforma_positivo(previsao)
b <- transforma_positivo(previsao2)
previsao <- transforma_probabilidade(a,b)
previsao <- previsao * 100
previsao2 <- previsao[((length(previsao)/2)+1):length(previsao)]
previsao <- previsao[1:(length(previsao)/2)]
previsao <- cbind(previsao, previsao2)

ganhadores <- read.csv2('csv/partidas_2.csv') %>% dplyr::select(ganhador)

previsao <- cbind(previsao, ganhadores)
colnames(previsao) <- c('previsao1', 'previsao2', 'ganhador')

previsao <- previsao %>% 
  mutate(ganhador = as.factor(ganhador))

# Plot
ggplot(data = previsao, mapping = aes(x = previsao1, y = previsao2, colour = ganhador)) +
  geom_tile(aes(fill = ganhador)) +
  geom_point() +
  theme_bw()


resultados <- dplyr::select(previsao, ganhador)

resultadovspredict <- cbind(partidas, previsao)

resultadovspredict$previsoes <-  ifelse(resultadovspredict$previsao1>resultadovspredict$previsao2,
                                        1,
                                        0)

i <- sum(resultadovspredict$ganhador == resultadovspredict$previsoes)/nrow(resultadovspredict)

# i = 0.747292418772563
# Acurácia de 75%

