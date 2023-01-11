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

dados_gerais <- read.csv2("csv/jogadores.csv")

# Criando variável páginas e criando variável 'p' que será a parte final do url (o número da página) -------
paginas <- ''
p <- 1

for (i in 34:44){
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

catalogarporUrl <- function (string){
  tryCatch(
    
    {
      dados_gerais <- dplyr::select(dados_gerais, Player, R, ACS, K.D, KAST, ADR)
      row.names(dados_gerais) <- make.names(dados_gerais[,1], unique = T)
      dados_gerais <- dplyr::select(dados_gerais, -Player)
      dados_gerais$KAST <- parse_number(dados_gerais$KAST)
      
      info <- read_html(string) %>% 
        html_nodes("table") %>% 
        html_table()
      
      placar <- read_html(string) %>% 
        html_nodes("div.js-spoiler") %>% html_text(trim=T)
      
      placar <- str_replace_all(placar, '\t', '') %>% str_replace_all('\n', '')
      
      placar <- as.data.frame(placar[1])
      
      placar <- separate(placar, 'placar[1]', into = c('Time1', 'Time2'), sep = ':', extra = 'merge')
      
      ifelse(placar$Time1 > placar$Time2, ganhador <- 1, ganhador <- 0)
      
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
      
      timeA <- paste0('\\b', timeA, '\\b') 
      dados_gerais$timeA <- ifelse(grepl(paste(timeA, collapse = '|'), rownames(dados_gerais), useBytes = T), 1, 0)
      
      timeB <- paste0('\\b', timeB, '\\b') 
      dados_gerais$timeB <- ifelse(grepl(paste(timeB, collapse = '|'), rownames(dados_gerais), useBytes = T), 1, 0)
      
      timeA_df <- filter(dados_gerais, dados_gerais$timeA == 1)
      timeA_df <- dplyr::select(timeA_df, R, ACS, K.D, KAST, ADR)
      timeB_df <- filter(dados_gerais, dados_gerais$timeB == 1) 
      timeB_df <- dplyr::select(timeB_df, R, ACS, K.D, KAST, ADR)
      
      if(nrow(timeA_df) == 5 && nrow(timeB_df) == 5){
        
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
        
        partida <- t(partida)
        
        partida <- as.data.frame(partida) %>% cbind(ganhador)
        
        colnames(partida) <- c('time1R', 'time2R', 'time1ACS', 'time2ACS', 'time1KAST', 'time2KAST', 'time1KD', 'time2KD',
                               'time1ADR', 'time2ADR', 'ganhador')
        
        return(partida)
      }
    }
    , error = function(e){cat('error:', conditionMessage(e), '\n')})
  
}


rm (c, f, i, p, matchs, paginas)

m <- 1

dff <- list()

for (i in a){
  tryCatch({
    dff[[length(dff)+1]] <- catalogarporUrl(a[m])
    m = m + 1
  }, error = function(e){cat('error:', conditionMessage(e), '\n')})
}

dff <- dff %>% map_df(as_tibble)

write.csv2(dff, 'csv/df2.csv')

load(file = "model_nnet.rda")

# Testando a acurácia

dados_gerais <- read.csv2('csv/jogadores.csv')
  
  # Arrumando as colunas -------------------------------------------------------------------------------------
dados_gerais <- dplyr::select(dados_gerais, Player, R, ACS, K.D, KAST, ADR)
row.names(dados_gerais) <- make.names(dados_gerais[,1], unique = T)
dados_gerais <- dplyr::select(dados_gerais, -Player)
dados_gerais$KAST <- parse_number(dados_gerais$KAST)
  
  # Pegando os dados no link da partida ----------------------------------------------------------------------
#jogos <- read.csv2('csv/df2.csv') %>% dplyr::select(-X, -ganhador)
    
jogos_scale <- read.csv2('csv/df.csv') %>% dplyr::select(-X, -ganhador)
    
dff_scale <- dplyr::select(dff, -ganhador)

jogos_scale <- rbind(jogos_scale, dff_scale)

jogos_scale <- scale(jogos_scale)
    
partidas <- jogos_scale[-1:-813,]
    
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
    
previsoes <- ifelse (previsao[,1] > previsao2[,1], 1, 0)
    
resultados <- dplyr::select(dff, ganhador)
    
resultadovspredict <- cbind(partidas, previsoes, resultados)
    
i <- sum(resultadovspredict$ganhador == resultadovspredict$previsoes)/nrow(resultadovspredict)

# i = 0.747292418772563
# Acurácia de 75%

