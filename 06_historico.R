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
paginas <- 'https://www.vlr.gg/matches/results'
p <- 1

links <- read_html(paginas) %>% 
  html_nodes('a') %>% html_attr('href')

last_page <- str_extract(links[68], '\\d+')

paginas <- ''

p <- 1

for (i in 1:last_page){
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
  a[length(a)+1] = funcaoPagina(paginas[f])
  f = f + 1
  
}

a <- unlist(a)

write.csv2(a, 'csv/a.csv')

a <- read.csv2('csv/a.csv') %>% dplyr::select(-X)

catalogarporUrl <- function (string){
  tryCatch(
    
    {
      info <- read_html(string) %>%
        html_nodes("table") %>%
        html_table()
      
      placar <- read_html(string) %>%
        html_nodes("div.js-spoiler") %>% html_text(trim=T)
      
      placar <- str_replace_all(placar, '\t', '') %>% str_replace_all('\n', '')
      
      placar <- as.data.frame(placar[1])
      
      placar <- separate(placar, 'placar[1]', into = c('Time1', 'Time2'), sep = ':', extra = 'merge')
      
      ifelse(placar$Time1 > placar$Time2, ganhador <- c(1,1,1,1,1,0,0,0,0,0), ganhador <- c(0,0,0,0,0,1,1,1,1,1))
      
      info <- rbind(info[[3]], info[[4]])
      
      colnames(info) <- c('jogador', 'time', 'R', 'ACS', 'K', 'D', 'A', '+/-', 'KAST', 'ADR', 'HS%', 'FK', 'FD', 'z')
      
      info <- select(info, 'jogador', 'R', 'ACS', 'K', 'D', 'A', 'KAST', 'ADR')
      
      info$R <- substr(info$R, 1, 4)
      info$ACS <- substr(info$ACS, 1, 3)
      info$K <- substr(info$K, 1, 2)
      info$D <- str_replace_all(info$D, '\t', '') %>%
        str_replace_all('\n', ' ') %>%
        str_replace_all('/  ', '')
      info$D <- substr(info$D, 1, 2)
      info$A <- substr(info$A, 1, 2)
      info$KAST <- substr(info$KAST, 1, 3)
      info$ADR <- substr(info$ADR, 1, 3)
      
      info <- separate(info, 'jogador', into = c("Player", "Team"), sep = "\\s+", extra = "merge")
      
      info <- cbind(info, ganhador)
      
      return(info)
    }
    , error = function(e){cat('error:', conditionMessage(e), '\n')})
  
}


rm (c, f, i, p, matchs, paginas)

m <- 1

dff <- list()

for (i in a[,]){
  tryCatch({
    dff[[length(dff)+1]] <- catalogarporUrl(a[m,])
    m = m + 1
  }, error = function(e){cat('error:', conditionMessage(e), '\n')})
}

dff <- dff %>% map_df(as_tibble)

write.csv2(dff, 'csv/historico.csv')

historico <- read.csv2('csv/historico.csv') %>% dplyr::select(-X)

id <- 1:117670

historico <- cbind(historico, id)

matriz_hist <- list()

z <- 1

while (z < count(historico)) {
  matriz_hist[[length(matriz_hist)+1]] <- historico[z:(z+9),]
  z = z + 10
}

partida <- 2

testes <- list()

testeF <- function(partida){
  time1Players <- matriz_hist[[partida]]$Player[1:5]
  time2Players <- matriz_hist[[partida]]$Player[6:10]
  time1Id <- matriz_hist[[partida]]$id[1:5]
  time2Id <- matriz_hist[[partida]]$id[6:10]
  time1 <- filter(historico, historico$Player==time1Players & historico$id > time1Id)
  time2 <- filter(historico, historico$Player==time2Players & historico$id > time2Id)
  time1R <- mean(as.numeric(time1$R))
  time2R <- mean(as.numeric(time2$R))
  time1ACS <- mean(time1$ACS)
  time2ACS <- mean(time2$ACS)
  time1KAST <- ifelse(is.character(time1$KAST), mean(parse_number(time1$KAST)), mean(time1$KAST))
  time2KAST <- ifelse(is.character(time2$KAST), mean(parse_number(time2$KAST)), mean(time2$KAST))
  time1KD <- mean(time1$K/time1$D)
  time2KD <- mean(time2$K/time2$D)
  time1ADR <- mean(time1$ADR)
  time2ADR <- mean(time2$ADR)
  ganhador <- ifelse(matriz_hist[[partida]]$ganhador[1] == 1, 1, 0)
  df <- cbind(time1R, time2R, time1ACS, time2ACS, time1KAST, time2KAST, time1KD, time2KD,
              time1ADR, time2ADR, ganhador)
  testes[[length(testes)+1]] <<- df
  return(testes)
  }

n <- 1

while(n < 11768){
  testeF(n)
  n = n + 1
}
