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

#write.csv2(a, 'csv/a.csv')

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

historico_sem_formatar <- read.csv2('csv/historico_sem_formatar.csv') %>% dplyr::select(-X)

dff <- historico_sem_formatar %>% map_df(as_tibble)

dff <- t(historico_sem_formatar)

dff2 <- dff %>% map_df(as_tibble)

#dff <- na.omit(dff)

write.csv2(dff, 'csv/historico.csv')

historico <- read.csv2('csv/historico.csv') %>% dplyr::select(-X)

i <- 1
n <- 1
m <- 10
partidas <- list()

while(i < (count(historico) / 10) + 1){
partidas[[length(partidas)+1]] <- historico[n: m,]
n = n + 10
m = m + 10
i = i + 1
}

rm(historico, i, m, n)

# preciso colocar de volta valores NA

i <- 1
m <- 1
n <- 1
testando <- list()

while(i < length(partidas)){
  if(partidas[[m]]$Player[n] == jogador)
  testando[[length(testando)+1]] <- partidas[[m]][n,]
  if(n == 10){n = 0}
  m = m + 1
  n = n + 1
}

 
