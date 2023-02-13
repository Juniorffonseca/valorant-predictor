#Instalando pacotes (se necessário)
library(devtools)
install_github("Juniorffonseca/r-pacote-valorant")

# Carregando pacotes --------------------------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(rvest)
library(quantmod)
library(httr)
library(tibble)
library(stringr)
library(reshape2)
library(readr)
library(purrr)
library(valorant)

# Criando variável páginas e criando variável 'p' que será a parte final do url (o número da página) -------
paginas <- ''
p <- 1

# Criando um laço for que armazenará o url de cada página dentro da variável paginas -----------------------
for (i in 1:17){
  paginas[p] <- paste('https://www.vlr.gg/matches/results/?page=', p, sep = '')
  p = p + 1
}

# Variável partidas e variável c ---------------------------------------------------------------------------
c <- 1
partidas <- 'a'

# Função que retorna o url de cada partida -----------------------------------------------------------------
funcaoPagina <- function(pagina){
  
  partidas <- read_html(pagina) %>% 
    html_nodes('a') %>% html_attr('href') # Nessa parte ele pega todos os urls que estão contidos na página.
  
  partidas <- partidas[15:64] # Aqui é separado os urls que são efetivamente de partidas.
  
  n <- 1
  
  for (i in partidas){
    partidas[n] <- paste('https://www.vlr.gg', partidas[n], sep = '') # Salvando urls dentro da variável partida
    n = n + 1
  }
  
  return(partidas)
  
}

# Criando f e uma lista que receberá todos os returns da funcaoPagina (url de cada partida) ----------------
f <- 1
a <- list()

# Executando um for que fará a iteração da funcaoPagina todas as vezes necessárias -------------------------
for (i in paginas){
  a[[length(a)+1]] = funcaoPagina(paginas[f])
  f = f + 1
}

# Fazendo unlist de 'a' e criando 'm' e 'dff' (lista que receberá todos os dados dos jogos) ----------------
m <- 1
a <- unlist(a)
dff <- list()

# Salvando os links que serão utilizados em um csv
#write.csv2(a, 'csv/urls.csv')

# Iteração para catalogar todos os jogos contidos nos urls armazenados --------------------------------------
for (i in a){
  tryCatch({
    dff[[length(dff)+1]] <- medias_Times(a[m], resultado = T)
    m = m + 1
  }, error = function(e){cat('error:', conditionMessage(e), '\n')})
}

# Passando os dados recebidos para um dataframe mais organizado --------------------------------------------
dff <- dff %>% map_df(as_tibble, .name_repair = "unique") # talvez não precise do .name_repair = "unique"

# Exportando como csv --------------------------------------------------------------------------------------
#write.csv2(dff, 'csv/partidas.csv')
