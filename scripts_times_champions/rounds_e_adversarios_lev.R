# Carregando pacotes --------------------------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(rvest)
library(quantmod)
library(httr)
library(tibble)
library(stringr)
library(lubridate)

# Urls ----------------------------------------------------------------------------------------------------
url_lev <- "https://www.vlr.gg/team/stats/2359/leviat-n/"

# Pegando os dados dos times no url e transformando em dataframe ------------------------------------------
ds_adversarios_lev <- read_html(url_lev) %>% 
  html_node('table') %>% 
  html_table

# Renomeando as colunas 9 e 10 para tirar a ambiguidade que havia no dataframe que veio do site -----------
names(ds_adversarios_lev)[9] <- 'RW ATK'
names(ds_adversarios_lev)[10] <- 'RL ATK'

# Removendo duas colunas que não serão usadas -------------------------------------------------------------
ds_adversarios_lev <- select(ds_adversarios_lev, -Expand) %>% 
  select( -'Agent Compositions')

# Tirando todos os caracteres '\n e \t' do dataframe com a função lapply ----------------------------------
ds_adversarios_lev[-1] <- lapply(ds_adversarios_lev[-1], str_replace_all, "\t", ' ')

# Retirando as linhas que contem 1 ou mais caracterer na coluna Map ---------------------------------------
ds_adversarios_lev <- subset(ds_adversarios_lev, nchar(gsub("[^a-z]", "", ds_adversarios_lev$`Map (#)`)) < 1)

ds_adversarios_lev[c('Data', 'Resultado')] <- str_split_fixed(
  ds_adversarios_lev$`WIN%`, '\n', 2)

ds_adversarios_lev[c('Adversario', 'Resultado')] <- str_split_fixed(
  ds_adversarios_lev$Resultado, '\n\n\n', 2)

# Separando os conteúdos das linhas em duas novas colunas de Data e Resultado -----------------------------
ds_adversarios_lev[c('Data', 'Resultado')] <- str_split_fixed(
  ds_adversarios_lev$`WIN%`, ' ', 2)

# Selecionando apenas Data, Resultado e Adversario para o nosso dataframe ---------------------------------
ds_adversarios_lev <- select(ds_adversarios_lev, 'Data', 'Resultado', 'Adversario')

# Passando os dados da coluna Data para o formato de data -------------------------------------------------
ds_adversarios_lev$Data <- as_date(ds_adversarios_lev$Data)

# Deixando apenas números e "/"----------------------------------------------------------------------------
ds_adversarios_lev$Resultado <- gsub("[^0-9/ ]", "", ds_adversarios_lev$Resultado)

# Tirando valores númericos, que são dos nomes dos times, da coluna Resultado 
ds_adversarios_lev$Resultado <- gsub(" \\b[(^0-9)]+ ", "", ds_adversarios_lev$Resultado)

# Tirando dados que estão longes das barras ("/")
ds_adversarios_lev$Resultado <- substr(ds_adversarios_lev$Resultado,
                                       gregexpr("/", ds_adversarios_lev$Resultado)[[1]][1] - 9,
                                       gregexpr("/", ds_adversarios_lev$Resultado)[[3]][1] + 9)

# Transformando a coluna Resultado em RW e RL ------------------------------------------------------------
ds_adversarios_lev <- separate(ds_adversarios_lev, Resultado, c("RW", "RL"), "/") 

# Tirando todos os caracteres que estavam à direita e à esquerda
ds_adversarios_lev$RL <- sub(" .*", "", ds_adversarios_lev$RL) 
ds_adversarios_lev$RW <- sub("*. ", "", ds_adversarios_lev$RW) 

# Tirando todos os espaços
ds_adversarios_lev$RW <- sub(" ", "", ds_adversarios_lev$RW) 
ds_adversarios_lev$RL <- sub(" ", "", ds_adversarios_lev$RL) 

# Criando uma coluna de resultados
ds_adversarios_lev$Resultados <- as.numeric(ds_adversarios_lev$RW) > as.numeric(ds_adversarios_lev$RL) 

# Renomeando TRUE para 'Win' e FALSE para 'Lose'
ds_adversarios_lev$Resultados <- replace(ds_adversarios_lev$Resultados, ds_adversarios_lev$Resultados == TRUE, 'Win') %>% 
  replace(ds_adversarios_lev$Resultados == FALSE, 'Lose') 

# Limpando a coluna 'Adversario' --------------------------------------------------------------------------
ds_adversarios_lev[-1] <- lapply(ds_adversarios_lev[-1], str_replace_all, "\\s", ' ') %>% 
  lapply(str_replace_all, '  ', ' ') %>% 
  lapply(str_replace_all, '   ', ' ') %>% 
  lapply(str_replace_all, '  ', ' ') %>% 
  lapply(str_replace_all, '   ', ' ') %>% 
  lapply(str_replace_all, '  ', '')

# Exportando o arquivo
write.csv(ds_adversarios_lev, file = "C:/Users/anonb/Documents/TCC Pós/Scripts/scripts_times_champions/ds_adversarios_lev.csv")