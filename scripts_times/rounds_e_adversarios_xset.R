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
url_xset <- "https://www.vlr.gg/team/stats/533/xset/"

# Pegando os dados dos times no url e transformando em dataframe ------------------------------------------
ds_adversarios_xset <- read_html(url_xset) %>% 
  html_node('table') %>% 
  html_table

# Renomeando as colunas 9 e 10 para tirar a ambiguidade que havia no dataframe que veio do site -----------
names(ds_adversarios_xset)[9] <- 'RW ATK'
names(ds_adversarios_xset)[10] <- 'RL ATK'

# Removendo duas colunas que não serão usadas -------------------------------------------------------------
ds_adversarios_xset <- select(ds_adversarios_xset, -Expand) %>% 
  select( -'Agent Compositions')

# Tirando todos os caracteres '\n e \t' do dataframe com a função lapply ----------------------------------
ds_adversarios_xset[-1] <- lapply(ds_adversarios_xset[-1], str_replace_all, "\t", ' ')

# Retirando as linhas que contem 1 ou mais caracterer na coluna Map ---------------------------------------
ds_adversarios_xset <- subset(ds_adversarios_xset, nchar(gsub("[^a-z]", "", ds_adversarios_xset$`Map (#)`)) < 1)

ds_adversarios_xset[c('Data', 'Resultado')] <- str_split_fixed(
  ds_adversarios_xset$`WIN%`, '\n', 2)

ds_adversarios_xset[c('Adversario', 'Resultado')] <- str_split_fixed(
  ds_adversarios_xset$Resultado, '\n\n\n', 2)

# Separando os conteúdos das linhas em duas novas colunas de Data e Resultado -----------------------------
ds_adversarios_xset[c('Data', 'Resultado')] <- str_split_fixed(
  ds_adversarios_xset$`WIN%`, ' ', 2)

# Selecionando apenas Data, Resultado e Adversario para o nosso dataframe ---------------------------------
ds_adversarios_xset <- select(ds_adversarios_xset, 'Data', 'Resultado', 'Adversario')

# Passando os dados da coluna Data para o formato de data -------------------------------------------------
ds_adversarios_xset$Data <- as_date(ds_adversarios_xset$Data)

# Deixando apenas números e "/"----------------------------------------------------------------------------
ds_adversarios_xset$Resultado <- gsub("[^0-9/ ]", "", ds_adversarios_xset$Resultado)

# Tirando valores númericos, que são dos nomes dos times, da coluna Resultado 
ds_adversarios_xset$Resultado <- gsub(" \\b[(^0-9)]+ ", "", ds_adversarios_xset$Resultado)

# Tirando dados que estão longes das barras ("/")
ds_adversarios_xset$Resultado <- substr(ds_adversarios_xset$Resultado,
                                       gregexpr("/", ds_adversarios_xset$Resultado)[[1]][1] - 9,
                                       gregexpr("/", ds_adversarios_xset$Resultado)[[3]][1] + 9)

# Transformando a coluna Resultado em RW e RL ------------------------------------------------------------
ds_adversarios_xset <- separate(ds_adversarios_xset, Resultado, c("RW", "RL"), "/") 

# Tirando todos os caracteres que estavam à direita e à esquerda
ds_adversarios_xset$RL <- sub(" .*", "", ds_adversarios_xset$RL) 
ds_adversarios_xset$RW <- sub("*. ", "", ds_adversarios_xset$RW) 

# Tirando todos os espaços
ds_adversarios_xset$RW <- sub(" ", "", ds_adversarios_xset$RW) 
ds_adversarios_xset$RL <- sub(" ", "", ds_adversarios_xset$RL) 

# Criando uma coluna de resultados
ds_adversarios_xset$Resultados <- as.numeric(ds_adversarios_xset$RW) > as.numeric(ds_adversarios_xset$RL) 

# Renomeando TRUE para 'Win' e FALSE para 'Lose'
ds_adversarios_xset$Resultados <- replace(ds_adversarios_xset$Resultados, ds_adversarios_xset$Resultados == TRUE, 'Win') %>% 
  replace(ds_adversarios_xset$Resultados == FALSE, 'Lose') 

# Limpando a coluna 'Adversario' --------------------------------------------------------------------------
ds_adversarios_xset[-1] <- lapply(ds_adversarios_xset[-1], str_replace_all, "\\s", ' ') %>% 
  lapply(str_replace_all, '  ', ' ') %>% 
  lapply(str_replace_all, '   ', ' ') %>% 
  lapply(str_replace_all, '  ', ' ') %>% 
  lapply(str_replace_all, '   ', ' ') %>% 
  lapply(str_replace_all, '  ', '')

# Exportando o arquivo
write.csv(ds_adversarios_xset, file = "C:/Users/anonb/Documents/TCC Pós/Scripts/scripts_times/ds_adversarios_xset.csv")