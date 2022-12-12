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
url_loud <- "https://www.vlr.gg/team/stats/6961/loud/"

# Pegando os dados dos times no url e transformando em dataframe ------------------------------------------
ds_adversarios_loud <- read_html(url_loud) %>% 
  html_node('table') %>% 
  html_table

# Renomeando as colunas 9 e 10 para tirar a ambiguidade que havia no dataframe que veio do site -----------
names(ds_adversarios_loud)[9] <- 'RW ATK'
names(ds_adversarios_loud)[10] <- 'RL ATK'

# Removendo duas colunas que não serão usadas -------------------------------------------------------------
ds_adversarios_loud <- select(ds_adversarios_loud, -Expand) %>% 
  select( -'Agent Compositions')

# Tirando todos os caracteres '\n e \t' do dataframe com a função lapply ----------------------------------
ds_adversarios_loud[-1] <- lapply(ds_adversarios_loud[-1], str_replace_all, "\t", ' ')

# Retirando as linhas que contem 1 ou mais caracterer na coluna Map ---------------------------------------
ds_adversarios_loud <- subset(ds_adversarios_loud, nchar(gsub("[^a-z]", "", ds_adversarios_loud$`Map (#)`)) < 1)

ds_adversarios_loud[c('Data', 'Resultado')] <- str_split_fixed(
  ds_adversarios_loud$`WIN%`, '\n', 2)

ds_adversarios_loud[c('Adversario', 'Resultado')] <- str_split_fixed(
  ds_adversarios_loud$Resultado, '\n\n\n', 2)

# Separando os conteúdos das linhas em duas novas colunas de Data e Resultado -----------------------------
ds_adversarios_loud[c('Data', 'Resultado')] <- str_split_fixed(
  ds_adversarios_loud$`WIN%`, ' ', 2)

# Selecionando apenas Data, Resultado e Adversario para o nosso dataframe ---------------------------------
ds_adversarios_loud <- select(ds_adversarios_loud, 'Data', 'Resultado', 'Adversario')

# Passando os dados da coluna Data para o formato de data -------------------------------------------------
ds_adversarios_loud$Data <- as_date(ds_adversarios_loud$Data)

# Deixando apenas números e "/"----------------------------------------------------------------------------
ds_adversarios_loud$Resultado <- gsub("[^0-9/ ]", "", ds_adversarios_loud$Resultado)

# Tirando valores númericos, que são dos nomes dos times, da coluna Resultado 
ds_adversarios_loud$Resultado <- gsub(" \\b[(^0-9)]+ ", "", ds_adversarios_loud$Resultado)

# Tirando dados que estão longes das barras ("/")
ds_adversarios_loud$Resultado <- substr(ds_adversarios_loud$Resultado,
                                       gregexpr("/", ds_adversarios_loud$Resultado)[[1]][1] - 9,
                                       gregexpr("/", ds_adversarios_loud$Resultado)[[3]][1] + 9)

# Transformando a coluna Resultado em RW e RL ------------------------------------------------------------
ds_adversarios_loud <- separate(ds_adversarios_loud, Resultado, c("RW", "RL"), "/") 

# Tirando todos os caracteres que estavam à direita e à esquerda
ds_adversarios_loud$RL <- sub(" .*", "", ds_adversarios_loud$RL) 
ds_adversarios_loud$RW <- sub("*. ", "", ds_adversarios_loud$RW) 

# Tirando todos os espaços
ds_adversarios_loud$RW <- sub(" ", "", ds_adversarios_loud$RW) 
ds_adversarios_loud$RL <- sub(" ", "", ds_adversarios_loud$RL) 

# Criando uma coluna de resultados
ds_adversarios_loud$Resultados <- as.numeric(ds_adversarios_loud$RW) > as.numeric(ds_adversarios_loud$RL) 

# Renomeando TRUE para 'Win' e FALSE para 'Lose'
ds_adversarios_loud$Resultados <- replace(ds_adversarios_loud$Resultados, ds_adversarios_loud$Resultados == TRUE, 'Win') %>% 
  replace(ds_adversarios_loud$Resultados == FALSE, 'Lose') 

# Limpando a coluna 'Adversario' --------------------------------------------------------------------------
ds_adversarios_loud[-1] <- lapply(ds_adversarios_loud[-1], str_replace_all, "\\s", ' ') %>% 
  lapply(str_replace_all, '  ', ' ') %>% 
  lapply(str_replace_all, '   ', ' ') %>% 
  lapply(str_replace_all, '  ', ' ') %>% 
  lapply(str_replace_all, '   ', ' ') %>% 
  lapply(str_replace_all, '  ', '')

# Exportando o arquivo
write.csv(ds_adversarios_loud, file = "C:/Users/anonb/Documents/TCC Pós/Scripts/scripts_times_champions/ds_adversarios_loud.csv")