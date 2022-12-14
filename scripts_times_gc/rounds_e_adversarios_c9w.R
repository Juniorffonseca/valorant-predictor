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
url_c9w <- "https://www.vlr.gg/team/stats/1267/cloud9-white/"

# Pegando os dados dos times no url e transformando em dataframe ------------------------------------------
ds_adversarios_c9w <- read_html(url_c9w) %>% 
  html_node('table') %>% 
  html_table

# Renomeando as colunas 9 e 10 para tirar a ambiguidade que havia no dataframe que veio do site -----------
names(ds_adversarios_c9w)[9] <- 'RW ATK'
names(ds_adversarios_c9w)[10] <- 'RL ATK'

# Removendo duas colunas que não serão usadas -------------------------------------------------------------
ds_adversarios_c9w <- select(ds_adversarios_c9w, -Expand) %>% 
  select( -'Agent Compositions')

# Tirando todos os caracteres '\n e \t' do dataframe com a função lapply ----------------------------------
ds_adversarios_c9w[-1] <- lapply(ds_adversarios_c9w[-1], str_replace_all, "\t", ' ')

# Retirando as linhas que contem 1 ou mais caracterer na coluna Map ---------------------------------------
ds_adversarios_c9w <- subset(ds_adversarios_c9w, nchar(gsub("[^a-z]", "", ds_adversarios_c9w$`Map (#)`)) < 1)

ds_adversarios_c9w[c('Data', 'Resultado')] <- str_split_fixed(
  ds_adversarios_c9w$`WIN%`, '\n', 2)

ds_adversarios_c9w[c('Adversario', 'Resultado')] <- str_split_fixed(
  ds_adversarios_c9w$Resultado, '\n\n\n', 2)

# Separando os conteúdos das linhas em duas novas colunas de Data e Resultado -----------------------------
ds_adversarios_c9w[c('Data', 'Resultado')] <- str_split_fixed(
  ds_adversarios_c9w$`WIN%`, ' ', 2)

# Selecionando apenas Data, Resultado e Adversario para o nosso dataframe ---------------------------------
ds_adversarios_c9w <- select(ds_adversarios_c9w, 'Data', 'Resultado', 'Adversario')

# Passando os dados da coluna Data para o formato de data -------------------------------------------------
ds_adversarios_c9w$Data <- as_date(ds_adversarios_c9w$Data)

# Deixando apenas números e "/"----------------------------------------------------------------------------
ds_adversarios_c9w$Resultado <- gsub("[^0-9/ ]", "", ds_adversarios_c9w$Resultado)

# Tirando valores númericos, que são dos nomes dos times, da coluna Resultado 
ds_adversarios_c9w$Resultado <- gsub(" \\b[(^0-9)]+ ", "", ds_adversarios_c9w$Resultado)

# Tirando dados que estão longes das barras ("/")
ds_adversarios_c9w$Resultado <- substr(ds_adversarios_c9w$Resultado,
                                       gregexpr("/", ds_adversarios_c9w$Resultado)[[1]][1] - 9,
                                       gregexpr("/", ds_adversarios_c9w$Resultado)[[3]][1] + 9)

# Transformando a coluna Resultado em RW e RL ------------------------------------------------------------
ds_adversarios_c9w <- separate(ds_adversarios_c9w, Resultado, c("RW", "RL"), "/") 

# Tirando todos os caracteres que estavam à direita e à esquerda
ds_adversarios_c9w$RL <- sub(" .*", "", ds_adversarios_c9w$RL) 
ds_adversarios_c9w$RW <- sub("*. ", "", ds_adversarios_c9w$RW) 

# Tirando todos os espaços
ds_adversarios_c9w$RW <- sub(" ", "", ds_adversarios_c9w$RW) 
ds_adversarios_c9w$RL <- sub(" ", "", ds_adversarios_c9w$RL) 

# Criando uma coluna de resultados
ds_adversarios_c9w$Resultados <- as.numeric(ds_adversarios_c9w$RW) > as.numeric(ds_adversarios_c9w$RL) 

# Renomeando TRUE para 'Win' e FALSE para 'Lose'
ds_adversarios_c9w$Resultados <- replace(ds_adversarios_c9w$Resultados, ds_adversarios_c9w$Resultados == TRUE, 'Win') %>% 
  replace(ds_adversarios_c9w$Resultados == FALSE, 'Lose') 

# Limpando a coluna 'Adversario' --------------------------------------------------------------------------
ds_adversarios_c9w[-1] <- lapply(ds_adversarios_c9w[-1], str_replace_all, "\\s", ' ') %>% 
  lapply(str_replace_all, '  ', ' ') %>% 
  lapply(str_replace_all, '   ', ' ') %>% 
  lapply(str_replace_all, '  ', ' ') %>% 
  lapply(str_replace_all, '   ', ' ') %>% 
  lapply(str_replace_all, '  ', '')

# Exportando o arquivo ------------------------------------------------------------------------------------
write.csv(ds_adversarios_c9w, file = "C:/Users/anonb/Documents/TCC Pós/Scripts/scripts_times_gc/ds_adversarios_c9w.csv")

