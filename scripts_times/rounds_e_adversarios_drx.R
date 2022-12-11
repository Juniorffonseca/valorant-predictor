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
url_drx <- "https://www.vlr.gg/team/stats/8185/drx/"

# Pegando os dados dos times no url e transformando em dataframe ------------------------------------------
ds_adversarios_drx <- read_html(url_drx) %>% 
  html_node('table') %>% 
  html_table

# Renomeando as colunas 9 e 10 para tirar a ambiguidade que havia no dataframe que veio do site -----------
names(ds_adversarios_drx)[9] <- 'RW ATK'
names(ds_adversarios_drx)[10] <- 'RL ATK'

# Removendo duas colunas que não serão usadas -------------------------------------------------------------
ds_adversarios_drx <- select(ds_adversarios_drx, -Expand) %>% 
  select( -'Agent Compositions')

# Tirando todos os caracteres '\n e \t' do dataframe com a função lapply ----------------------------------
ds_adversarios_drx[-1] <- lapply(ds_adversarios_drx[-1], str_replace_all, "\t", ' ')

# Retirando as linhas que contem 1 ou mais caracterer na coluna Map ---------------------------------------
ds_adversarios_drx <- subset(ds_adversarios_drx, nchar(gsub("[^a-z]", "", ds_adversarios_drx$`Map (#)`)) < 1)

ds_adversarios_drx[c('Data', 'Resultado')] <- str_split_fixed(
  ds_adversarios_drx$`WIN%`, '\n', 2)

ds_adversarios_drx[c('Adversario', 'Resultado')] <- str_split_fixed(
  ds_adversarios_drx$Resultado, '\n\n\n', 2)

# Separando os conteúdos das linhas em duas novas colunas de Data e Resultado -----------------------------
ds_adversarios_drx[c('Data', 'Resultado')] <- str_split_fixed(
  ds_adversarios_drx$`WIN%`, ' ', 2)

# Selecionando apenas Data, Resultado e Adversario para o nosso dataframe ---------------------------------
ds_adversarios_drx <- select(ds_adversarios_drx, 'Data', 'Resultado', 'Adversario')

# Passando os dados da coluna Data para o formato de data -------------------------------------------------
ds_adversarios_drx$Data <- as_date(ds_adversarios_drx$Data)

# Deixando apenas números e "/"----------------------------------------------------------------------------
ds_adversarios_drx$Resultado <- gsub("[^0-9/ ]", "", ds_adversarios_drx$Resultado)

# Tirando valores númericos, que são dos nomes dos times, da coluna Resultado 
ds_adversarios_drx$Resultado <- gsub(" \\b[(^0-9)]+ ", "", ds_adversarios_drx$Resultado)

# Tirando dados que estão longes das barras ("/")
ds_adversarios_drx$Resultado <- substr(ds_adversarios_drx$Resultado,
                                        gregexpr("/", ds_adversarios_drx$Resultado)[[1]][1] - 9,
                                        gregexpr("/", ds_adversarios_drx$Resultado)[[3]][1] + 9)

# Transformando a coluna Resultado em RW e RL ------------------------------------------------------------
ds_adversarios_drx <- separate(ds_adversarios_drx, Resultado, c("RW", "RL"), "/") 

# Tirando todos os caracteres que estavam à direita e à esquerda
ds_adversarios_drx$RL <- sub(" .*", "", ds_adversarios_drx$RL) 
ds_adversarios_drx$RW <- sub("*. ", "", ds_adversarios_drx$RW) 

# Tirando todos os espaços
ds_adversarios_drx$RW <- sub(" ", "", ds_adversarios_drx$RW) 
ds_adversarios_drx$RL <- sub(" ", "", ds_adversarios_drx$RL) 

# Criando uma coluna de resultados
ds_adversarios_drx$Resultados <- as.numeric(ds_adversarios_drx$RW) > as.numeric(ds_adversarios_drx$RL) 

# Renomeando TRUE para 'Win' e FALSE para 'Lose'
ds_adversarios_drx$Resultados <- replace(ds_adversarios_drx$Resultados, ds_adversarios_drx$Resultados == TRUE, 'Win') %>% 
  replace(ds_adversarios_drx$Resultados == FALSE, 'Lose') 

# Limpando a coluna 'Adversario' --------------------------------------------------------------------------
ds_adversarios_drx[-1] <- lapply(ds_adversarios_drx[-1], str_replace_all, "\\s", ' ') %>% 
  lapply(str_replace_all, '  ', ' ') %>% 
  lapply(str_replace_all, '   ', ' ') %>% 
  lapply(str_replace_all, '  ', ' ') %>% 
  lapply(str_replace_all, '   ', ' ') %>% 
  lapply(str_replace_all, '  ', '')

# Exportando o arquivo ------------------------------------------------------------------------------------
write.csv(ds_adversarios_drx, file = "C:/Users/anonb/Documents/TCC Pós/Scripts/scripts_times/ds_adversarios_drx.csv")

