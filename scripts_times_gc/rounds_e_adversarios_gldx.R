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
url_gldx <- "https://www.vlr.gg/team/stats/6263/guild-x/"

# Pegando os dados dos times no url e transformando em dataframe ------------------------------------------
ds_adversarios_gldx <- read_html(url_gldx) %>% 
  html_node('table') %>% 
  html_table

# Renomeando as colunas 9 e 10 para tirar a ambiguidade que havia no dataframe que veio do site -----------
names(ds_adversarios_gldx)[9] <- 'RW ATK'
names(ds_adversarios_gldx)[10] <- 'RL ATK'

# Removendo duas colunas que não serão usadas -------------------------------------------------------------
ds_adversarios_gldx <- select(ds_adversarios_gldx, -Expand) %>% 
  select( -'Agent Compositions')

# Tirando todos os caracteres '\n e \t' do dataframe com a função lapply ----------------------------------
ds_adversarios_gldx[-1] <- lapply(ds_adversarios_gldx[-1], str_replace_all, "\t", ' ')

# Retirando as linhas que contem 1 ou mais caracterer na coluna Map ---------------------------------------
ds_adversarios_gldx <- subset(ds_adversarios_gldx, nchar(gsub("[^a-z]", "", ds_adversarios_gldx$`Map (#)`)) < 1)

ds_adversarios_gldx[c('Data', 'Resultado')] <- str_split_fixed(
  ds_adversarios_gldx$`WIN%`, '\n', 2)

ds_adversarios_gldx[c('Adversario', 'Resultado')] <- str_split_fixed(
  ds_adversarios_gldx$Resultado, '\n\n\n', 2)

# Separando os conteúdos das linhas em duas novas colunas de Data e Resultado -----------------------------
ds_adversarios_gldx[c('Data', 'Resultado')] <- str_split_fixed(
  ds_adversarios_gldx$`WIN%`, ' ', 2)

# Selecionando apenas Data, Resultado e Adversario para o nosso dataframe ---------------------------------
ds_adversarios_gldx <- select(ds_adversarios_gldx, 'Data', 'Resultado', 'Adversario')

# Passando os dados da coluna Data para o formato de data -------------------------------------------------
ds_adversarios_gldx$Data <- as_date(ds_adversarios_gldx$Data)

# Deixando apenas números e "/"----------------------------------------------------------------------------
ds_adversarios_gldx$Resultado <- gsub("[^0-9/ ]", "", ds_adversarios_gldx$Resultado)

# Tirando valores númericos, que são dos nomes dos times, da coluna Resultado 
ds_adversarios_gldx$Resultado <- gsub(" \\b[(^0-9)]+ ", "", ds_adversarios_gldx$Resultado)

# Tirando dados que estão longes das barras ("/")
ds_adversarios_gldx$Resultado <- substr(ds_adversarios_gldx$Resultado,
                                       gregexpr("/", ds_adversarios_gldx$Resultado)[[1]][1] - 9,
                                       gregexpr("/", ds_adversarios_gldx$Resultado)[[3]][1] + 9)

# Transformando a coluna Resultado em RW e RL ------------------------------------------------------------
ds_adversarios_gldx <- separate(ds_adversarios_gldx, Resultado, c("RW", "RL"), "/") 

# Tirando todos os caracteres que estavam à direita e à esquerda
ds_adversarios_gldx$RL <- sub(" .*", "", ds_adversarios_gldx$RL) 
ds_adversarios_gldx$RW <- sub("*. ", "", ds_adversarios_gldx$RW) 

# Tirando todos os espaços
ds_adversarios_gldx$RW <- sub(" ", "", ds_adversarios_gldx$RW) 
ds_adversarios_gldx$RL <- sub(" ", "", ds_adversarios_gldx$RL) 

# Criando uma coluna de resultados
ds_adversarios_gldx$Resultados <- as.numeric(ds_adversarios_gldx$RW) > as.numeric(ds_adversarios_gldx$RL) 

# Renomeando TRUE para 'Win' e FALSE para 'Lose'
ds_adversarios_gldx$Resultados <- replace(ds_adversarios_gldx$Resultados, ds_adversarios_gldx$Resultados == TRUE, 'Win') %>% 
  replace(ds_adversarios_gldx$Resultados == FALSE, 'Lose') 

# Limpando a coluna 'Adversario' --------------------------------------------------------------------------
ds_adversarios_gldx[-1] <- lapply(ds_adversarios_gldx[-1], str_replace_all, "\\s", ' ') %>% 
  lapply(str_replace_all, '  ', ' ') %>% 
  lapply(str_replace_all, '   ', ' ') %>% 
  lapply(str_replace_all, '  ', ' ') %>% 
  lapply(str_replace_all, '   ', ' ') %>% 
  lapply(str_replace_all, '  ', '')

# Exportando o arquivo ------------------------------------------------------------------------------------
write.csv(ds_adversarios_gldx, file = "C:/Users/anonb/Documents/TCC Pós/Scripts/scripts_times_gc/ds_adversarios_gldx.csv")

