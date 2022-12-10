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
url_fpx <- "https://www.vlr.gg/team/stats/628/funplus-phoenix/"

# Pegando os dados dos times no url e transformando em dataframe ------------------------------------------
ds_adversarios_fpx <- read_html(url_fpx) %>% 
  html_node('table') %>% 
  html_table

# Renomeando as colunas 9 e 10 para tirar a ambiguidade que havia no dataframe que veio do site -----------
names(ds_adversarios_fpx)[9] <- 'RW ATK'
names(ds_adversarios_fpx)[10] <- 'RL ATK'

# Removendo duas colunas que não serão usadas -------------------------------------------------------------
ds_adversarios_fpx <- select(ds_adversarios_fpx, -Expand) %>% 
  select( -'Agent Compositions')

# Tirando todos os caracteres '\n e \t' do dataframe com a função lapply ----------------------------------
ds_adversarios_fpx[-1] <- lapply(ds_adversarios_fpx[-1], str_replace_all, "\t", ' ')

# Retirando as linhas que contem 1 ou mais caracterer na coluna Map ---------------------------------------
ds_adversarios_fpx <- subset(ds_adversarios_fpx, nchar(gsub("[^a-z]", "", ds_adversarios_fpx$`Map (#)`)) < 1)

ds_adversarios_fpx[c('Data', 'Resultado')] <- str_split_fixed(
  ds_adversarios_fpx$`WIN%`, '\n', 2)

ds_adversarios_fpx[c('Adversario', 'Resultado')] <- str_split_fixed(
  ds_adversarios_fpx$Resultado, '\n\n\n', 2)

# Separando os conteúdos das linhas em duas novas colunas de Data e Resultado -----------------------------
ds_adversarios_fpx[c('Data', 'Resultado')] <- str_split_fixed(
  ds_adversarios_fpx$`WIN%`, ' ', 2)

# Selecionando apenas Data, Resultado e Adversario para o nosso dataframe ---------------------------------
ds_adversarios_fpx <- select(ds_adversarios_fpx, 'Data', 'Resultado', 'Adversario')

# Passando os dados da coluna Data para o formato de data -------------------------------------------------
ds_adversarios_fpx$Data <- as_date(ds_adversarios_fpx$Data)

# Limpando os dados para deixar apenas os resultados------------------------------------------------------
ds_adversarios_fpx$Resultado <- gsub("[^0-9/ .-]", "", ds_adversarios_fpx$Resultado)# Deixando apenas números e "/"
#
ds_adversarios_fpx$Resultado <- substr(ds_adversarios_fpx$Resultado,
                                        gregexpr("/", ds_adversarios_fpx$Resultado)[[1]][1] - 3,
                                        gregexpr("/", ds_adversarios_fpx$Resultado)[[3]][1] + 3) # Tirando dados que estão longes das barras ("/")

ds_adversarios_fpx <- separate(ds_adversarios_fpx, Resultado, c("RW", "RL"), "/") # Transformando a coluna Resultado em RW e RL

ds_adversarios_fpx$RL <- sub(" .*", "", ds_adversarios_fpx$RL) # Tirando todos os caracteres que estavam à direita
ds_adversarios_fpx$RW <- sub("*. ", "", ds_adversarios_fpx$RW) # Tirando todos os caracteres que estavam à esquerda

ds_adversarios_fpx$RW <- sub(" ", "", ds_adversarios_fpx$RW) # Tirando todos os espaços
ds_adversarios_fpx$RL <- sub(" ", "", ds_adversarios_fpx$RL) # Tirando todos os espaços

ds_adversarios_fpx$Resultados <- as.numeric(ds_adversarios_fpx$RW) > as.numeric(ds_adversarios_fpx$RL) # Criando uma coluna de resultados

ds_adversarios_fpx$Resultados <- replace(ds_adversarios_fpx$Resultados, ds_adversarios_fpx$Resultados == TRUE, 'Win') %>% 
  replace(ds_adversarios_fpx$Resultados == FALSE, 'Lose') # Renomeando TRUE para 'Win' e FALSE para 'Lose'

# Limpando a coluna 'Adversario' --------------------------------------------------------------------------
ds_adversarios_fpx[-1] <- lapply(ds_adversarios_fpx[-1], str_replace_all, "\\s", ' ') %>% 
  lapply(str_replace_all, '  ', ' ') %>% 
  lapply(str_replace_all, '   ', ' ') %>% 
  lapply(str_replace_all, '  ', ' ') %>% 
  lapply(str_replace_all, '   ', ' ')

ds_adversarios_fpx[-1] <- lapply(ds_adversarios_fpx[-1], str_replace_all, "  ", '')

# Exportando o arquivo
write.csv(ds_adversarios_fpx, file = "C:/Users/anonb/Documents/TCC Pós/Scripts/scripts_times/ds_adversarios_fpx.csv")
