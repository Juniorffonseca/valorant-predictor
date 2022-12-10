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
url_fntc <- "https://www.vlr.gg/team/stats/2593/fnatic/"

# Pegando os dados dos times no url e transformando em dataframe ------------------------------------------
ds_adversarios_fntc <- read_html(url_fntc) %>% 
  html_node('table') %>% 
  html_table

# Renomeando as colunas 9 e 10 para tirar a ambiguidade que havia no dataframe que veio do site -----------
names(ds_adversarios_fntc)[9] <- 'RW ATK'
names(ds_adversarios_fntc)[10] <- 'RL ATK'

# Removendo duas colunas que não serão usadas -------------------------------------------------------------
ds_adversarios_fntc <- select(ds_adversarios_fntc, -Expand) %>% 
  select( -'Agent Compositions')

# Tirando todos os caracteres '\n e \t' do dataframe com a função lapply ----------------------------------
ds_adversarios_fntc[-1] <- lapply(ds_adversarios_fntc[-1], str_replace_all, "\t", ' ')

# Retirando as linhas que contem 1 ou mais caracterer na coluna Map ---------------------------------------
ds_adversarios_fntc <- subset(ds_adversarios_fntc, nchar(gsub("[^a-z]", "", ds_adversarios_fntc$`Map (#)`)) < 1)

ds_adversarios_fntc[c('Data', 'Resultado')] <- str_split_fixed(
  ds_adversarios_fntc$`WIN%`, '\n', 2)

ds_adversarios_fntc[c('Adversario', 'Resultado')] <- str_split_fixed(
  ds_adversarios_fntc$Resultado, '\n\n\n', 2)

# Separando os conteúdos das linhas em duas novas colunas de Data e Resultado -----------------------------
ds_adversarios_fntc[c('Data', 'Resultado')] <- str_split_fixed(
  ds_adversarios_fntc$`WIN%`, ' ', 2)

# Selecionando apenas Data, Resultado e Adversario para o nosso dataframe ---------------------------------
ds_adversarios_fntc <- select(ds_adversarios_fntc, 'Data', 'Resultado', 'Adversario')

# Passando os dados da coluna Data para o formato de data -------------------------------------------------
ds_adversarios_fntc$Data <- as_date(ds_adversarios_fntc$Data)

# Limpando os dados para deixar apenas os resultados------------------------------------------------------
ds_adversarios_fntc$Resultado <- gsub("[^0-9/ .-]", "", ds_adversarios_fntc$Resultado)# Deixando apenas números e "/"
#
ds_adversarios_fntc$Resultado <- substr(ds_adversarios_fntc$Resultado,
                                        gregexpr("/", ds_adversarios_fntc$Resultado)[[1]][1] - 3,
                                        gregexpr("/", ds_adversarios_fntc$Resultado)[[3]][1] + 3) # Tirando dados que estão longes das barras ("/")

ds_adversarios_fntc <- separate(ds_adversarios_fntc, Resultado, c("RW", "RL"), "/") # Transformando a coluna Resultado em RW e RL

ds_adversarios_fntc$RL <- sub(" .*", "", ds_adversarios_fntc$RL) # Tirando todos os caracteres que estavam à direita
ds_adversarios_fntc$RW <- sub("*. ", "", ds_adversarios_fntc$RW) # Tirando todos os caracteres que estavam à esquerda

ds_adversarios_fntc$RW <- sub(" ", "", ds_adversarios_fntc$RW) # Tirando todos os espaços
ds_adversarios_fntc$RL <- sub(" ", "", ds_adversarios_fntc$RL) # Tirando todos os espaços

ds_adversarios_fntc$Resultados <- as.numeric(ds_adversarios_fntc$RW) > as.numeric(ds_adversarios_fntc$RL) # Criando uma coluna de resultados

ds_adversarios_fntc$Resultados <- replace(ds_adversarios_fntc$Resultados, ds_adversarios_fntc$Resultados == TRUE, 'Win') %>% 
  replace(ds_adversarios_fntc$Resultados == FALSE, 'Lose') # Renomeando TRUE para 'Win' e FALSE para 'Lose'

# Limpando a coluna 'Adversario' --------------------------------------------------------------------------
ds_adversarios_fntc[-1] <- lapply(ds_adversarios_fntc[-1], str_replace_all, "\\s", ' ') %>% 
  lapply(str_replace_all, '  ', ' ') %>% 
  lapply(str_replace_all, '   ', ' ') %>% 
  lapply(str_replace_all, '  ', ' ') %>% 
  lapply(str_replace_all, '   ', ' ')

ds_adversarios_fntc[-1] <- lapply(ds_adversarios_fntc[-1], str_replace_all, "  ", '')

# Exportando o arquivo
write.csv(ds_adversarios_fntc, file = "C:/Users/anonb/Documents/TCC Pós/Scripts/scripts_times/ds_adversarios_fntc.csv")
