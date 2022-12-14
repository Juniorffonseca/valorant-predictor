# Carregando pacotes -------------------------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(rvest)
library(quantmod)
library(httr)
library(tibble)
library(stringr)
library(car)

# Armazenando a url em uma variável -----------------------------------------------------------------------
vlr_url_players <- "https://www.vlr.gg/stats/?event_group_id=all&event_id=all&region=all&country=all&min_rounds=1&min_rating=1550&agent=all&map_id=all&timespan=all"

# Pegando os Dados no link -------------------------------------------------------------------------------
players <- read_html(vlr_url_players) %>% 
  html_node("table") %>% 
  html_table()

# Removendo a  coluna de Agentes -------------------------------------------------------------------------
players <- select(players, -Agents)

# Separando time e jogador em duas colunas --------------------------------------------------------------
players <- separate(players, Player, into = c("Player", "Team"), sep = "\\s+", extra = "merge")

# Removendo jogadores sem times -------------------------------------------------------------------------
#players <- na.omit(players)

# Criando uma coluna IK, onde IK = FK - FD (Impact kill = first kill - first death) ---------------------
players["IK"] <- players$FK - players$FD

# Select para pegar apenas dados que serão utilizados agora ----------------------------------------------
players <- select(players, 'Player', 'Team', 'Rnd', 'R', 'ACS', 'K:D', 'KAST', 'ADR', 'HS%', 'KMax', 'IK')

# Exportando como csv -------------------------------------------------------------------------------------
write.csv2(players, 'jogadores_all.csv')

