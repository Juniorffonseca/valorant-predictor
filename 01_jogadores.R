# Carregando pacotes -------------------------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(rvest)
library(httr)

# Armazenando a url em uma variável -----------------------------------------------------------------------
link <- "https://www.vlr.gg/stats/?event_group_id=all&event_id=all&region=all&country=all&min_rounds=50&min_rating=1550&agent=all&map_id=all&timespan=all"

# Pegando os Dados no link e separando time e jogador em duas colunas -------------------------------------
players <- read_html(link) %>% 
  html_node("table") %>% 
  html_table() %>% 
  separate(Player, into = c("Player", "Team"), sep = "\\s+", extra = "merge") %>% 
  select('Player', 'Team', 'R', 'ACS', 'K:D', 'KAST', 'ADR') %>% 
  as.data.frame()

# Exportando como csv -------------------------------------------------------------------------------------
write.csv2(players, 'csv/jogadores.csv')