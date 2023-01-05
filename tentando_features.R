# Carregando pacotes -------------------------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(rvest)
library(httr)
library(ggplot2)
library(httr)
library(purrr)
library(readr)

# Carregando o dataframe -----------------------------------------------------------------------------------
jogos <- read.csv2('csv/df.csv') %>% dplyr::select(-X)
jogadores <- read.csv2('csv/jogadores.csv') %>% dplyr::select(-X)


# Esboços:

ggplot(jogadores, aes(ACS, R)) +
  geom_point()

geom_point(data = jogos)

jogadores %>% 
  group_by(R) %>% 
  summarise(ACS = mean(ACS, na.rm = TRUE)) %>% 
  ggplot() +
  geom_line(aes(x = R, y = ACS))

jogos %>% 
  group_by(time2R) %>% 
  ggplot() +
  geom_line(aes(x = time2R, y = time1R)) +
  geom_point(aes(x = time2R, y = time1R))


hist(jogadores$ADR)
hist(jogos$time1R)
hist(jogos$time2R)

jogadores %>% 
  count(R) %>%
  filter(!is.na(R)) %>% 
  slice_max(order_by = n, n = 25) %>% 
  ggplot() +
  geom_col(aes(x = R, y = n, fill = R), show.legend = FALSE) +
  geom_label(aes(x = R, y = n/2, label = n)) +
  coord_flip()

# ---------------------------

url <- 'https://www.vlr.gg/rankings'

a <- read_html(url) %>% 
  html_nodes('div.world-rankings-col') %>% 
  html_table()

melhores_times <- a %>% map_df(as_tibble)

colnames(melhores_times) <- c('Ranking', 'Time', 'Pontos')

write.csv(melhores_times, 'csv/melhores_times.csv')
