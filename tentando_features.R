# Carregando pacotes -------------------------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(rvest)
library(httr)
library(ggplot2)

# Carregando o dataframe -----------------------------------------------------------------------------------
jogos <- read.csv2('csv/df.csv') %>% dplyr::select(-X)
jogadores <- read.csv2('csv/jogadores.csv') %>% dplyr::select(-X)


# Esboços:

ggplot(data = jogadores)
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



