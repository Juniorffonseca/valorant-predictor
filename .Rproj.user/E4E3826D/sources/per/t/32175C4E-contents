# Carregando pacotes ---------------------------------------------------------------------------------------
library(tidyverse)
library(FactoMineR)
library(factoextra)
library(cluster)
library(xlsx)
library(Hmisc)
library(rlang)

# Importando dados -----------------------------------------------------------------------------------------
dados <- read.csv2('jogadores.csv')

# Arrumando as colunas
dados <- select(dados, -X)
dados2 <- dados
dados <- select(dados, -Player, -Team, -Rnd, -'HS.', -KMax, -IK, -R)
dados2 <- select(dados2, -'HS.', -'KMax', -Rnd, -IK)

# Colocando row.names
row.names(dados) <- make.names(dados2$Player, unique = T)

# Tirando porcentagem
dados2$KAST <- parse_number(dados2$KAST)
dados$KAST <- parse_number(dados$KAST)

# Tirando valores NAs
#dados <- na.omit(dados)

# Fazendo scale dos dados
dados <- scale(dados)

# Filling NAS
dados[is.na(dados)] = 0

# Definir quantidade otima de cluster
#fviz_nbclust(dados, kmeans, method = 'gap_stat')

# Gerar o kmeans
set.seed('13')
dados_kmeans <- kmeans(dados, 3)

# Visualizar no gráfico
#fviz_cluster(dados_kmeans, data = dados)

# Lista com os clusters
lista <- dados_kmeans$cluster

# Agrupando os dados em uma tabela
dados_gerais <- cbind(dados2, lista)
rm(lista) # removendo a lista após concatenar ela com dados2

# Obtendo os centros de cada cluster
#centers <- as.data.frame(dados_kmeans$centers)

# Renomeando as numerações da lista para melhor entendimento
#dados_gerais$lista <- replace(dados_gerais$lista, dados_gerais$lista == '2', '1') %>% 
#  replace(dados_gerais$lista == '3', '2') %>% 
#  replace(dados_gerais$lista == '1','3')

# Passando lista para númerico
dados_gerais$lista <- as.numeric(dados_gerais$lista)

# Mudando nome da coluna lista para 'Elo'
data.table::setnames(dados_gerais, 'lista', 'Elo')

# Exportando os dados
write.csv2(dados_gerais, 'dados_gerais.csv')

#------------------------------------- #ESBOÇO# ---------------------------------------------------------

# Segmentando os centers em 3 divisões diferentes
#center1 <- centers[2,]
#center2 <- centers[3,]
#center3 <- centers[1,]

# Gerando estatisticas aleatorias de uma proxima partida desses jogadores
#jogador1 <- rnorm(1000000, mean = c9w_df$ACS[1])
#jogador2 <- rnorm(1000000, mean = c9w_df$ACS[2])
#jogador3 <- rnorm(1000000, mean = c9w_df$ACS[3])
#jogador4 <- rnorm(1000000, mean = c9w_df$ACS[4])
#jogador5 <- rnorm(1000000, mean = c9w_df$ACS[5])
#jogador6 <- rnorm(1000000, mean = c9w_df$ACS[1])
#jogador7 <- rnorm(1000000, mean = c9w_df$ACS[2])
#jogadores <- data.frame(jogador1,jogador2, jogador3, jogador4, jogador5)
#hist(jogador1, col = 'green')
#hist(jogador2, col = 'green')
#hist(jogadores, col = 'green')
#summary(jogadores)
#rnorm(5, mean = time1_df$KAST, sd = time1_df$Elo)



