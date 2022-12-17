# Carregando pacotes ---------------------------------------------------------------------------------------
library(tidyverse)
library(FactoMineR)
library(factoextra)
library(cluster)
library(xlsx)
library(ineq)
library(stringr)
library(dplyr)
library(h2o)

# Carregando a base de dados de jogadores ---------------------------------------------------------------
dados_gerais <- read.csv2('jogadores.csv')

# Arrumando as colunas ----------------------------------------------------------------------------------
dados_gerais <- select(dados_gerais, Player, R, ACS, K.D, KAST, ADR)
row.names(dados_gerais) <- make.names(dados_gerais[,1], unique = T)
dados_gerais <- select(dados_gerais, -Player)
dados_gerais$KAST <- parse_number(dados_gerais$KAST)

# Definindo times especificos da competição CHAMPIONS ---------------------------------------------------------------------------
#Loud
loud = c('Sacy', 'pancada', 'saadhak', 'Less', 'aspas') # Definindo o time 1
loud <- paste0('\\b', loud, '\\b') # Colocando '\\b' antes e dps p pegar apenas as strings exatas
dados_gerais$loud <- ifelse(grepl(paste(loud, collapse = '|'), rownames(dados_gerais), useBytes = T), 1, 0)

#OpTic Gaming
op = c('crashies', 'Victor', 'Marved', 'FNS', 'yay')
op <- paste0('\\b', op, '\\b') 
dados_gerais$op <- ifelse(grepl(paste(op, collapse = '|'), rownames(dados_gerais), useBytes = T), 1, 0)

#DRX
drx = c('stax', 'Rb', 'Zest', 'BuZz', 'MaKo')
drx <- paste0('\\b', drx, '\\b')
dados_gerais$drx <- ifelse(grepl(paste(drx, collapse = '|'), rownames(dados_gerais), useBytes = T), 1, 0)

#XSET
xset = c('AYRIN', 'BcJ', 'dephh', 'zekken', 'Cryocells')
xset <- paste0('\\b', xset, '\\b')
dados_gerais$xset <- ifelse(grepl(paste(xset, collapse = '|'), rownames(dados_gerais), useBytes = T), 1, 0)

#TL
tl = c('ScreaM', 'soulcas', 'dimasick', 'Jamppi', 'Nivera')
tl <- paste0('\\b', tl, '\\b')
dados_gerais$tl <- ifelse(grepl(paste(tl, collapse = '|'), rownames(dados_gerais), useBytes = T), 1, 0)

#Leviatan
lev = c('Melser', 'adverso', 'Tacolilla', 'kiNgg', 'Shyy')
lev <- paste0('\\b', lev, '\\b')
dados_gerais$lev <- ifelse(grepl(paste(lev, collapse = '|'), rownames(dados_gerais), useBytes = T), 1, 0)

#FunPlus Phoenix
fpx = c('Zyppan', 'ardiis', 'ANGE1', 'Shao', 'SUYGETSU')
fpx <- paste0('\\b', fpx, '\\b')
dados_gerais$fpx <- ifelse(grepl(paste(fpx, collapse = '|'), rownames(dados_gerais), useBytes = T), 1 ,0)

#FNATIC
fntc = c('Enzo', 'Boaster', 'Mistic', 'Derke', 'Alfajer')
fntc <- paste0('\\b', fntc, '\\b')
dados_gerais$fntc <- ifelse(grepl(paste(fntc, collapse = '|'), rownames(dados_gerais), useBytes = T), 1, 0)

resultado <- filter(dados_gerais, dados_gerais$loud == 1 | dados_gerais$op == 1 | dados_gerais$drx == 1
                    | dados_gerais$xset == 1 | dados_gerais$tl == 1 | dados_gerais$lev == 1 | 
                      dados_gerais$fpx == 1 | dados_gerais$fntc == 1)

# Removendo uma jogadora que tem o mesmo de outra
while (nrow(resultado) > 40) {
  resultado <- resultado[-41,]
}

# Separando os times em dataframes
loud_df <- filter(resultado, resultado$loud == 1)
op_df <- filter(resultado, resultado$op == 1)
drx_df <- filter(resultado, resultado$drx == 1)
xset_df <- filter(resultado, resultado$xset == 1)
tl_df <- filter(resultado, resultado$tl == 1)
lev_df <- filter(resultado, resultado$lev == 1)
fpx_df <- filter(resultado, resultado$fpx == 1)
fntc_df <- filter(resultado, resultado$fntc == 1)

rm(loud, op, drx, xset, tl, lev, fpx, fntc)

# Tirando colunas de times dos dataframes especificos de cada time
loud_df <- loud_df[,-6:-13]
fntc_df <- fntc_df[,-6:-13]
xset_df <- xset_df[,-6:-13]
drx_df <- drx_df[,-6:-13]
lev_df <- lev_df[,-6:-13]
op_df <- op_df[,-6:-13]
tl_df <- tl_df[,-6:-13]
fpx_df <- fpx_df[,-6:-13]

#
drxR <- mean(drx_df$R)
levR <- mean(lev_df$R)
opR <- mean(op_df$R)
xsetR <- mean(xset_df$R)
fpxR <- mean(fpx_df$R)
tlR <- mean(tl_df$R)
loudR <- mean(loud_df$R)
fntcR <- mean(fntc_df$R)

time1 <- c(drxR, levR, opR, xsetR, fpxR, tlR, drxR, opR, xsetR, drxR, loudR, fpxR, loudR, opR)
time2 <- c(fpxR, loudR, tlR, fntcR, levR, fntcR, loudR, xsetR, fpxR, fntcR, opR, drxR, opR, drxR)
ganhador <- c(1, 2, 1, 1, 1, 2, 2, 1, 2, 1, 1, 2, 1, 1)

jogos <- data.frame(time1, time2, ganhador)

rm(drxR, levR, opR, xsetR, fpxR, tlR, loudR, fntcR, time1, time2, ganhador)

write.csv2(jogos, 'jogos.csv')

rm(list = ls())


# Carregando a base de dados de jogadores ---------------------------------------------------------------
dados_gerais <- read.csv2('jogadores.csv')

# Arrumando as colunas ----------------------------------------------------------------------------------
dados_gerais <- select(dados_gerais, Player, R, ACS, K.D, KAST, ADR)
row.names(dados_gerais) <- make.names(dados_gerais[,1], unique = T)
dados_gerais <- select(dados_gerais, -Player)
dados_gerais$KAST <- parse_number(dados_gerais$KAST)

# Definindo times especificos da competição GAME CHANGERS ---------------------------------------------------------------------------
#Cloud9 White
c9w = c('Bob', 'meL', 'Jazzyk1ns', 'alexis', 'katsumi') # Definindo o time 1
c9w <- paste0('\\b', c9w, '\\b') # Colocando '\\b' antes e dps p pegar apenas as strings exatas
dados_gerais$c9w <- ifelse(grepl(paste(c9w, collapse = '|'), rownames(dados_gerais), useBytes = T), 1, 0)

#Shopify Rebellion GC
sr = c('KP', 'bENITA', 'flowerful', 'sonder', 'Lorri')
sr <- paste0('\\b', sr, '\\b') 
dados_gerais$sr <- ifelse(grepl(paste(sr, collapse = '|'), rownames(dados_gerais), useBytes = T), 1, 0)

#Guild X
gldx = c('aNNja', 'cinnamon', 'Smurfette', 'roxi', 'ness')
gldx <- paste0('\\b', gldx, '\\b')
dados_gerais$gldx <- ifelse(grepl(paste(gldx, collapse = '|'), rownames(dados_gerais), useBytes = T), 1, 0)

#G2 Gozen
g2 = c('Glance', 'Petra', 'mimi', 'juliano', 'Mary')
g2 <- paste0('\\b', g2, '\\b')
dados_gerais$g2 <- ifelse(grepl(paste(g2, collapse = '|'), rownames(dados_gerais), useBytes = T), 1, 0)

#Team Liquid Brazil
tl = c('drn', 'naxy', 'bstrdd', 'daiki', 'nat1')
tl <- paste0('\\b', tl, '\\b')
dados_gerais$tl <- ifelse(grepl(paste(tl, collapse = '|'), rownames(dados_gerais), useBytes = T), 1, 0)

#KRÜ Fem
kru = c('consu', 'baesht', 'conir', 'kalita', 'romi')
kru <- paste0('\\b', kru, '\\b')
dados_gerais$kru <- ifelse(grepl(paste(kru, collapse = '|'), rownames(dados_gerais), useBytes = T), 1, 0)

#X10 Sapphire
x10 = c('JinNy', 'Muffyn', 'Babytz', 'Poly', 'alyssa')
x10 <- paste0('\\b', x10, '\\b')
dados_gerais$x10 <- ifelse(grepl(paste(x10, collapse = '|'), rownames(dados_gerais), useBytes = T), 1 ,0)

#FENNEL GC
flgc = c('suzu', 'KOHAL', 'Festival', 'Len', 'Curumi')
flgc <- paste0('\\b', flgc, '\\b')
dados_gerais$flgc <- ifelse(grepl(paste(flgc, collapse = '|'), rownames(dados_gerais), useBytes = T), 1, 0)

resultado <- filter(dados_gerais, dados_gerais$c9w == 1 | dados_gerais$sr == 1 | dados_gerais$gldx == 1
                    | dados_gerais$g2 == 1 | dados_gerais$tl == 1 | dados_gerais$kru == 1 | 
                      dados_gerais$x10 == 1 | dados_gerais$flgc == 1)

# Removendo uma jogadora que tem o mesmo de outra
while (nrow(resultado) > 40) {
  resultado <- resultado[-41,]
}

# Separando os times em dataframes
c9w_df <- filter(resultado, resultado$c9w == 1)
sr_df <- filter(resultado, resultado$sr == 1)
gldx_df <- filter(resultado, resultado$gldx == 1)
g2_df <- filter(resultado, resultado$g2 == 1)
tl_df <- filter(resultado, resultado$tl == 1)
kru_df <- filter(resultado, resultado$kru == 1)
x10_df <- filter(resultado, resultado$x10 == 1)
flgc_df <- filter(resultado, resultado$flgc == 1)

# Tirando colunas de times dos dataframes especificos de cada time
c9w_df <- c9w_df[,-6:-13]
flgc_df <- flgc_df[,-6:-13]
g2_df <- g2_df[,-6:-13]
gldx_df <- gldx_df[,-6:-13]
kru_df <- kru_df[,-6:-13]
sr_df <- sr_df[,-6:-13]
tl_df <- tl_df[,-6:-13]
x10_df <- x10_df[,-6:-13]

#
c9wR <- mean(c9w_df$R)
flgcR <- mean(flgc_df$R)
g2R <- mean(g2_df$R)
gldxR <- mean(gldx_df$R)
kruR <- mean(kru_df$R)
srR <- mean(sr_df$R)
tlR <- mean(tl_df$R)
x10R <- mean(x10_df$R)

time1 <- c(drxR, levR, opR, xsetR, fpxR, tlR, drxR, opR, xsetR, drxR, loudR, fpxR, loudR, opR)
time2 <- c(fpxR, loudR, tlR, fntcR, levR, fntcR, loudR, xsetR, fpxR, fntcR, opR, drxR, opR, drxR)
ganhador <- c(1, 2, 1, 1, 1, 2, 2, 1, 2, 1, 1, 2, 1, 1)

jogos <- data.frame(time1, time2, ganhador)

rm(drxR, levR, opR, xsetR, fpxR, tlR, loudR, fntcR, time1, time2, ganhador)

write.csv2(jogos, 'jogos2.csv')

