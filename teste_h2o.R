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

# Definindo times especificos ---------------------------------------------------------------------------
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

# Calculando IDC (variancia de KAST entre os jogadores de cada time)
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


