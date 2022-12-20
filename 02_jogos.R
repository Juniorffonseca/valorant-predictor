# Carregando pacotes ---------------------------------------------------------------------------------------
library(tidyverse)
library(FactoMineR)
library(factoextra)
library(cluster)
library(xlsx)
library(ineq)
library(stringr)
library(dplyr)

# CHAMPIONS -------------------------------------------------------------------------------------------------
# Carregando a base de dados de jogadores 
dados_gerais <- read.csv2('jogadores.csv')

# Arrumando as colunas 
dados_gerais <- dplyr::select(dados_gerais, Player, R, ACS, K.D, KAST, ADR)
row.names(dados_gerais) <- make.names(dados_gerais[,1], unique = T)
dados_gerais <- dplyr::select(dados_gerais, -Player)
dados_gerais$KAST <- parse_number(dados_gerais$KAST)

# Definindo times especificos da competição CHAMPIONS 
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

# Média R (Rating)
drxR <- mean(drx_df$R) 
levR <- mean(lev_df$R) 
opR <- mean(op_df$R) 
xsetR <- mean(xset_df$R) 
fpxR <- mean(fpx_df$R) 
tlR <- mean(tl_df$R) 
loudR <- mean(loud_df$R) 
fntcR <- mean(fntc_df$R) 
# Média ACS
drxACS <- mean(drx_df$ACS) 
levACS <- mean(lev_df$ACS) 
opACS <- mean(op_df$ACS) 
xsetACS <- mean(xset_df$ACS) 
fpxACS <- mean(fpx_df$ACS) 
tlACS <- mean(tl_df$ACS) 
loudACS <- mean(loud_df$ACS) 
fntcACS <- mean(fntc_df$ACS) 
# Média KD
drxKD <- mean(drx_df$K.D) 
levKD <- mean(lev_df$K.D) 
opKD <- mean(op_df$K.D) 
xsetKD <- mean(xset_df$K.D) 
fpxKD <- mean(fpx_df$K.D) 
tlKD <- mean(tl_df$K.D) 
loudKD <- mean(loud_df$K.D) 
fntcKD <- mean(fntc_df$K.D) 
# Média KAST
drxKAST <- mean(drx_df$KAST) 
levKAST <- mean(lev_df$KAST) 
opKAST <- mean(op_df$KAST) 
xsetKAST <- mean(xset_df$KAST) 
fpxKAST <- mean(fpx_df$KAST) 
tlKAST <- mean(tl_df$KAST) 
loudKAST <- mean(loud_df$KAST) 
fntcKAST <- mean(fntc_df$KAST) 
# Média ADR
drxADR <- mean(drx_df$ADR) 
levADR <- mean(lev_df$ADR) 
opADR <- mean(op_df$ADR) 
xsetADR <- mean(xset_df$ADR) 
fpxADR <- mean(fpx_df$ADR) 
tlADR <- mean(tl_df$ADR) 
loudADR <- mean(loud_df$ADR) 
fntcADR <- mean(fntc_df$ADR) 
#Criando o dataframe
time1R <- c(drxR, levR, opR, xsetR, fpxR, tlR, drxR, opR, xsetR, drxR, loudR, fpxR, loudR, opR)
time2R <- c(fpxR, loudR, tlR, fntcR, levR, fntcR, loudR, xsetR, fpxR, fntcR, opR, drxR, opR, drxR)
time1ACS <-c(drxACS, levACS, opACS, xsetACS, fpxACS, tlACS, drxACS, opACS, xsetACS, drxACS, loudACS, fpxACS, loudACS, opACS)
time2ACS <- c(fpxACS, loudACS, tlACS, fntcACS, levACS, fntcACS, loudACS, xsetACS, fpxACS, fntcACS, opACS, drxACS, opACS, drxACS)
time1KD <-c(drxKD, levKD, opKD, xsetKD, fpxKD, tlKD, drxKD, opKD, xsetKD, drxKD, loudKD, fpxKD, loudKD, opKD)
time2KD <- c(fpxKD, loudKD, tlKD, fntcKD, levKD, fntcKD, loudKD, xsetKD, fpxKD, fntcKD, opKD, drxKD, opKD, drxKD)
time1KAST <-c(drxKAST, levKAST, opKAST, xsetKAST, fpxKAST, tlKAST, drxKAST, opKAST, xsetKAST, drxKAST, loudKAST, fpxKAST, loudKAST, opKAST)
time2KAST <- c(fpxKAST, loudKAST, tlKAST, fntcKAST, levKAST, fntcKAST, loudKAST, xsetKAST, fpxKAST, fntcKAST, opKAST, drxKAST, opKAST, drxKAST)
time1ADR <-c(drxADR, levADR, opADR, xsetADR, fpxADR, tlADR, drxADR, opADR, xsetADR, drxADR, loudADR, fpxADR, loudADR, opADR)
time2ADR <- c(fpxADR, loudADR, tlADR, fntcADR, levADR, fntcADR, loudADR, xsetADR, fpxADR, fntcADR, opADR, drxADR, opADR, drxADR)

ganhador <- c(1, 0, 1, 1, 1, 0, 0, 1, 0, 1, 1, 0, 1, 1)

jogos <- data.frame(time1R, time2R, time1ACS, time2ACS, time1KD, time2KD, time1KAST, time2KAST, time1ADR, time2ADR, ganhador)

write.csv2(jogos, 'csv/jogos1.csv')

rm(list = ls())


# GAME CHANGERS ----------------------------------------------------------------------------------------------
# Carregando a base de dados de jogadores 
dados_gerais <- read.csv2('jogadores.csv')

# Arrumando as colunas 
dados_gerais <- dplyr::select(dados_gerais, Player, R, ACS, K.D, KAST, ADR)
row.names(dados_gerais) <- make.names(dados_gerais[,1], unique = T)
dados_gerais <- dplyr::select(dados_gerais, -Player)
dados_gerais$KAST <- parse_number(dados_gerais$KAST)

# Definindo times especificos da competição GAME CHANGERS 
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

# Média R
c9wR <- mean(c9w_df$R) 
flgcR <- mean(flgc_df$R) 
g2R <- mean(g2_df$R) 
gldxR <- mean(gldx_df$R) 
kruR <- mean(kru_df$R) 
srR <- mean(sr_df$R) 
tlR <- mean(tl_df$R) 
x10R <- mean(x10_df$R) 
# Média ACS
c9wACS <- mean(c9w_df$ACS) 
flgcACS <- mean(flgc_df$ACS) 
g2ACS <- mean(g2_df$ACS) 
gldxACS <- mean(gldx_df$ACS) 
kruACS <- mean(kru_df$ACS) 
srACS <- mean(sr_df$ACS) 
tlACS <- mean(tl_df$ACS) 
x10ACS <- mean(x10_df$ACS) 
# Média KD
c9wKD <- mean(c9w_df$K.D) 
flgcKD <- mean(flgc_df$K.D) 
g2KD <- mean(g2_df$K.D) 
gldxKD <- mean(gldx_df$K.D) 
kruKD <- mean(kru_df$K.D) 
srKD <- mean(sr_df$K.D) 
tlKD <- mean(tl_df$K.D) 
x10KD <- mean(x10_df$K.D) 
# Média KAST
c9wKAST <- mean(c9w_df$KAST) 
flgcKAST <- mean(flgc_df$KAST) 
g2KAST <- mean(g2_df$KAST) 
gldxKAST <- mean(gldx_df$KAST) 
kruKAST <- mean(kru_df$KAST) 
srKAST <- mean(sr_df$KAST) 
tlKAST <- mean(tl_df$KAST) 
x10KAST <- mean(x10_df$KAST) 
# Média ADR
c9wADR <- mean(c9w_df$ADR) 
flgcADR <- mean(flgc_df$ADR) 
g2ADR <- mean(g2_df$ADR) 
gldxADR <- mean(gldx_df$ADR) 
kruADR <- mean(kru_df$ADR) 
srADR <- mean(sr_df$ADR) 
tlADR <- mean(tl_df$ADR) 
x10ADR <- mean(x10_df$ADR) 

time1R <- c(c9wR, g2R, gldxR, flgcR, kruR, gldxR, c9wR, srR, srR, c9wR, g2R, srR, g2R, tlR)
time2R <- c(kruR, x10R, srR, tlR, x10R, flgcR, g2R, tlR, x10R, gldxR, tlR, c9wR, srR, srR)
time1ACS <- c(c9wACS, g2ACS, gldxACS, flgcACS, kruACS, gldxACS, c9wACS, srACS, srACS, c9wACS, g2ACS, srACS, g2ACS, tlACS)
time2ACS <- c(kruACS, x10ACS, srACS, tlACS, x10ACS, flgcACS, g2ACS, tlACS, x10ACS, gldxACS, tlACS, c9wACS, srACS, srACS)
time1KD <- c(c9wKD, g2KD, gldxKD, flgcKD, kruKD, gldxKD, c9wKD, srKD, srKD, c9wKD, g2KD, srKD, g2KD, tlKD)
time2KD <- c(kruKD, x10KD, srKD, tlKD, x10KD, flgcKD, g2KD, tlKD, x10KD, gldxKD, tlKD, c9wKD, srKD, srKD)
time1KAST <- c(c9wKAST, g2KAST, gldxKAST, flgcKAST, kruKAST, gldxKAST, c9wKAST, srKAST, srKAST, c9wKAST, g2KAST, srKAST, g2KAST, tlKAST)
time2KAST <- c(kruKAST, x10KAST, srKAST, tlKAST, x10KAST, flgcKAST, g2KAST, tlKAST, x10KAST, gldxKAST, tlKAST, c9wKAST, srKAST, srKAST)
time1ADR <- c(c9wADR, g2ADR, gldxADR, flgcADR, kruADR, gldxADR, c9wADR, srADR, srADR, c9wADR, g2ADR, srADR, g2ADR, tlADR)
time2ADR <- c(kruADR, x10ADR, srADR, tlADR, x10ADR, flgcADR, g2ADR, tlADR, x10ADR, gldxADR, tlADR, c9wADR, srADR, srADR)
ganhador <- c(1, 1, 0, 0, 0, 1, 0, 0, 1, 1, 1, 1, 1, 0)

jogos <- data.frame(time1R, time2R, time1ACS, time2ACS, time1KD, time2KD, time1KAST, time2KAST, time1ADR, time2ADR, ganhador)

write.csv2(jogos, 'csv/jogos2.csv')

rm(list = ls())

# CHAMPIONS TOUR SOUTH AMERICA -----------------------------------------------------------------------------

# Carregando a base de dados de jogadores 
dados_gerais <- read.csv2('jogadores.csv')

# Arrumando as colunas 
dados_gerais <- dplyr::select(dados_gerais, Player, R, ACS, K.D, KAST, ADR)
row.names(dados_gerais) <- make.names(dados_gerais[,1], unique = T)
dados_gerais <- dplyr::select(dados_gerais, -Player)
dados_gerais$KAST <- parse_number(dados_gerais$KAST)

# Definindo times especificos da competição CHAMPIONS 
#nip
nip = c('xand', 'Jonn', 'v1xen', 'bezn1', 'cauanzin') # Definindo o time 1
nip <- paste0('\\b', nip, '\\b') # Colocando '\\b' antes e dps p pegar apenas as strings exatas
dados_gerais$nip <- ifelse(grepl(paste(nip, collapse = '|'), rownames(dados_gerais), useBytes = T), 1, 0)

#keyd Gaming
keyd = c('murizzz', 'mwzera', 'rhz', 'RgLMeister', 'heat')
keyd <- paste0('\\b', keyd, '\\b') 
dados_gerais$keyd <- ifelse(grepl(paste(keyd, collapse = '|'), rownames(dados_gerais), useBytes = T), 1, 0)

#furia
furia = c('nzr', 'Quick', 'Khalil', 'Mazin', 'dgzin')
furia <- paste0('\\b', furia, '\\b')
dados_gerais$furia <- ifelse(grepl(paste(furia, collapse = '|'), rownames(dados_gerais), useBytes = T), 1, 0)
dados_gerais['Quick.1',]$furia <- 0

#TBK
tbk = c('matheuzin', 'kon4n', 'tuyz', 'luk', 'ryotzz')
tbk <- paste0('\\b', tbk, '\\b')
dados_gerais$tbk <- ifelse(grepl(paste(tbk, collapse = '|'), rownames(dados_gerais), useBytes = T), 1, 0)

#kru
kru = c('NagZ', 'keznit', 'delz1k', 'Klaus', 'Mazino')
kru <- paste0('\\b', kru, '\\b')
dados_gerais$kru <- ifelse(grepl(paste(kru, collapse = '|'), rownames(dados_gerais), useBytes = T), 1, 0)

#Fusion
fus = c('Dcop', 'sickLy', 'xander', 'Torrify', 'Mited', 'Darker')
fus <- paste0('\\b', fus, '\\b')
dados_gerais$fus <- ifelse(grepl(paste(fus, collapse = '|'), rownames(dados_gerais), useBytes = T), 1, 0)

#E-Xolos LAZER
exl = c('jfoeN', 'BandiCoot', 'DaveeyS', 'Feniz', 'Peloncito')
exl <- paste0('\\b', exl, '\\b')
dados_gerais$exl <- ifelse(grepl(paste(exl, collapse = '|'), rownames(dados_gerais), useBytes = T), 1 ,0)

#z9 Team
z9 = c('puleule', 'bnj', 'mizu', 'Tuli', 'deigara')
z9 <- paste0('\\b', z9, '\\b')
dados_gerais$z9 <- ifelse(grepl(paste(z9, collapse = '|'), rownames(dados_gerais), useBytes = T), 1, 0)

resultado <- filter(dados_gerais, dados_gerais$nip == 1 | dados_gerais$keyd == 1 | dados_gerais$furia == 1
                    | dados_gerais$tbk == 1 | dados_gerais$kru == 1 | dados_gerais$fus == 1 | 
                      dados_gerais$exl == 1 | dados_gerais$z9 == 1)

# Removendo uma jogadora que tem o mesmo de outra
while (nrow(resultado) > 41) {
  resultado <- resultado[-42,]
}

# Separando os times em dataframes
nip_df <- filter(resultado, resultado$nip == 1)
keyd_df <- filter(resultado, resultado$keyd == 1)
furia_df <- filter(resultado, resultado$furia == 1)
tbk_df <- filter(resultado, resultado$tbk == 1)
kru_df <- filter(resultado, resultado$kru == 1)
fus_df <- filter(resultado, resultado$fus == 1)
exl_df <- filter(resultado, resultado$exl == 1)
z9_df <- filter(resultado, resultado$z9 == 1)

rm(nip, keyd, furia, tbk, kru, fus, exl, z9)

# Tirando colunas de times dos dataframes especificos de cada time
nip_df <- nip_df[,-6:-13]
z9_df <- z9_df[,-6:-13]
tbk_df <- tbk_df[,-6:-13]
furia_df <- furia_df[,-6:-13]
fus_df <- fus_df[,-6:-13]
keyd_df <- keyd_df[,-6:-13]
kru_df <- kru_df[,-6:-13]
exl_df <- exl_df[,-6:-13]

# Média R
furiaR <- mean(furia_df$R) 
fusR <- mean(fus_df$R) 
keydR <- mean(keyd_df$R) 
tbkR <- mean(tbk_df$R) 
exlR <- mean(exl_df$R) 
kruR <- mean(kru_df$R) 
nipR <- mean(nip_df$R) 
z9R <- mean(z9_df$R) 
# Média ACS
furiaACS <- mean(furia_df$ACS) 
fusACS <- mean(fus_df$ACS) 
keydACS <- mean(keyd_df$ACS) 
tbkACS <- mean(tbk_df$ACS) 
exlACS <- mean(exl_df$ACS) 
kruACS <- mean(kru_df$ACS) 
nipACS <- mean(nip_df$ACS) 
z9ACS <- mean(z9_df$ACS) 
# Média KD
furiaKD <- mean(furia_df$K.D) 
fusKD <- mean(fus_df$K.D) 
keydKD <- mean(keyd_df$K.D) 
tbkKD <- mean(tbk_df$K.D) 
exlKD <- mean(exl_df$K.D) 
kruKD <- mean(kru_df$K.D) 
nipKD <- mean(nip_df$K.D) 
z9KD <- mean(z9_df$K.D) 
# Média KAST
furiaKAST <- mean(furia_df$KAST) 
fusKAST <- mean(fus_df$KAST) 
keydKAST <- mean(keyd_df$KAST) 
tbkKAST <- mean(tbk_df$KAST) 
exlKAST <- mean(exl_df$KAST) 
kruKAST <- mean(kru_df$KAST) 
nipKAST <- mean(nip_df$KAST) 
z9KAST <- mean(z9_df$KAST) 
# Média ADR
furiaADR <- mean(furia_df$ADR) 
fusADR <- mean(fus_df$ADR) 
keydADR <- mean(keyd_df$ADR) 
tbkADR <- mean(tbk_df$ADR) 
exlADR <- mean(exl_df$ADR) 
kruADR <- mean(kru_df$ADR) 
nipADR <- mean(nip_df$ADR) 
z9ADR <- mean(z9_df$ADR) 

time1R <- c(nipR, fusR, kruR, keydR, nipR, tbkR, z9R, kruR, keydR, z9R, furiaR, keydR, furiaR)
time2R <- c(z9R, furiaR, tbkR, exlR, fusR, exlR, furiaR, keydR, nipR, tbkR, kruR, tbkR, tbkR)
time1ACS <- c(nipACS, fusACS, kruACS, keydACS, nipACS, tbkACS, z9ACS, kruACS, keydACS, z9ACS, furiaACS, keydACS, furiaACS)
time2ACS <- c(z9ACS, furiaACS, tbkACS, exlACS, fusACS, exlACS, furiaACS, keydACS, nipACS, tbkACS, kruACS, tbkACS, tbkACS)
time1KD <- c(nipKD, fusKD, kruKD, keydKD, nipKD, tbkKD, z9KD, kruKD, keydKD, z9KD, furiaKD, keydKD, furiaKD)
time2KD <- c(z9KD, furiaKD, tbkKD, exlKD, fusKD, exlKD, furiaKD, keydKD, nipKD, tbkKD, kruKD, tbkKD, tbkKD)
time1KAST <- c(nipKAST, fusKAST, kruKAST, keydKAST, nipKAST, tbkKAST, z9KAST, kruKAST, keydKAST, z9KAST, furiaKAST, keydKAST, furiaKAST)
time2KAST <- c(z9KAST, furiaKAST, tbkKAST, exlKAST, fusKAST, exlKAST, furiaKAST, keydKAST, nipKAST, tbkKAST, kruKAST, tbkKAST, tbkKAST)
time1ADR <- c(nipADR, fusADR, kruADR, keydADR, nipADR, tbkADR, z9ADR, kruADR, keydADR, z9ADR, furiaADR, keydADR, furiaADR)
time2ADR <- c(z9ADR, furiaADR, tbkADR, exlADR, fusADR, exlADR, furiaADR, keydADR, nipADR, tbkADR, kruADR, tbkADR, tbkADR)
ganhador <- c(0, 0, 1, 1, 1, 1, 0, 1, 1, 0, 0, 0, 1)

jogos <- data.frame(time1R, time2R, time1ACS, time2ACS, time1KD, time2KD, time1KAST, time2KAST, time1ADR, time2ADR, ganhador)

write.csv2(jogos, 'csv/jogos3.csv')

rm(list = ls())


# CHAMPIONS TOUR NORTH AMERICA -----------------------------------------------------------------------------

# Carregando a base de dados de jogadores 
dados_gerais <- read.csv2('jogadores.csv')

# Arrumando as colunas 
dados_gerais <- dplyr::select(dados_gerais, Player, R, ACS, K.D, KAST, ADR)
row.names(dados_gerais) <- make.names(dados_gerais[,1], unique = T)
dados_gerais <- dplyr::select(dados_gerais, -Player)
dados_gerais$KAST <- parse_number(dados_gerais$KAST)

# Definindo times especificos da competição CHAMPIONS 
#The Guard
tg = c('valyn', 'Sayaplayer', 'JonahP', 'neT', 'trent') # Definindo o time 1
tg <- paste0('\\b', tg, '\\b') # Colocando '\\b' antes e dps p pegar apenas as strings exatas
dados_gerais$tg <- ifelse(grepl(paste(tg, collapse = '|'), rownames(dados_gerais), useBytes = T), 1, 0)

#FaZe Clan
fzc = c('supamen', 'POISED', 'dicey', 'BABYBAY', 'flyuh')
fzc <- paste0('\\b', fzc, '\\b') 
dados_gerais$fzc <- ifelse(grepl(paste(fzc, collapse = '|'), rownames(dados_gerais), useBytes = T), 1, 0)

#cl9
cl9 = c('mitch', 'curry', 'leaf', 'vanity', 'Xeppaa')
cl9 <- paste0('\\b', cl9, '\\b')
dados_gerais$cl9 <- ifelse(grepl(paste(cl9, collapse = '|'), rownames(dados_gerais), useBytes = T), 1, 0)
dados_gerais['leaf.1',]$cl9 <- 0

#sr
sr = c('TiGG', 'bdog', 'dazzLe', 'mada', 'moose')
sr <- paste0('\\b', sr, '\\b')
dados_gerais$sr <- ifelse(grepl(paste(sr, collapse = '|'), rownames(dados_gerais), useBytes = T), 1, 0)

#t100
t100 = c('Asuna', 'stellar', 'Will', 'Derrek', 'bang')
t100 <- paste0('\\b', t100, '\\b')
dados_gerais$t100 <- ifelse(grepl(paste(t100, collapse = '|'), rownames(dados_gerais), useBytes = T), 1, 0)

#Evil Geniuses
eg = c('Boostio', 'C0M', 'Reformed', 'jawgemo', 'Apoth')
eg <- paste0('\\b', eg, '\\b')
dados_gerais$eg <- ifelse(grepl(paste(eg, collapse = '|'), rownames(dados_gerais), useBytes = T), 1, 0)

#Sentinels
sen = c('TenZ', 'ShahZaM', 'shroud', 'dapr', 'Zellsis')
sen <- paste0('\\b', sen, '\\b')
dados_gerais$sen <- ifelse(grepl(paste(sen, collapse = '|'), rownames(dados_gerais), useBytes = T), 1 ,0)

#NRG Esports
nrg = c('hazed', 'eeiu', 'tex', 's0m', 'Ethan')
nrg <- paste0('\\b', nrg, '\\b')
dados_gerais$nrg <- ifelse(grepl(paste(nrg, collapse = '|'), rownames(dados_gerais), useBytes = T), 1, 0)

resultado <- filter(dados_gerais, dados_gerais$tg == 1 | dados_gerais$fzc == 1 | dados_gerais$cl9 == 1
                    | dados_gerais$sr == 1 | dados_gerais$t100 == 1 | dados_gerais$eg == 1 | 
                      dados_gerais$sen == 1 | dados_gerais$nrg == 1)

# Removendo uma jogadora que tem o mesmo de outra
while (nrow(resultado) > 40) {
  resultado <- resultado[-41,]
}

# Separando os times em dataframes
tg_df <- filter(resultado, resultado$tg == 1)
fzc_df <- filter(resultado, resultado$fzc == 1)
cl9_df <- filter(resultado, resultado$cl9 == 1)
sr_df <- filter(resultado, resultado$sr == 1)
t100_df <- filter(resultado, resultado$t100 == 1)
eg_df <- filter(resultado, resultado$eg == 1)
sen_df <- filter(resultado, resultado$sen == 1)
nrg_df <- filter(resultado, resultado$nrg == 1)

rm(tg, fzc, cl9, sr, t100, eg, sen, nrg)

# Tirando colunas de times dos dataframes especificos de cada time
tg_df <- tg_df[,-6:-13]
nrg_df <- nrg_df[,-6:-13]
sr_df <- sr_df[,-6:-13]
cl9_df <- cl9_df[,-6:-13]
eg_df <- eg_df[,-6:-13]
fzc_df <- fzc_df[,-6:-13]
t100_df <- t100_df[,-6:-13]
sen_df <- sen_df[,-6:-13]

# Média R
cl9R <- mean(cl9_df$R) 
egR <- mean(eg_df$R) 
fzcR <- mean(fzc_df$R) 
srR <- mean(sr_df$R) 
senR <- mean(sen_df$R) 
t100R <- mean(t100_df$R) 
tgR <- mean(tg_df$R) 
nrgR <- mean(nrg_df$R) 
# Média ACS
cl9ACS <- mean(cl9_df$ACS) 
egACS <- mean(eg_df$ACS) 
fzcACS <- mean(fzc_df$ACS) 
srACS <- mean(sr_df$ACS) 
senACS <- mean(sen_df$ACS) 
t100ACS <- mean(t100_df$ACS) 
tgACS <- mean(tg_df$ACS) 
nrgACS <- mean(nrg_df$ACS) 
# Média KD
cl9KD <- mean(cl9_df$K.D) 
egKD <- mean(eg_df$K.D) 
fzcKD <- mean(fzc_df$K.D) 
srKD <- mean(sr_df$K.D) 
senKD <- mean(sen_df$K.D) 
t100KD <- mean(t100_df$K.D) 
tgKD <- mean(tg_df$K.D) 
nrgKD <- mean(nrg_df$K.D) 
# Média KAST
cl9KAST <- mean(cl9_df$KAST) 
egKAST <- mean(eg_df$KAST) 
fzcKAST <- mean(fzc_df$KAST) 
srKAST <- mean(sr_df$KAST) 
senKAST <- mean(sen_df$KAST) 
t100KAST <- mean(t100_df$KAST) 
tgKAST <- mean(tg_df$KAST) 
nrgKAST <- mean(nrg_df$KAST) 
# Média ADR
cl9ADR <- mean(cl9_df$ADR) 
egADR <- mean(eg_df$ADR) 
fzcADR <- mean(fzc_df$ADR) 
srADR <- mean(sr_df$ADR) 
senADR <- mean(sen_df$ADR) 
t100ADR <- mean(t100_df$ADR) 
tgADR <- mean(tg_df$ADR) 
nrgADR <- mean(nrg_df$ADR) 


time1R <- c(tgR, srR, fzcR, nrgR, senR, egR, tgR, fzcR, t100R, cl9R, tgR, t100R, tgR, fzcR)
time2R <- c(senR, cl9R, egR, t100R, srR, nrgR, cl9R, t100R, senR, nrgR, fzcR, cl9R, t100R, t100R)
time1ACS <- c(tgACS, srACS, fzcACS, nrgACS, senACS, egACS, tgACS, fzcACS, t100ACS, cl9ACS, tgACS, t100ACS, tgACS, fzcACS)
time2ACS <- c(senACS, cl9ACS, egACS, t100ACS, srACS, nrgACS, cl9ACS, t100ACS, senACS, nrgACS, fzcACS, cl9ACS, t100ACS, t100ACS)
time1KD <- c(tgKD, srKD, fzcKD, nrgKD, senKD, egKD, tgKD, fzcKD, t100KD, cl9KD, tgKD, t100KD, tgKD, fzcKD)
time2KD <- c(senKD, cl9KD, egKD, t100KD, srKD, nrgKD, cl9KD, t100KD, senKD, nrgKD, fzcKD, cl9KD, t100KD, t100KD)
time1KAST <- c(tgKAST, srKAST, fzcKAST, nrgKAST, senKAST, egKAST, tgKAST, fzcKAST, t100KAST, cl9KAST, tgKAST, t100KAST, tgKAST, fzcKAST)
time2KAST <- c(senKAST, cl9KAST, egKAST, t100KAST, srKAST, nrgKAST, cl9KAST, t100KAST, senKAST, nrgKAST, fzcKAST, cl9KAST, t100KAST, t100KAST)
time1ADR <- c(tgADR, srADR, fzcADR, nrgADR, senADR, egADR, tgADR, fzcADR, t100ADR, cl9ADR, tgADR, t100ADR, tgADR, fzcADR)
time2ADR <- c(senADR, cl9ADR, egADR, t100ADR, srADR, nrgADR, cl9ADR, t100ADR, senADR, nrgADR, fzcADR, cl9ADR, t100ADR, t100ADR)
ganhador <- c(1, 0, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 0, 0)

jogos <- data.frame(time1R, time2R, time1ACS, time2ACS, time1KD, time2KD, time1KAST, time2KAST, time1ADR, time2ADR, ganhador)

write.csv2(jogos, 'csv/jogos4.csv')

rm(list = ls())


# CHAMPIONS TOUR EMEA LAST CHANCE QUALIFIER ----------------------------------------------------------------
# Carregando a base de dados de jogadores 
dados_gerais <- read.csv2('jogadores.csv')

# Arrumando as colunas 
dados_gerais <- dplyr::select(dados_gerais, Player, R, ACS, K.D, KAST, ADR)
row.names(dados_gerais) <- make.names(dados_gerais[,1], unique = T)
dados_gerais <- dplyr::select(dados_gerais, -Player)
dados_gerais$KAST <- parse_number(dados_gerais$KAST)

# Definindo times especificos da competição CHAMPIONS 
#g2
g2 = c('Mixwell', 'Meddo', 'AvovA', 'hoody', 'nukkye') # Definindo o time 1
g2 <- paste0('\\b', g2, '\\b') # Colocando '\\b' antes e dps p pegar apenas as strings exatas
dados_gerais$g2 <- ifelse(grepl(paste(g2, collapse = '|'), rownames(dados_gerais), useBytes = T), 1, 0)

#m3
m3c = c('nAts', 'Chronicle', 'Redgar', 'sheydos', 'purp0', 'Jady')
m3c <- paste0('\\b', m3c, '\\b') 
dados_gerais$m3c <- ifelse(grepl(paste(m3c, collapse = '|'), rownames(dados_gerais), useBytes = T), 1, 0)

#acend
acend = c('stax', 'Rb', 'Zest', 'BuZz', 'MaKo')
acend <- paste0('\\b', acend, '\\b')
dados_gerais$acend <- ifelse(grepl(paste(acend, collapse = '|'), rownames(dados_gerais), useBytes = T), 1, 0)

#bbl
bbl = c('Turko', 'aimDLL', 'AsLanM4shadoW', 'QutionerX', 'CyderX')
bbl <- paste0('\\b', bbl, '\\b')
dados_gerais$bbl <- ifelse(grepl(paste(bbl, collapse = '|'), rownames(dados_gerais), useBytes = T), 1, 0)

#TL
tl = c('ScreaM', 'soulcas', 'dimasick', 'Jamppi', 'Nivera')
tl <- paste0('\\b', tl, '\\b')
dados_gerais$tl <- ifelse(grepl(paste(tl, collapse = '|'), rownames(dados_gerais), useBytes = T), 1, 0)

#Natus Vincere
navi = c('dinkzj', 'Duno', '7ssk7', 'zeddy', 'Cloud')
navi <- paste0('\\b', navi, '\\b')
dados_gerais$navi <- ifelse(grepl(paste(navi, collapse = '|'), rownames(dados_gerais), useBytes = T), 1, 0)

#OG LDN UTD
ldn = c('hype', 'Boo', 'MOLSI', 'Destrian', 'feqew')
ldn <- paste0('\\b', ldn, '\\b')
dados_gerais$ldn <- ifelse(grepl(paste(ldn, collapse = '|'), rownames(dados_gerais), useBytes = T), 1 ,0)

#Guild Esports
ge = c('Yacine', 'Sayf', 'koldamenta', 'Leo', 'trexx')
ge <- paste0('\\b', ge, '\\b')
dados_gerais$ge <- ifelse(grepl(paste(ge, collapse = '|'), rownames(dados_gerais), useBytes = T), 1, 0)

resultado <- filter(dados_gerais, dados_gerais$g2 == 1 | dados_gerais$m3c == 1 | dados_gerais$acend == 1
                    | dados_gerais$bbl == 1 | dados_gerais$tl == 1 | dados_gerais$navi == 1 | 
                      dados_gerais$ldn == 1 | dados_gerais$ge == 1)

# Removendo uma jogadora que tem o mesmo de outra
while (nrow(resultado) > 40) {
  resultado <- resultado[-41,]
}

# Separando os times em dataframes
g2_df <- filter(resultado, resultado$g2 == 1)
m3c_df <- filter(resultado, resultado$m3c == 1)
acend_df <- filter(resultado, resultado$acend == 1)
bbl_df <- filter(resultado, resultado$bbl == 1)
tl_df <- filter(resultado, resultado$tl == 1)
navi_df <- filter(resultado, resultado$navi == 1)
ldn_df <- filter(resultado, resultado$ldn == 1)
ge_df <- filter(resultado, resultado$ge == 1)

rm(g2, m3c, acend, bbl, tl, navi, ldn, ge)

# Tirando colunas de times dos dataframes especificos de cada time
g2_df <- g2_df[,-6:-13]
ge_df <- ge_df[,-6:-13]
bbl_df <- bbl_df[,-6:-13]
acend_df <- acend_df[,-6:-13]
navi_df <- navi_df[,-6:-13]
m3c_df <- m3c_df[,-6:-13]
tl_df <- tl_df[,-6:-13]
ldn_df <- ldn_df[,-6:-13]

# Média R (Rating)
acendR <- mean(acend_df$R) 
naviR <- mean(navi_df$R) 
m3cR <- mean(m3c_df$R) 
bblR <- mean(bbl_df$R) 
ldnR <- mean(ldn_df$R) 
tlR <- mean(tl_df$R) 
g2R <- mean(g2_df$R) 
geR <- mean(ge_df$R) 
# Média ACS
acendACS <- mean(acend_df$ACS) 
naviACS <- mean(navi_df$ACS) 
m3cACS <- mean(m3c_df$ACS) 
bblACS <- mean(bbl_df$ACS) 
ldnACS <- mean(ldn_df$ACS) 
tlACS <- mean(tl_df$ACS) 
g2ACS <- mean(g2_df$ACS) 
geACS <- mean(ge_df$ACS) 
# Média KD
acendKD <- mean(acend_df$K.D) 
naviKD <- mean(navi_df$K.D) 
m3cKD <- mean(m3c_df$K.D) 
bblKD <- mean(bbl_df$K.D) 
ldnKD <- mean(ldn_df$K.D) 
tlKD <- mean(tl_df$K.D) 
g2KD <- mean(g2_df$K.D) 
geKD <- mean(ge_df$K.D) 
# Média KAST
acendKAST <- mean(acend_df$KAST) 
naviKAST <- mean(navi_df$KAST) 
m3cKAST <- mean(m3c_df$KAST) 
bblKAST <- mean(bbl_df$KAST) 
ldnKAST <- mean(ldn_df$KAST) 
tlKAST <- mean(tl_df$KAST) 
g2KAST <- mean(g2_df$KAST) 
geKAST <- mean(ge_df$KAST) 
# Média ADR
acendADR <- mean(acend_df$ADR) 
naviADR <- mean(navi_df$ADR) 
m3cADR <- mean(m3c_df$ADR) 
bblADR <- mean(bbl_df$ADR) 
ldnADR <- mean(ldn_df$ADR) 
tlADR <- mean(tl_df$ADR) 
g2ADR <- mean(g2_df$ADR) 
geADR <- mean(ge_df$ADR) 
#Criando o dataframe
time1R <- c(g2R, m3cR, tlR, geR, g2R, bblR, ldnR, tlR, naviR, ldnR, m3cR, g2R, m3cR, tlR)
time2R <- c(ldnR, acendR, bblR, naviR, acendR, geR, m3cR, naviR, g2R, geR, tlR, ldnR, tlR, g2R)
time1ACS <- c(g2ACS, m3cACS, tlACS, geACS, g2ACS, bblACS, ldnACS, tlACS, naviACS, ldnACS, m3cACS, g2ACS, m3cACS, tlACS)
time2ACS <- c(ldnACS, acendACS, bblACS, naviACS, acendACS, geACS, m3cACS, naviACS, g2ACS, geACS, tlACS, ldnACS, tlACS, g2ACS)
time1KD <- c(g2KD, m3cKD, tlKD, geKD, g2KD, bblKD, ldnKD, tlKD, naviKD, ldnKD, m3cKD, g2KD, m3cKD, tlKD)
time2KD <- c(ldnKD, acendKD, bblKD, naviKD, acendKD, geKD, m3cKD, naviKD, g2KD, geKD, tlKD, ldnKD, tlKD, g2KD)
time1KAST <- c(g2KAST, m3cKAST, tlKAST, geKAST, g2KAST, bblKAST, ldnKAST, tlKAST, naviKAST, ldnKAST, m3cKAST, g2KAST, m3cKAST, tlKAST)
time2KAST <- c(ldnKAST, acendKAST, bblKAST, naviKAST, acendKAST, geKAST, m3cKAST, naviKAST, g2KAST, geKAST, tlKAST, ldnKAST, tlKAST, g2KAST)
time1ADR <- c(g2ADR, m3cADR, tlADR, geADR, g2ADR, bblADR, ldnADR, tlADR, naviADR, ldnADR, m3cADR, g2ADR, m3cADR, tlADR)
time2ADR <- c(ldnADR, acendADR, bblADR, naviADR, acendADR, geADR, m3cADR, naviADR, g2ADR, geADR, tlADR, ldnADR, tlADR, g2ADR)

ganhador <- c(0, 1, 1, 0, 1, 0, 0, 1, 0, 1, 1, 1, 0, 1)

jogos <- data.frame(time1R, time2R, time1ACS, time2ACS, time1KD, time2KD, time1KAST, time2KAST, time1ADR, time2ADR, ganhador)

write.csv2(jogos, 'csv/jogos5.csv')

rm(list = ls())

# CHAMPIONS TOUR EAST ASIA LAST CHANCE QUALIFIER -----------------------------------------------------------
# Carregando a base de dados de jogadores 
dados_gerais <- read.csv2('jogadores.csv')

# Arrumando as colunas 
dados_gerais <- dplyr::select(dados_gerais, Player, R, ACS, K.D, KAST, ADR)
row.names(dados_gerais) <- make.names(dados_gerais[,1], unique = T)
dados_gerais <- dplyr::select(dados_gerais, -Player)
dados_gerais$KAST <- parse_number(dados_gerais$KAST)

# Definindo times especificos da competição CHAMPIONS 
#Maru Gaming
mg = c('Chibab', 'Jeong Hi', 'WIX', 'Moves', 'NakJi') # Definindo o time 1
mg <- paste0('\\b', mg, '\\b') # Colocando '\\b' antes e dps p pegar apenas as strings exatas
dados_gerais$mg <- ifelse(grepl(paste(mg, collapse = '|'), rownames(dados_gerais), useBytes = T), 1, 0)

#DAMWON Gaming
dwgc = c('exy', 'Lakia', 'Esperanza', 'allow', 't3xture')
dwgc <- paste0('\\b', dwgc, '\\b') 
dados_gerais$dwgc <- ifelse(grepl(paste(dwgc, collapse = '|'), rownames(dados_gerais), useBytes = T), 1, 0)

#edg
edg = c('nobody', 'Life', 'ZmjjKK', 'Haodong', 'CHICHOO')
edg <- paste0('\\b', edg, '\\b')
dados_gerais$edg <- ifelse(grepl(paste(edg, collapse = '|'), rownames(dados_gerais), useBytes = T), 1, 0)

#s2
s2 = c('Bazzi', 'TS', 'eKo', 'GodDead', 'Estrella')
s2 <- paste0('\\b', s2, '\\b')
dados_gerais$s2 <- ifelse(grepl(paste(s2, collapse = '|'), rownames(dados_gerais), useBytes = T), 1, 0)

#nth
nth = c('Derialy', 'JoXJo', 'xnfri', 'BlackWiz', 'Meteor')
nth <- paste0('\\b', nth, '\\b')
dados_gerais$nth <- ifelse(grepl(paste(nth, collapse = '|'), rownames(dados_gerais), useBytes = T), 1, 0)

#cr
cr = c('rion', 'neth', 'popogachi', 'Meiy', 'Astell')
cr <- paste0('\\b', cr, '\\b')
dados_gerais$cr <- ifelse(grepl(paste(cr, collapse = '|'), rownames(dados_gerais), useBytes = T), 1, 0)

#Reject
rc = c('iNTRO', 'Medusa', 'takej', 'Reita', 'Anthem')
rc <- paste0('\\b', rc, '\\b')
dados_gerais$rc <- ifelse(grepl(paste(rc, collapse = '|'), rownames(dados_gerais), useBytes = T), 1 ,0)

#Guild Esports
kone = c('LuoK1ng', 'sword9', 'Ninebody', 'Knight', 'Yosemite')
kone <- paste0('\\b', kone, '\\b')
dados_gerais$kone <- ifelse(grepl(paste(kone, collapse = '|'), rownames(dados_gerais), useBytes = T), 1, 0)

resultado <- filter(dados_gerais, dados_gerais$mg == 1 | dados_gerais$dwgc == 1 | dados_gerais$edg == 1
                    | dados_gerais$s2 == 1 | dados_gerais$nth == 1 | dados_gerais$cr == 1 | 
                      dados_gerais$rc == 1 | dados_gerais$kone == 1)

# Removendo uma jogadora que tem o mesmo de outra
while (nrow(resultado) > 40) {
  resultado <- resultado[-41,]
}

# Separando os times em dataframes
mg_df <- filter(resultado, resultado$mg == 1)
dwgc_df <- filter(resultado, resultado$dwgc == 1)
edg_df <- filter(resultado, resultado$edg == 1)
s2_df <- filter(resultado, resultado$s2 == 1)
nth_df <- filter(resultado, resultado$nth == 1)
cr_df <- filter(resultado, resultado$cr == 1)
rc_df <- filter(resultado, resultado$rc == 1)
kone_df <- filter(resultado, resultado$kone == 1)

rm(mg, dwgc, edg, s2, nth, cr, rc, kone)

# Tirando colunas de times dos dataframes especificos de cada time
mg_df <- mg_df[,-6:-13]
kone_df <- kone_df[,-6:-13]
s2_df <- s2_df[,-6:-13]
edg_df <- edg_df[,-6:-13]
cr_df <- cr_df[,-6:-13]
dwgc_df <- dwgc_df[,-6:-13]
nth_df <- nth_df[,-6:-13]
rc_df <- rc_df[,-6:-13]

# Média R (Rating)
edgR <- mean(edg_df$R) 
crR <- mean(cr_df$R) 
dwgcR <- mean(dwgc_df$R) 
s2R <- mean(s2_df$R) 
rcR <- mean(rc_df$R) 
nthR <- mean(nth_df$R) 
mgR <- mean(mg_df$R) 
koneR <- mean(kone_df$R) 
# Média ACS
edgACS <- mean(edg_df$ACS) 
crACS <- mean(cr_df$ACS) 
dwgcACS <- mean(dwgc_df$ACS) 
s2ACS <- mean(s2_df$ACS) 
rcACS <- mean(rc_df$ACS) 
nthACS <- mean(nth_df$ACS) 
mgACS <- mean(mg_df$ACS) 
koneACS <- mean(kone_df$ACS) 
# Média KD
edgKD <- mean(edg_df$K.D) 
crKD <- mean(cr_df$K.D) 
dwgcKD <- mean(dwgc_df$K.D) 
s2KD <- mean(s2_df$K.D) 
rcKD <- mean(rc_df$K.D) 
nthKD <- mean(nth_df$K.D) 
mgKD <- mean(mg_df$K.D) 
koneKD <- mean(kone_df$K.D) 
# Média KAST
edgKAST <- mean(edg_df$KAST) 
crKAST <- mean(cr_df$KAST) 
dwgcKAST <- mean(dwgc_df$KAST) 
s2KAST <- mean(s2_df$KAST) 
rcKAST <- mean(rc_df$KAST) 
nthKAST <- mean(nth_df$KAST) 
mgKAST <- mean(mg_df$KAST) 
koneKAST <- mean(kone_df$KAST) 
# Média ADR
edgADR <- mean(edg_df$ADR) 
crADR <- mean(cr_df$ADR) 
dwgcADR <- mean(dwgc_df$ADR) 
s2ADR <- mean(s2_df$ADR) 
rcADR <- mean(rc_df$ADR) 
nthADR <- mean(nth_df$ADR) 
mgADR <- mean(mg_df$ADR) 
koneADR <- mean(kone_df$ADR) 
#Criando o dataframe
time1R <- c(edgR, mgR, crR, nthR, s2R, dwgcR, edgR, crR, crR, rcR, edgR, s2R, edgR, nthR)
time2R <- c(s2R, rcR, dwgcR, koneR, mgR, koneR, rcR, nthR, s2R, koneR, nthR, koneR, s2R, s2R)
time1ACS <- c(edgACS, mgACS, crACS, nthACS, s2ACS, dwgcACS, edgACS, crACS, crACS, rcACS, edgACS, s2ACS, edgACS, nthACS)
time2ACS <- c(s2ACS, rcACS, dwgcACS, koneACS, mgACS, koneACS, rcACS, nthACS, s2ACS, koneACS, nthACS, koneACS, s2ACS, s2ACS)
time1KD <- c(edgKD, mgKD, crKD, nthKD, s2KD, dwgcKD, edgKD, crKD, crKD, rcKD, edgKD, s2KD, edgKD, nthKD)
time2KD <- c(s2KD, rcKD, dwgcKD, koneKD, mgKD, koneKD, rcKD, nthKD, s2KD, koneKD, nthKD, koneKD, s2KD, s2KD)
time1KAST <- c(edgKAST, mgKAST, crKAST, nthKAST, s2KAST, dwgcKAST, edgKAST, crKAST, crKAST, rcKAST, edgKAST, s2KAST, edgKAST, nthKAST)
time2KAST <- c(s2KAST, rcKAST, dwgcKAST, koneKAST, mgKAST, koneKAST, rcKAST, nthKAST, s2KAST, koneKAST, nthKAST, koneKAST, s2KAST, s2KAST)
time1ADR <- c(edgADR, mgADR, crADR, nthADR, s2ADR, dwgcADR, edgADR, crADR, crADR, rcADR, edgADR, s2ADR, edgADR, nthADR)
time2ADR <- c(s2ADR, rcADR, dwgcADR, koneADR, mgADR, koneADR, rcADR, nthADR, s2ADR, koneADR, nthADR, koneADR, s2ADR, s2ADR)


ganhador <- c(1, 0, 1, 1, 1, 0, 1, 0, 0, 0, 1, 1, 1, 0)

jogos <- data.frame(time1R, time2R, time1ACS, time2ACS, time1KD, time2KD, time1KAST, time2KAST, time1ADR, time2ADR, ganhador)

write.csv2(jogos, 'csv/jogos6.csv')

rm(list = ls())


# CHAMPIONS TOUR ~~ Preciso pegar mais algumas comeptições
# União dos dataframes -------------------------------------------------------------------------------------
jogos1 <- read.csv2('csv/jogos1.csv') %>% dplyr::select(-X)
jogos2 <- read.csv2('csv/jogos2.csv') %>% dplyr::select(-X)
jogos3 <- read.csv2('csv/jogos3.csv') %>% dplyr::select(-X)
jogos4 <- read.csv2('csv/jogos4.csv') %>% dplyr::select(-X)
jogos5 <- read.csv2('csv/jogos5.csv') %>% dplyr::select(-X)
jogos6 <- read.csv2('csv/jogos6.csv') %>% dplyr::select(-X)
jogos <- rbind(jogos1, jogos2, jogos3, jogos4, jogos5, jogos6)
jogos$ganhador <- gsub('0', '2', jogos$ganhador)
rm(jogos1, jogos2, jogos3, jogos4, jogos5, jogos6)
write.csv2(jogos, 'csv/jogos.csv')
