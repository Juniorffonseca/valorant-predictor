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
dados_gerais <- select(dados_gerais, Player, R, ACS, K.D, KAST, ADR)
row.names(dados_gerais) <- make.names(dados_gerais[,1], unique = T)
dados_gerais <- select(dados_gerais, -Player)
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


# GAME CHANGERS ----------------------------------------------------------------------------------------------
# Carregando a base de dados de jogadores 
dados_gerais <- read.csv2('jogadores.csv')

# Arrumando as colunas 
dados_gerais <- select(dados_gerais, Player, R, ACS, K.D, KAST, ADR)
row.names(dados_gerais) <- make.names(dados_gerais[,1], unique = T)
dados_gerais <- select(dados_gerais, -Player)
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

#
c9wR <- mean(c9w_df$R)
flgcR <- mean(flgc_df$R)
g2R <- mean(g2_df$R)
gldxR <- mean(gldx_df$R)
kruR <- mean(kru_df$R)
srR <- mean(sr_df$R)
tlR <- mean(tl_df$R)
x10R <- mean(x10_df$R)

time1 <- c(c9wR, g2R, gldxR, flgcR, kruR, gldxR, c9wR, srR, srR, c9wR, g2R, srR, g2R, tlR)
time2 <- c(kruR, x10R, srR, tlR, x10R, flgcR, g2R, tlR, x10R, gldxR, tlR, c9wR, srR, srR)
ganhador <- c(1, 1, 2, 2, 2, 1, 2, 2, 1, 1, 1, 1, 1, 2)

jogos <- data.frame(time1, time2, ganhador)

rm(drxR, levR, opR, xsetR, fpxR, tlR, loudR, fntcR, time1, time2, ganhador)

write.csv2(jogos, 'jogos2.csv')

rm(list = ls())

# CHAMPIONS TOUR SOUTH AMERICA -----------------------------------------------------------------------------

# Carregando a base de dados de jogadores 
dados_gerais <- read.csv2('jogadores.csv')

# Arrumando as colunas 
dados_gerais <- select(dados_gerais, Player, R, ACS, K.D, KAST, ADR)
row.names(dados_gerais) <- make.names(dados_gerais[,1], unique = T)
dados_gerais <- select(dados_gerais, -Player)
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

#
furiaR <- mean(furia_df$R)
fusR <- mean(fus_df$R)
keydR <- mean(keyd_df$R)
tbkR <- mean(tbk_df$R)
exlR <- mean(exl_df$R)
kruR <- mean(kru_df$R)
nipR <- mean(nip_df$R)
z9R <- mean(z9_df$R)

time1 <- c(nipR, fusR, kruR, keydR, nipR, tbkR, z9R, kruR, keydR, z9R, furiaR, keydR, furiaR)
time2 <- c(z9R, furiaR, tbkR, exlR, fusR, exlR, furiaR, keydR, nipR, tbkR, kruR, tbkR, tbkR)
ganhador <- c(2, 2, 1, 1, 1, 1, 2, 1, 1, 2, 2, 2, 1)

jogos <- data.frame(time1, time2, ganhador)

rm(furiaR, fusR, keydR, tbkR, exlR, kruR, nipR, z9R, time1, time2, ganhador)

write.csv2(jogos, 'jogos3.csv')

rm(list = ls())


# CHAMPIONS TOUR NORTH AMERICA -----------------------------------------------------------------------------

# Carregando a base de dados de jogadores 
dados_gerais <- read.csv2('jogadores.csv')

# Arrumando as colunas 
dados_gerais <- select(dados_gerais, Player, R, ACS, K.D, KAST, ADR)
row.names(dados_gerais) <- make.names(dados_gerais[,1], unique = T)
dados_gerais <- select(dados_gerais, -Player)
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

#
cl9R <- mean(cl9_df$R)
egR <- mean(eg_df$R)
fzcR <- mean(fzc_df$R)
srR <- mean(sr_df$R)
senR <- mean(sen_df$R)
t100R <- mean(t100_df$R)
tgR <- mean(tg_df$R)
nrgR <- mean(nrg_df$R)

time1 <- c(tgR, srR, fzcR, nrgR, senR, egR, tgR, fzcR, t100R, cl9R, tgR, t100R, tgR, fzcR)
time2 <- c(senR, cl9R, egR, t100R, srR, nrgR, cl9R, t100R, senR, nrgR, fzcR, cl9R, t100R, t100R)
ganhador <- c(1, 2, 1, 2, 1, 2, 1, 1, 1, 1, 1, 1, 2, 2)

jogos <- data.frame(time1, time2, ganhador)

rm(cl9R, egR, fzcR, srR, senR, t100R, tgR, nrgR, time1, time2, ganhador)

write.csv2(jogos, 'jogos4.csv')

rm(list = ls())


# União dos dataframes -------------------------------------------------------------------------------------
jogos1 <- read.csv2('jogos.csv') %>% select(-X)
jogos2 <- read.csv2('jogos2.csv') %>% select(-X)
jogos3 <- read.csv2('jogos3.csv') %>% select(-X)
jogos4 <- read.csv2('jogos4.csv') %>% select(-X)
jogos <- rbind(jogos1, jogos2, jogos3, jogos4)
jogos$ganhador <- as.factor(jogos$ganhador)
