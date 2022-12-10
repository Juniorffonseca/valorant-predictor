# Carregando pacotes ---------------------------------------------------------------------------------------
library(tidyverse)
library(FactoMineR)
library(factoextra)
library(cluster)
library(xlsx)
library(ineq)
library(stringr)
library(dplyr)

# Carregando a base de dados de jogadores ---------------------------------------------------------------
dados_gerais <- read.csv2('dados_gerais.csv')

# Arrumando as colunas ----------------------------------------------------------------------------------
dados_gerais <- select(dados_gerais, -X, -Team)
row.names(dados_gerais) <- make.names(dados_gerais[,1], unique = T)
dados_gerais <- select(dados_gerais, -Player)

# Definindo times especificos ---------------------------------------------------------------------------
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

# Calculando IDC (variancia de KAST entre os jogadores de cada time)
c9w_df <- filter(resultado, resultado$c9w == 1)
sr_df <- filter(resultado, resultado$sr == 1)
gldx_df <- filter(resultado, resultado$gldx == 1)
g2_df <- filter(resultado, resultado$g2 == 1)
tl_df <- filter(resultado, resultado$tl == 1)
kru_df <- filter(resultado, resultado$kru == 1)
x10_df <- filter(resultado, resultado$x10 == 1)
flgc_df <- filter(resultado, resultado$flgc == 1)
idc_t1 <- ineq(c9w_df$KAST, type = 'Gini')
idc_t2 <- ineq(sr_df$KAST, type = 'Gini')
idc_t3 <- ineq(gldx_df$KAST, type = 'Gini')
idc_t4 <- ineq(g2_df$KAST, type = 'Gini')
idc_t5 <- ineq(tl_df$KAST, type = 'Gini')
idc_t6 <- ineq(kru_df$KAST, type = 'Gini')
idc_t7 <- ineq(x10_df$KAST, type = 'Gini')
idc_t8 <- ineq(flgc_df$KAST, type = 'Gini')


# Colocando o indice de Gini em cada jogador para seu respectivo time
c9w_df$idc <- idc_t1
sr_df$idc <- idc_t2
gldx_df$idc <- idc_t3
g2_df$idc <- idc_t4
tl_df$idc <- idc_t5
kru_df$idc <- idc_t6
x10_df$idc <- idc_t7
flgc_df$idc <- idc_t8

# Removendo as variaveis idc_tn e times
rm(idc_t1, idc_t2, idc_t3, idc_t4, idc_t5, idc_t6, idc_t7, idc_t8)
rm(c9w, sr, gldx, g2, tl, kru, x10, flgc)

# Colocando os indices de gini no dataframe 'resultado'
resultado <- cbind(c9w_df, sr_df, gldx_df, g2_df, tl_df, kru_df, x10_df, flgc_df)
resultado <- merge(c9w_df, sr_df, all = T) %>%  
  merge(gldx_df, all = T) %>%  
  merge(g2_df, all = T) %>% 
  merge(tl_df, all = T) %>% 
  merge(kru_df, all = T) %>% 
  merge(x10_df, all = T) %>% 
  merge(flgc_df, all = T)

# Tirando colunas de times dos dataframes especificos de cada time
c9w_df <- c9w_df[,-7:-14]
flgc_df <- flgc_df[,-7:-14]
g2_df <- g2_df[,-7:-14]
gldx_df <- gldx_df[,-7:-14]
kru_df <- kru_df[,-7:-14]
sr_df <- sr_df[,-7:-14]
tl_df <- tl_df[,-7:-14]
x10_df <- x10_df[,-7:-14]

# Criando uma formula para dizer a porcentagem de chance de vitória do time 1 sobre o time 2 ----------------------
# Calculando índices dos times
mediac9w <- mean(c9w_df$R) + 1/mean(c9w_df$idc)

mediakru <- mean(kru_df$R) + 1/mean(kru_df$idc)

mediag2 <- mean(g2_df$R) + 1/mean(g2_df$idc)

mediax10 <- mean(x10_df$R) + 1/mean(x10_df$idc)

mediagldx <- mean(gldx_df$R) + 1/mean(gldx_df$idc)

mediasr <- mean(sr_df$R) + 1/mean(sr_df$idc)

mediaflgc <- mean(flgc_df$R) + 1/mean(flgc_df$idc)

mediatl <- mean(tl_df$R) + 1/mean(tl_df$idc)

# Porcentagem de vitória
jogo1 <- round(mediac9w / (mediac9w + mediakru), 2) 
jogo2 <- round(mediag2 / (mediag2 + mediax10), 3)
jogo3 <- round(mediagldx / (mediagldx + mediasr), 2) 
jogo4 <- round(mediaflgc / (mediaflgc + mediatl), 2) 
jogo5 <- round(mediakru / (mediakru + mediax10), 2)
jogo6 <- round(mediagldx / (mediagldx + mediaflgc), 2) 
jogo7 <- round(mediac9w / (mediac9w + mediag2), 2) 
jogo8 <- round(mediasr / (mediasr + mediatl), 2) 
jogo9 <- round(mediasr / (mediasr + mediax10), 2) 
jogo10 <- round(mediac9w / (mediac9w + mediagldx), 2) 
jogo11 <- round(mediag2 / (mediag2 + mediatl), 2) 
jogo12 <- round(mediasr / (mediasr + mediac9w), 2) 
jogo13 <- round(mediag2 / (mediag2 + mediasr), 2) 
jogo14 <- round(mediatl / (mediatl + mediasr), 2) 

acertos = 0

analisa_resultados = function(jogo1, jogo2, jogo3, jogo4, jogo5, jogo6, jogo7, jogo8, jogo9, jogo10, jogo11, jogo12,
                              jogo13, jogo14){
  if(jogo1 > 0.50){
    acertos = acertos + 1
  }
  if(jogo2 > 0.50){
    acertos = acertos + 1
  }
  if(jogo3 < 0.50){
    acertos = acertos + 1
  }
  if(jogo4 < 0.50){
    acertos = acertos + 1
  }
  if(jogo5 < 0.50){
    acertos = acertos + 1
  }
  if(jogo6 > 0.50){
    acertos = acertos + 1
  }
  if(jogo7 < 0.50){
    acertos = acertos + 1
  }
  if(jogo8 < 0.50){
    acertos = acertos + 1
  }
  if(jogo9 > 0.50){
    acertos = acertos + 1
  }
  if(jogo10 > 0.50){
    acertos = acertos + 1
  }
  if(jogo11 > 0.50){
    acertos = acertos + 1
  }
  if(jogo12 > 0.50){
    acertos = acertos + 1
  }
  if(jogo13 > 0.50){
    acertos = acertos + 1
  }
  if(jogo14 < 0.50){
    acertos = acertos + 1
  }
  return(acertos/14)
}


analisa_resultados(jogo1, jogo2, jogo3, jogo4, jogo5, jogo6, jogo7, jogo8, jogo9, jogo10, jogo11,
                   jogo12, jogo13, jogo14)

# Primeiro teste: 64.28571% de acuracia
  


hist <- c(jogo1, jogo2, jogo3, jogo4, jogo5, jogo6, jogo7, jogo8, jogo9, jogo10, jogo11, jogo12, jogo13, jogo14)

hist(hist, breaks = 10)
