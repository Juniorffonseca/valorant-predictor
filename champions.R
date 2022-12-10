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
dados_gerais <- dados_gerais[,-1]
dados_gerais <- dados_gerais[,-2]
row.names(dados_gerais) <- make.names(dados_gerais[,1], unique = T)
dados_gerais <- dados_gerais[,-1]

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
resultado <- resultado[-41,]

# Calculando IDC (variancia de KAST entre os jogadores de cada time)
loud_df <- filter(resultado, resultado$loud == 1)
op_df <- filter(resultado, resultado$op == 1)
drx_df <- filter(resultado, resultado$drx == 1)
xset_df <- filter(resultado, resultado$xset == 1)
tl_df <- filter(resultado, resultado$tl == 1)
lev_df <- filter(resultado, resultado$lev == 1)
fpx_df <- filter(resultado, resultado$fpx == 1)
fntc_df <- filter(resultado, resultado$fntc == 1)
idc_t1 <- ineq(loud_df$KAST, type = 'Gini')
idc_t2 <- ineq(op_df$KAST, type = 'Gini')
idc_t3 <- ineq(drx_df$KAST, type = 'Gini')
idc_t4 <- ineq(xset_df$KAST, type = 'Gini')
idc_t5 <- ineq(tl_df$KAST, type = 'Gini')
idc_t6 <- ineq(lev_df$KAST, type = 'Gini')
idc_t7 <- ineq(fpx_df$KAST, type = 'Gini')
idc_t8 <- ineq(fntc_df$KAST, type = 'Gini')


# Colocando o indice de Gini em cada jogador para seu respectivo time
loud_df$idc <- idc_t1
op_df$idc <- idc_t2
drx_df$idc <- idc_t3
xset_df$idc <- idc_t4
tl_df$idc <- idc_t5
lev_df$idc <- idc_t6
fpx_df$idc <- idc_t7
fntc_df$idc <- idc_t8

# Removendo as variaveis idc_tn e times
rm(idc_t1, idc_t2, idc_t3, idc_t4, idc_t5, idc_t6, idc_t7, idc_t8)
rm(loud, op, drx, xset, tl, lev, fpx, fntc)

# Colocando os indices de gini no dataframe 'resultado'
resultado <- cbind(loud_df, op_df, drx_df, xset_df, tl_df, lev_df, fpx_df, fntc_df)
resultado <- merge(loud_df, op_df, all = T) %>%  
  merge(drx_df, all = T) %>%  
  merge(xset_df, all = T) %>% 
  merge(tl_df, all = T) %>% 
  merge(lev_df, all = T) %>% 
  merge(fpx_df, all = T) %>% 
  merge(fntc_df, all = T)

# Tirando colunas de times dos dataframes especificos de cada time
loud_df <- loud_df[,-7:-14]
fntc_df <- fntc_df[,-7:-14]
xset_df <- xset_df[,-7:-14]
drx_df <- drx_df[,-7:-14]
lev_df <- lev_df[,-7:-14]
op_df <- op_df[,-7:-14]
tl_df <- tl_df[,-7:-14]
fpx_df <- fpx_df[,-7:-14]

# Tentando uma formula para dizer a porcentagem de chance de vitória do time 1 sobre o time 2 -----------------------
medialoud <- mean(loud_df$R) + 1/mean(loud_df$idc)

medialev <- mean(lev_df$R) + 1/mean(lev_df$idc)

mediaxset <- mean(xset_df$R) + 1/mean(xset_df$idc)

mediafpx <- mean(fpx_df$R) + 1/mean(fpx_df$idc)

mediadrx <- mean(drx_df$R) + 1/mean(drx_df$idc)

mediaop <- mean(op_df$R) + 1/mean(op_df$idc)

mediafntc <- mean(fntc_df$R) + 1/mean(fntc_df$idc)

mediatl <- mean(tl_df$R) + 1/mean(tl_df$idc)

# Porcentagem de vitória
jogo1 <- round(mediadrx / (mediadrx + mediafpx), 3) 
jogo2 <- round(medialev / (medialev + medialoud), 3) 
jogo3 <- round(mediaop / (mediaop + mediatl), 3) 
jogo4 <- round(mediaxset / (mediaxset + mediafntc), 3) 
jogo5 <- round(mediafpx / (mediafpx + medialev), 3) 
jogo6 <- round(mediatl / (mediatl + mediafntc), 3) 
jogo7 <- round(mediadrx / (mediadrx + medialoud), 3) 
jogo8 <- round(mediaop / (mediaop + mediaxset), 6)
jogo9 <- round(mediaxset / (mediaxset + mediafpx), 3) 
jogo10 <- round(mediadrx / (mediadrx + mediafntc), 3)
jogo11 <- round(medialoud / (medialoud + mediaop), 3)
jogo12 <- round(mediafpx / (mediafpx + mediadrx), 3) 
jogo13 <- round(mediaxloud / (medialoud + mediaop), 3) 
jogo14 <- round(mediaop / (mediaop + mediadrx), 6) 


acertos = 0

analisa_resultados = function(jogo1, jogo2, jogo3, jogo4, jogo5, jogo6, jogo7, jogo8, jogo9, jogo10, jogo11, jogo12,
                              jogo13, jogo14){
  if(jogo1 > 0.50){
    acertos = acertos + 1
  }
  if(jogo2 < 0.50){
    acertos = acertos + 1
  }
  if(jogo3 > 0.50){
    acertos = acertos + 1
  }
  if(jogo4 > 0.50){
    acertos = acertos + 1
  }
  if(jogo5 > 0.50){
    acertos = acertos + 1
  }
  if(jogo6 < 0.50){
    acertos = acertos + 1
  }
  if(jogo7 < 0.50){
    acertos = acertos + 1
  }
  if(jogo8 > 0.50){
    acertos = acertos + 1
  }
  if(jogo9 < 0.50){
    acertos = acertos + 1
  }
  if(jogo10 > 0.50){
    acertos = acertos + 1
  }
  if(jogo11 < 0.50){
    acertos = acertos + 1
  }
  if(jogo12 < 0.50){
    acertos = acertos + 1
  }
  if(jogo13 > 0.50){
    acertos = acertos + 1
  }
  if(jogo14 > 0.50){
    acertos = acertos + 1
  }
  return(acertos/14)
}


analisa_resultados(jogo1, jogo2, jogo3, jogo4, jogo5, jogo6, jogo7, jogo8, jogo9, jogo10, jogo11,
                   jogo12, jogo13, jogo14)


# Primeiro teste: 42,85 % de acurácia (6/14)
# Segundo teste: 05714286 % de acurácia (8/14)
# Terceiro teste: 0.6428571 % de acurácia (9/14)

# Tentando mesclar dataframe ds_adversarios com outras estatisticas

loud_lev = 0

loud_lev <- sum(ds_adversarios$Adversario == 'Leviatán' & ds_adversarios$Resultados == 'Win')
loud_xset <- sum(ds_adversarios$Adversario == '')

