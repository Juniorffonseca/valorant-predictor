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
loud_df <- loud_df[,-6:-13]
fntc_df <- fntc_df[,-6:-13]
xset_df <- xset_df[,-6:-13]
drx_df <- drx_df[,-6:-13]
lev_df <- lev_df[,-6:-13]
op_df <- op_df[,-6:-13]
tl_df <- tl_df[,-6:-13]
fpx_df <- fpx_df[,-6:-13]

# Carregando os dataframes de adversarios ---------------------------------------------------------------------------
ds_adversarios_loud <- read.csv("C:/Users/anonb/Documents/TCC Pós/Scripts/scripts_times_champions/ds_adversarios_loud.csv",
                                sep = ',') %>% select(-X)
ds_adversarios_drx <- read.csv("C:/Users/anonb/Documents/TCC Pós/Scripts/scripts_times_champions/ds_adversarios_drx.csv",
                               sep = ',') %>% select(-X)
ds_adversarios_fpx <- read.csv("C:/Users/anonb/Documents/TCC Pós/Scripts/scripts_times_champions/ds_adversarios_fpx.csv",
                               sep = ',') %>% select(-X)
ds_adversarios_fntc <- read.csv("C:/Users/anonb/Documents/TCC Pós/Scripts/scripts_times_champions/ds_adversarios_fntc.csv",
                                sep = ',') %>% select(-X)
ds_adversarios_tl <- read.csv("C:/Users/anonb/Documents/TCC Pós/Scripts/scripts_times_champions/ds_adversarios_tl.csv",
                              sep = ',') %>% select(-X)
ds_adversarios_op <- read.csv("C:/Users/anonb/Documents/TCC Pós/Scripts/scripts_times_champions/ds_adversarios_op.csv",
                              sep = ',') %>% select(-X)
ds_adversarios_xset <- read.csv("C:/Users/anonb/Documents/TCC Pós/Scripts/scripts_times_champions/ds_adversarios_xset.csv",
                                sep = ',') %>% select(-X)
ds_adversarios_lev <- read.csv("C:/Users/anonb/Documents/TCC Pós/Scripts/scripts_times_champions/ds_adversarios_lev.csv",
                               sep = ',') %>% select(-X)

# Ao fim de cada formula abaixo eu desconsiderei (subtraindo) o número de rounds ganhos ou perdidos nesse campeonato
#Loud
loud_lev <- sum(ds_adversarios_loud$Adversario == 'Leviatán' & ds_adversarios_loud$Resultados == 'Win') - 
  sum(ds_adversarios_loud$Adversario == 'Leviatán' & ds_adversarios_loud$Resultados == 'Lose') - 2 

loud_op <- sum(ds_adversarios_loud$Adversario == 'OpTic Gaming' & ds_adversarios_loud$Resultados == 'Win') - 
  sum(ds_adversarios_loud$Adversario == 'OpTic Gaming' & ds_adversarios_loud$Resultados == 'Lose') - 4

#Drx
drx_fpx <- sum(ds_adversarios_drx$Adversario == 'FunPlus Phoenix' & ds_adversarios_drx$Resultados == 'Win') -
  sum(ds_adversarios_drx$Adversario == 'FunPlus Phoenix' & ds_adversarios_drx$Resultados == 'Lose') - 4

drx_loud <- sum(ds_adversarios_drx$Adversario == 'LOUD' & ds_adversarios_drx$Resultados == 'Win') -
  sum(ds_adversarios_drx$Adversario == 'LOUD' & ds_adversarios_drx$Resultados == 'Lose') + 2

drx_fntc <- sum(ds_adversarios_drx$Adversario == 'FNATIC' & ds_adversarios_drx$Resultados == 'Win') -
  sum(ds_adversarios_drx$Adversario == 'FNATIC' & ds_adversarios_drx$Resultados == 'Lose') - 1

drx_op <- sum(ds_adversarios_drx$Adversario == 'OpTic Gaming' & ds_adversarios_drx$Resultados == 'Win') -
  sum(ds_adversarios_drx$Adversario == 'OpTic Gaming' & ds_adversarios_drx$Resultados == 'Lose') + 1

#OpTic
op_tl <- sum(ds_adversarios_op$Adversario == 'Team Liquid' & ds_adversarios_op$Resultados == 'Win') -
  sum(ds_adversarios_op$Adversario == 'Team Liquid' & ds_adversarios_op$Resultados == 'Lose') - 1

#Xset
xset_fntc <- sum(ds_adversarios_xset$Adversario == 'FNATIC' & ds_adversarios_xset$Resultados == 'Win') -
  sum(ds_adversarios_xset$Adversario == 'FNATIC' & ds_adversarios_xset$Resultados == 'Lose') - 2

xset_fpx <- sum(ds_adversarios_xset$Adversario == 'FunPlus Phoenix' & ds_adversarios_xset$Resultados == 'Win') -
  sum(ds_adversarios_xset$Adversario == 'FunPlus Phoenix' & ds_adversarios_xset$Resultados == 'Lose') + 1

xset_op <- sum(ds_adversarios_xset$Adversario == 'OpTic Gaming' & ds_adversarios_xset$Resultados == 'Win') - 
  sum(ds_adversarios_xset$Adversario == 'OpTic Gaming' & ds_adversarios_xset$Resultados == 'Lose') + 1

#Fpx
fpx_lev <- sum(ds_adversarios_fpx$Adversario == 'Leviatán' & ds_adversarios_fpx$Resultados == 'Win') -
  sum(ds_adversarios_fpx$Adversario == 'Leviatán' & ds_adversarios_fpx$Resultados == 'Lose') - 2

#Team Liquid
tl_fntc <- sum(ds_adversarios_tl$Adversario == 'FNATIC' & ds_adversarios_tl$Resultados == 'Win') -
  sum(ds_adversarios_tl$Adversario == 'FNATIC' & ds_adversarios_tl$Resultados == 'Lose') + 2

# Tentando uma formula para dizer a porcentagem de chance de vitória do time 1 sobre o time 2 ----------------------
jogo1 <- (mean(drx_df$R) + drx_fpx * 0.01) / ((mean(drx_df$R) + drx_fpx * 0.01) +
                                                (mean(fpx_df$R) + (-drx_fpx * 0.01)))

jogo2 <- (mean(lev_df$R) + -loud_lev * 0.01) / ((mean(lev_df$R) + -loud_lev * 0.01) +
                                                 mean(loud_df$R) + (loud_lev * 0.01))

jogo3 <- (mean(op_df$R) + op_tl * 0.01) / ((mean(op_df$R) + op_tl * 0.01) + 
                                             mean(tl_df$R) + -op_tl * 0.01)

jogo4 <- (mean(xset_df$R) + xset_fntc * 0.01) / ((mean(xset_df$R) + xset_fntc * 0.01) +
                                                 mean(fntc_df$R) + -xset_fntc * 0.01)

jogo5 <- (mean(fpx_df$R) + fpx_lev * 0.01) / ((mean(fpx_df$R) + fpx_lev * 0.01) + 
                                                mean(lev_df$R) + -fpx_lev * 0.01)

jogo6 <- (mean(tl_df$R) + tl_fntc * 0.01) / ((mean(tl_df$R) + tl_fntc * 0.01) +
                                               mean(fntc_df$R) + -tl_fntc * 0.01)

jogo7 <- (mean(drx_df$R) + drx_loud * 0.01) / ((mean(drx_df$R) + drx_loud * 0.01) + 
                                                 mean(loud_df$R) + -drx_loud * 0.01)

jogo8 <- (mean(op_df$R) + -xset_op * 0.01) / ((mean(op_df$R) + -xset_op * 0.01) + 
                                                mean(xset_df$R) + xset_op * 0.01)

jogo9 <- (mean(xset_df$R) + xset_fpx * 0.01) / ((mean(xset_df$R) + xset_fpx * 0.01) + 
                                                  mean(fpx_df$R) + -xset_fpx * 0.01)

jogo10 <- (mean(drx_df$R) + drx_fntc * 0.01) / ((mean(drx_df$R) + drx_fntc * 0.01) + 
                                                  mean(fntc_df$R) + -drx_fntc * 0.01)

jogo11 <- (mean(loud_df$R) + loud_op * 0.01) / ((mean(loud_df$R) + loud_op * 0.01) +
                                                  mean(op_df$R) + -loud_op * 0.01)

jogo12 <- (mean(fpx_df$R) + -drx_fpx * 0.01) / ((mean(fpx_df$R) + -drx_fpx * 0.01) +
                                                  mean(drx_df$R) + drx_fpx * 0.01)

jogo13 <- (mean(loud_df$R) + loud_op * 0.01) / ((mean(loud_df$R) + loud_op * 0.01) +
                                                  mean(op_df$R) + -loud_op * 0.01)

jogo14 <- (mean(op_df$R) + -drx_op * 0.01) / ((mean(op_df$R) + -drx_op * 0.01) + 
                                               mean(drx_df$R) + drx_op * 0.01)

acertos = 0

# Criando variaveis dos times
drx <- (mean(drx_df$R) + drx_fpx * 0.01)
# Tentando criar um dataframe com os embates para usar na rede neural
resultado <- c('W', 'L', 'W', 'W', 'W', 'L', 'L', 'W', 'L', 'W', 'W', 'L', 'W', 'W')

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
  if(jogo11 > 0.50){
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


# Resultado final: 0.6428571 % de acurácia, ou seja, (9/14) acertos.

# Histograma para analisar os resultados e entender melhor a distribuição dos resultados
hist <- c(jogo1, jogo2, jogo3, jogo4, jogo5, jogo6, jogo7, jogo8, jogo9, jogo10, jogo11, jogo12, jogo13, jogo14)

hist(hist, breaks = 10)

