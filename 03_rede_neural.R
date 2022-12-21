# Carregando pacotes --------------------------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(rvest)
library(quantmod)
library(httr)
library(tibble)
library(stringr)
library(neuralnet)

# Carregando o dataframe -----------------------------------------------------------------------------------
jogos <- read.csv2('csv/jogos.csv') %>% dplyr::select(-X)

# Normalizando os dados ------------------------------------------------------------------------------------
normalizando <- dplyr::select(jogos, -ganhador)
normalizando <- as.data.frame(scale(normalizando))
jogos <- dplyr::select(jogos, ganhador)
jogos <- cbind(normalizando, jogos)
rm(normalizando)
jogos$ganhador <- as.factor(jogos$ganhador)

# Criando dataframes de teste e validação -----------------------------------------------------------------
set.seed(15)
inp <- sample(2, nrow(jogos), replace = TRUE, prob = c(0.8, 0.2))
training_data <- jogos[inp==1, ]
test_data <- jogos[inp==2, ]

# Modelando a rede neural ---------------------------------------------------------------------------------
n <- neuralnet(ganhador ~ time1R + time2R + time1ACS + time2ACS + time1KD + time2KD + time1KAST + time2KAST + time1ADR + 
                 time2ADR,
               data = training_data,
               hidden = c(10,10),
               err.fct = "sse",
               linear.output = T,
               threshold = 0.01,
               lifesign = 'minimal',
               rep = 10,
               algorithm = 'rprop-',
               stepmax = 10000)

plot(n, rep = 3)

# Prediction ---------------------------------------------------------------------------------------------
Predict = compute(n, test_data)

nn2 <- ifelse(Predict$net.result[,1]>Predict$net.result[,2],1,0)

predictVstest <- cbind(test_data, Predict$net.result)
sum(predictVstest$ganhador == nn2)/28



