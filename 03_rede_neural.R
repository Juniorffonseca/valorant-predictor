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
set.seed(33)
inp <- sample(2, nrow(jogos), replace = TRUE, prob = c(0.7, 0.3))
training_data <- jogos[inp==1, ]
test_data <- jogos[inp==2, ]

# Modelando a rede neural ---------------------------------------------------------------------------------
n <- neuralnet(ganhador ~ time1R + time2R + time1ACS + time2ACS + time1KAST + time2KAST + time1KD + time2KD + time1ADR + 
                 time2ADR,
               data = training_data,
               hidden = c(8,8,8),
               err.fct = "sse",
               linear.output = T,
               threshold = 0.001,
               lifesign = 'minimal',
               rep = 1,
               algorithm = 'rprop-',
               stepmax = 10000)

#plot(n, rep = 1)

# Prediction ---------------------------------------------------------------------------------------------
Predict = compute(n, test_data)

nn2 <- ifelse(Predict$net.result[,1]>Predict$net.result[,2],1,0)

predictVstest <- cbind(test_data, Predict$net.result)
sum(predictVstest$ganhador == nn2)/30

# Salvando o modelo
save(n, file = "model_nnet.rda")
