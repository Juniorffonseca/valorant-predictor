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
jogos <- read.csv2('jogos.csv') %>% dplyr::select(-X)

# Normalizando os dados ------------------------------------------------------------------------------------
normalizando <- dplyr::select(jogos, -ganhador)
normalizando <- as.data.frame(scale(normalizando))
jogos <- dplyr::select(jogos, ganhador)
jogos <- cbind(normalizando, jogos)
rm(normalizando)

# Criando dataframes de teste e validação -----------------------------------------------------------------
set.seed(15)
inp <- sample(2, nrow(jogos), replace = TRUE, prob = c(0.7, 0.3))
training_data <- jogos[inp==1, ]
test_data <- jogos[inp==2, ]

# Modelando a rede neural ---------------------------------------------------------------------------------
set.seed(13)
n <- neuralnet(ganhador ~ time1R + time2R + time1ACS + time2ACS + time1KD + time2KD + time1KAST + time2KAST + time1ADR + 
                 time2ADR,
               data = training_data,
               hidden = c(7),
               err.fct = "sse",
               linear.output = T,
               threshold = 0.01,
               lifesign = 'minimal',
               rep = 10,
               algorithm = 'rprop-',
               stepmax = 10000)

plot(n, rep = 3)

n$result.matrix
n$net.result[[1]]
nn1 <- ifelse(n$net.result[[1]]>0.5,0,1)
misClassificationError = mean(training_data$ganhador != nn1)
OutPutVsPred <- as.data.frame(cbind(training_data$ganhador, nn1))
OutPutVsPred$V1 <- gsub(2, 0, OutPutVsPred$V1) 
#nteste <- neuralnet(ganhador == 1 ~ time1 + time2, jogos, hidden = 10, threshold = 0.01)
#plot(nteste, rep = 1)
# Prediction ---------------------------------------------------------------------------------------------

Predict = compute(n, test_data)
Predict$net.result

predictVstest <- cbind(test_data, Predict$net.result)

prob <- Predict$net.result
pred <- ifelse(prob > 0.5, 'Win', 'Lose')
pred

predictVstest <- cbind(test_data$ganhador, pred)


