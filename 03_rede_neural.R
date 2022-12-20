# Carregando pacotes --------------------------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(rvest)
library(quantmod)
library(httr)
library(tibble)
library(stringr)
library(neuralnet)

# Normalizando os dados (aplicando normalize) -------------------------------------------------------------
normalizando <- dplyr::select(jogos, -ganhador)
normalizando <- as.data.frame(scale(normalizando))
jogos <- dplyr::select(jogos, ganhador)
jogos <- cbind(normalizando, jogos)

#jogos$time1R <- (jogos$time1R - min(jogos$time1R)) / max(jogos$time1R) - min(jogos$time1R)
#jogos$time2R <- (jogos$time2R - min(jogos$time2R)) / max(jogos$time2R) - min(jogos$time2R)
#jogos$time1ACS <- (jogos$time1ACS - min(jogos$time1ACS)) / max(jogos$time1ACS) - min(jogos$time1ACS)
#jogos$time2ACS <- (jogos$time2ACS - min(jogos$time2ACS)) / max(jogos$time2ACS) - min(jogos$time2ACS)
#jogos$time1KD <- (jogos$time1KD - min(jogos$time1KD)) / max(jogos$time1KD) - min(jogos$time1KD)
#jogos$time2KD <- (jogos$time2KD - min(jogos$time2KD)) / max(jogos$time2KD) - min(jogos$time2KD)
#jogos$time1KAST <- (jogos$time1KAST - min(jogos$time1KAST)) / max(jogos$time1KAST) - min(jogos$time1KAST)
#jogos$time2KAST <- (jogos$time2KAST - min(jogos$time2KAST)) / max(jogos$time2KAST) - min(jogos$time2KAST)
#jogos$time1ADR <- (jogos$time1ADR - min(jogos$time1ADR)) / max(jogos$time1ADR) - min(jogos$time1ADR)
#jogos$time2ADR <- (jogos$time2ADR - min(jogos$time2ADR)) / max(jogos$time2ADR) - min(jogos$time2ADR)

# Criando dataframes de teste e validação -----------------------------------------------------------------
set.seed(13)
inp <- sample(2, nrow(jogos), replace = TRUE, prob = c(0.7, 0.3))
training_data <- jogos[inp==1, ]
test_data <- jogos[inp==2, ]

# Modelando a rede neural ---------------------------------------------------------------------------------
set.seed(1)
n <- neuralnet(ganhador == 1 ~ time1R + time2R + time1ACS + time2ACS + time1KD + time2KD + time1KAST + time2KAST + time1ADR + 
                 time2ADR,
               data = training_data,
               hidden = 7,
               err.fct = "sse",
               linear.output = T,
               threshold = 0.1,
               lifesign = 'minimal',
               rep = 10,
               algorithm = 'rprop-',
               stepmax = 10000)

plot(n, rep = 1)

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


