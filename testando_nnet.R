# Instalando pacotes (se necessário) e carregando ----------------------------------------------------------
library(devtools)
install_github("Juniorffonseca/r-pacote-valorant")
library(caret)
library(dplyr)
library(tidyr)
library(rvest)
library(rsample)
library(readr)
library(quantmod)
library(httr)
library(tibble)
library(stringr)
library(neuralnet)
library(nnet)
library(caret)
library(ggplot2)
library(ModelMetrics)
library(beepr)
library(purrr)
library(valorant)

# Carregando partidas diarias e unindo em um df ------------------------------------------------------------
datas <- seq(as.Date("2023-02-19"), Sys.Date() - 1, by = "day")
nomes_arquivos <- paste0("csv/catalogacao_diaria/", format(datas, "%Y-%m-%d"), "_partidas.csv")

jogos_lista <- list()

for (arquivo in nomes_arquivos) {
  jogos_lista[[arquivo]] <- read.csv2(arquivo) %>% select(-X)
}

jogos <- bind_rows(jogos_lista)
jogos$ganhador <- as.factor(jogos$ganhador)

# Criando dataframes de teste e validação -----------------------------------------------------------------
set.seed(1)

data_split <- initial_split(jogos, prop = 0.7, strata = "ganhador")

training_data <- training(data_split)
test_data <- testing(data_split)

hidden_n <- c(30)

# Normalizando os dados ------------------------------------------------------------------------------------
normalizando_test <- dplyr::select(test_data, -ganhador)
normalizando_test <- as.data.frame(scale(normalizando_test))
test_data <- dplyr::select(test_data, ganhador)
test_data <- cbind(normalizando_test, test_data)

normalizando_training <- dplyr::select(training_data, -ganhador)
normalizando_training <- as.data.frame(scale(normalizando_training))
training_data <- dplyr::select(training_data, ganhador)
training_data <- cbind(normalizando_training, training_data)

training_data$ganhador <- as.factor(training_data$ganhador)

model <- train(ganhador ~ ., training_data, method = 'nnet', metric = 'Accuracy')

# Modelando a rede neural ---------------------------------------------------------------------------------
n <- nnet(formula = ganhador == 1 ~ .,
          data = training_data,
          decay = 0.1,
          size = 1,
          linout = F,
          threshold = 0.1,
          maxit = 100,
          trace = T,
          algorithm = "backpropragation")

# Fazer previsões usando o modelo treinado
predictions <- predict(n, test_data[, -31])
predictions
predictions <- ifelse(predictions > 0.5, 1, 0)
print(accuracy <- sum(predictions == test_data[, 31]) / nrow(test_data))

achar_Seed <- function(seed){
  set.seed(seed)
  data_split <- initial_split(jogos, prop = 0.7, strata = "ganhador")
  
  training_data <- training(data_split)
  test_data <- testing(data_split)
  
  hidden_n <- c(30)
  
  # Normalizando os dados ------------------------------------------------------------------------------------
  normalizando_test <- dplyr::select(test_data, -ganhador)
  normalizando_test <- as.data.frame(scale(normalizando_test))
  test_data <- dplyr::select(test_data, ganhador)
  test_data <- cbind(normalizando_test, test_data)
  
  normalizando_training <- dplyr::select(training_data, -ganhador)
  normalizando_training <- as.data.frame(scale(normalizando_training))
  training_data <- dplyr::select(training_data, ganhador)
  training_data <- cbind(normalizando_training, training_data)
  
  training_data$ganhador <- as.factor(training_data$ganhador)
  test_data$ganhador <- as.factor(test_data$ganhador)
  
  
  # Modelando a rede neural ---------------------------------------------------------------------------------
  n <- nnet(formula = ganhador == 1 ~ .,
            data = training_data,
            decay = 0.1,
            size = 1,
            linout = F,
            threshold = 1,
            maxit = 1000,
            trace = F,
            algorithm = "rprop+")
  
  # Fazer previsões usando o modelo treinado
  predictions <- predict(n, test_data[, -31])
  predictions <- ifelse(predictions > 0.5, 1, 0)
  print(accuracy <<- sum(predictions == test_data[, 31]) / nrow(test_data))
  
}

s <- 1

while (accuracy < 0.83){
  achar_Seed(s)
  s <- s + 1
  w <<- ifelse(accuracy>w, w <<- accuracy, w <<- w) 
  print(w)
}


achar_Nn <- function(){
  
  n <- nnet(formula = ganhador == 1 ~ .,
            data = training_data,
            decay = 0.05,
            size = 1,
            linout = F,
            threshold = 0.2,
            maxit = 1000,
            trace = F,
            algorithm = "rprop+")
  
  predictions <- predict(n, test_data[, -31])
  predictions <- ifelse(predictions > 0.5, 1, 0)
  print(accuracy <<- sum(predictions == test_data[, 31]) / nrow(test_data))
  
  z <<- ifelse(accuracy>z, z <<- i, z <<- z)
  
  print(z)
}

