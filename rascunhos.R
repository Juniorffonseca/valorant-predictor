# Instalando pacotes (se necessário) e carregando ----------------------------------------------------------
remotes::install_github('Juniorffonseca/r-pacote-valorant')
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
library(plotly)
library(pROC)
library(ROCR)
library(kableExtra)
library(glmnet)
library(valorant)

# Carregando partidas diarias e unindo em um df ------------------------------------------------------------
datas <- seq(as.Date('2023-02-19'), Sys.Date() - 1, by = 'day')
nomes_arquivos <- paste0('csv/catalogacao_diaria/', format(datas, '%Y-%m-%d'), '_partidas.csv')

jogos_lista <- list()

for (arquivo in nomes_arquivos) {
  jogos_lista[[arquivo]] <- possibly(read.csv2, otherwise = NULL)(arquivo)
}

jogos <- bind_rows(jogos_lista) %>% select(-X)

vars <- c('RND', 'R', 'ACS', 'KAST', 'KD', 'ADR', 'KPR', 'APR', 'FKPR', 'FDPR', 'K', 'D', 'A', 'FK', 'FD')

for (i in vars) {
  new_var <- paste0(i, "_diff")
  jogos[[new_var]] <- jogos[[paste0("time1", i)]] - jogos[[paste0("time2", i)]]
}

jogos_diff <- jogos %>% select(ends_with("_diff"), ganhador)

jogos_diff$ganhador <- as.factor(jogos_diff$ganhador)

data_split <- initial_split(jogos_diff, prop = 0.7, strata = 'ganhador')

training_data <- training(data_split)
test_data <- testing(data_split)

s <- 1

argumentos <- list(hidden = c(30), linear.output = FALSE)
metodo <- "neuralnet"
formula <- as.formula("ganhador ~ .")

modelo <- train(formula,
                data = training_data,
                method = "rf",
                trControl = trainControl(method = "boot"),
                method.args = list(method = metodo,
                                   linear.output = FALSE,
                                   algorithm = "rprop-",
                                   learningrate = 0.5,
                                   hidden = c(30),
                                   stepmax = 1e6,
                                   err.fct = "sse"))

previsoes <- predict(modelo, newdata = test_data)

caret::confusionMatrix(previsoes, test_data$ganhador)


# Definir a grade de hiperparâmetros a serem testados
nnGrid <- expand.grid(
  size = 7,
  hidden = c(15,30,45)
)

nnGrid <- expand.grid(
  size = c(7, 10, 15),
  decay = c(0, 0.001, 0.01, 0.1),
  maxit = c(500, 1000, 2000),
  linout = c(TRUE, FALSE),
  entropy = c(TRUE, FALSE),
  maxNWts = c(1000, 2000, 5000)
)

ctrl <- caret::trainControl(method='cv')

nnfit <- caret::train(formula,
                      data=training_data,
                      method='mlp',
                      tuneGrid=nnGrid,
                      trControl=ctrl,
                      maxit=1000,
                      verboseIter = FALSE
)

nnfit$results

modelo.final <- nnfit$finalModel


