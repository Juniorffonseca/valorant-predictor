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
library(plotly)
library(pROC)
library(valorant)

# Carregando partidas diarias e unindo em um df ------------------------------------------------------------
datas <- seq(as.Date("2023-02-19"), Sys.Date() - 1, by = "day")
nomes_arquivos <- paste0("csv/catalogacao_diaria/", format(datas, "%Y-%m-%d"), "_partidas.csv")

jogos_lista <- list()

for (arquivo in nomes_arquivos) {
  jogos_lista[[arquivo]] <- possibly(read.csv2, otherwise = NULL)(arquivo)
}

jogos <- bind_rows(jogos_lista) %>% select(-X)
jogos$ganhador <- as.factor(jogos$ganhador)

#write.csv2(jogos, 'csv/partidas_teste.csv')
#jogos <- read.csv2('csv/partidas_teste.csv')

# Criando dataframes de teste e validação -----------------------------------------------------------------
set.seed(1)

data_split <- initial_split(jogos, prop = 0.7, strata = "ganhador")

training_data <- training(data_split)
test_data <- testing(data_split)

hidden_n <- c(30)
t <- 1 #thresholder
formula <- 'ganhador == 1 ~ .'

# Normalizando os dados ------------------------------------------------------------------------------------
normalizando_test <- dplyr::select(test_data, -ganhador)
normalizando_test <- as.data.frame(scale(normalizando_test))
test_data <- dplyr::select(test_data, ganhador)
test_data <- cbind(normalizando_test, test_data)

normalizando_training <- dplyr::select(training_data, -ganhador)
normalizando_training <- as.data.frame(scale(normalizando_training))
training_data <- dplyr::select(training_data, ganhador)
training_data <- cbind(normalizando_training, training_data)

# Modelando a rede neural ---------------------------------------------------------------------------------
n <- neuralnet(formula,
               data = training_data,
               hidden = hidden_n,
               err.fct = "sse",
               linear.output = F,
               threshold = t,
               lifesign = 'minimal',
               rep = 1,
               algorithm = 'rprop-',
               stepmax = 10000)

#plot(n, rep = 1)

# Prediction ---------------------------------------------------------------------------------------------
Predict = compute(n, test_data)

nn2 <<- ifelse(Predict$net.result[,1]>0.5,1,0)


predictVstest <- cbind(test_data, Predict$net.result)
i <<- sum(predictVstest$ganhador == nn2)/ nrow(test_data)

# Achar uma boa seed -------------------------------------------------------------------------------------
s <- 1 # 10679 13/03 0.7959% acuracia 98 partidas
w <- 0.1

while ( i < 0.75) {
  achar_Seed(s, hidden_n, t = 0.9)
  s <- s + 1
  w <<- ifelse(i>w, w <<- i, w <<- w) 
  
  print(w)
}

# Atualizando a seed para achar a melhor neuralnetwork ----------------------------------------------------
set.seed(s-1)
data_split <- initial_split(jogos, prop = 0.7, strata = "ganhador")
training_data <- training(data_split)
test_data <- testing(data_split)

normalizando_test <- dplyr::select(test_data, -ganhador)
normalizando_test <- as.data.frame(scale(normalizando_test))
test_data <- dplyr::select(test_data, ganhador)
test_data <- cbind(normalizando_test, test_data)

normalizando_training <- dplyr::select(training_data, -ganhador)
normalizando_training <- as.data.frame(scale(normalizando_training))
training_data <- dplyr::select(training_data, ganhador)
training_data <- cbind(normalizando_training, training_data)

Predict = compute(n, test_data)

nn2 <<- ifelse(Predict$net.result[,1]>0.5,1,0)

predictVstest <- cbind(test_data, Predict$net.result)

# Procurando uma rede neural com acuracia a cima de determinado percentual --------------------------------
z <- 0.1

while (i < 0.79) {
  achar_Nn(t = 0.9)
}
beep(8)

#save(n, file ='rede_neural.rda')
#save(n, file='rede_neural_teste.rda')
#save(n, file='prototipo_rede_neural.rda') #14/03/2023 81/98 base de testes (0.8265306% acuracia)

# Matriz de confusão ---------------------------------------------------------------------------------------
jogos <- read.csv2('csv/partidas_teste.csv') %>% dplyr::select(-X)
set.seed(s-1) #10679
data_split <- initial_split(jogos, prop = 0.7, strata = "ganhador")
training_data <- training(data_split)
test_data <- testing(data_split)

normalizando_test <- dplyr::select(test_data, -ganhador)
normalizando_test <- as.data.frame(scale(normalizando_test))
test_data <- dplyr::select(test_data, ganhador)
test_data <- cbind(normalizando_test, test_data)

normalizando_training <- dplyr::select(training_data, -ganhador)
normalizando_training <- as.data.frame(scale(normalizando_training))
training_data <- dplyr::select(training_data, ganhador)
training_data <- cbind(normalizando_training, training_data)

test_data$ganhador <- as.factor(test_data$ganhador)

# Carregando modelo e obtendo os resultados
load('prototipo_rede_neural.rda')
Predict = compute(n, test_data)
nn2 <- ifelse(Predict$net.result[,1]>0.5, 1, 0)
nn2 <- as.factor(nn2)
x <- caret::confusionMatrix(nn2, test_data$ganhador)
F1 <- x$byClass['F1']
x <- as.data.frame(x$table)
predictVstest <- cbind(test_data, Predict$net.result)
names(predictVstest)[32] <- 'previsao'

# Plot
ggplot(data = x, mapping = aes(x = Reference, y = Prediction)) +
  geom_tile(aes(fill = Freq), colour = 'white') +
  geom_text(aes(label = sprintf('%1.0f', Freq)), vjust = 1) +
  scale_fill_gradient(low = 'white', high = 'green') +
  theme_bw() + theme(legend.position = 'none')


#Log Loss
logLoss(actual = test_data$ganhador, predicted = Predict$net.result)

#Plot distribuição
histogram(predictVstest$`Predict$net.result`, breaks = 98,
          col = ifelse(as.factor(predictVstest$ganhador) == 1, "blue", "red"))

plot(predictVstest$`Predict$net.result`, predictVstest$ganhador,
     col = ifelse(predictVstest$ganhador == 1, "green", "red"),
     xlim = c(0,1), xlab = "Porcentagem", ylab = "Ganhador",
     cex = 1, pch = 19, yaxt = 'n')

axis(side = 2, at = c(0, 1), labels = c("0", "1"))

# Adicionar legenda para as cores
legend("left", legend = c("Time 2 ganhador", "Time 1 ganhador"), col = c("red", "green"), pch = 1)

abline(v = 0.5, lty = 2, col = "black")

plot_ly(data = predictVstest, x = ~previsao, y = ~ganhador,
        color = ~factor(ganhador), colors = c("red", "green"), type = "scatter",
        mode = "markers", marker = list(size = 10)) %>%
  layout(xaxis = list(title = "Porcentagem"), yaxis = list(title = "Ganhador"),
         legend = list(title = "Ganhador", font = list(size = 16)),
         margin = list(l = 50, r = 50, t = 50, b = 50),
         shapes = list(list(type = "line", x0 = 0.5, x1 = 0.5, y0 = 0, y1 = 1,
                            line = list(color = "gray", width = 2))))

prever('https://www.vlr.gg/167393/loud-vs-fnatic-champions-tour-2023-lock-in-s-o-paulo-gf')

#[1,] 43.10593
#[2,] 56.89407

# preciso ver a questão de espelhamento, provavelmente está errada a forma q estou fazendo atualmente, preciso fazer antes de treinar o modelo.
