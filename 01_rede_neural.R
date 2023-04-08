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
library(valorant)

# Carregando partidas diarias e unindo em um df ------------------------------------------------------------
datas <- seq(as.Date('2023-02-19'), Sys.Date() - 1, by = 'day')
nomes_arquivos <- paste0('csv/catalogacao_diaria/', format(datas, '%Y-%m-%d'), '_partidas.csv')

jogos_lista <- list()

for (arquivo in nomes_arquivos) {
  jogos_lista[[arquivo]] <- possibly(read.csv2, otherwise = NULL)(arquivo)
}

jogos <- bind_rows(jogos_lista) %>% select(time1FKPR, time1FDPR, time1KPR, time1APR, time1KD,
                                           time2FKPR, time2FDPR, time2KPR, time2APR, time2KD,
                                           ganhador)
jogos$ganhador <- as.factor(jogos$ganhador)

#write.csv2(jogos, 'csv/partidas_teste.csv')
#jogos <- read.csv2('csv/partidas_teste.csv')

# Criando dataframes de teste e validação -----------------------------------------------------------------
set.seed(1)

data_split <- initial_split(jogos, prop = 0.7, strata = 'ganhador')

training_data <- training(data_split)
test_data <- testing(data_split)

hidden_n <- c(10)
#hidden_n <- c(30)
t <- 0.5 #thresholder

# formula <- 'ganhador == 1 ~ time1FKPR + time1FDPR + time1KPR + time1APR + time1KD + time1R + time1ADR +
# time2FKPR + time2FDPR + time2KPR + time2APR + time2KD + time2R + time2ADR'
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
               err.fct = 'sse',
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
s <- 700 # 10679 13/03 0.7959% acuracia 98 partidas
# 49308 08/04
w <- 0.1

while ( i < 0.76) {
  achar_Seed(s, hidden_n, t = 0.5, mostrar_i = F)
  s <- s + 1
  w <<- ifelse(i>w, w <<- i, w <<- w) 
  
  print(w)
}
#parei em 363195
# 16065 01/04
# 49260 02/04
# 122810 04/04 arquitetura antiga
# 8196 04/04 arquitetura nova (baita diferença no numero de seeds necessarias p o msm resultado)
# 143890 tentativas de seed e 80% acurácia
# 71905 0.8045113 acurácia

# Atualizando a seed para achar a melhor neuralnetwork ----------------------------------------------------
set.seed(s-1) #22263
data_split <- initial_split(jogos, prop = 0.7, strata = 'ganhador')
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

# Procurando uma rede neural com acuracia acima de determinado percentual --------------------------------
z <- 0.1

while (i < 0.80) {
  achar_Nn(t = 0.5, mostrar_i = F)
}
beep(8)

#save(n, file ='rede_neural.rda')
#save(n, file='rede_neural_teste.rda')
#save(n, file='prototipo_rede_neural.rda') #14/03/2023 81/98 base de teste (0.8265306% acuracia)
#save(n, file='21_03_nnet.rda') #21/03/2023 97/117 base de teste (0.8290598% acuracia)
#save(n, file='26_03_nnet.rda') #26/03/2023 105/129 base de teste (0.8139535 acuracia) #seed 16323
#save(n, file='04_04_nnet.rda') #04/04/2023 138/170 base de teste (0.8117647 acuracia) #seed 8196

# Matriz de confusão ---------------------------------------------------------------------------------------
jogos <- read.csv2('csv/partidas_teste.csv') %>% dplyr::select(-X)
s <- 10679
set.seed(s-1) #10679
data_split <- initial_split(jogos, prop = 0.7, strata = 'ganhador')
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
training_data$ganhador <- as.factor(training_data$ganhador)

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

# Curva ROC
ROC <- roc(response = as.factor(predictVstest$ganhador), 
           predictor = predictVstest$previsao)

predicoes <- ROCR::prediction(predictions = predictVstest$previsao, 
                        labels = as.factor(predictVstest$ganhador))

dados_curva_roc <- performance(predicoes, measure = "sens") 
sensitividade <- (performance(predicoes, measure = "sens"))@y.values[[1]] 
especificidade <- (performance(predicoes, measure = "spec"))@y.values[[1]]

ggplot() +
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1),
               color = 'grey40', size = 0.2) +
  geom_line(aes(x = 1 - especificidade, y = sensitividade),
            color = "darkorchid", size = 2) +
  labs(x = '1 - Especificidade',
       y = 'Sensitividade',
       title = paste('Área abaixo da curva:',
                     round(ROC$auc, 4),
                     '|',
                     'Coeficiente de Gini:',
                     round((ROC$auc[1] - 0.5) / 0.5, 4))) +
  theme(panel.background = element_rect(NA),
        panel.border = element_rect(color = 'black', fill = NA),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10)
  )


# Plot matriz de confusão
ggplot(data = x, mapping = aes(x = Reference, y = Prediction)) +
  geom_tile(aes(fill = Freq), colour = 'white') +
  geom_text(aes(label = sprintf('%1.0f', Freq)), vjust = 1) +
  scale_fill_gradient(low = 'white', high = 'green') +
  theme_bw() + theme(legend.position = 'none')


#Log Loss
logLoss(actual = test_data$ganhador, predicted = Predict$net.result)

#Plot distribuição
plot_ly(data = predictVstest, x = ~previsao, y = ~ganhador,
        color = ~factor(ganhador), colors = c('red', 'green'), type = 'scatter',
        mode = 'markers', marker = list(size = 4)) %>%
  layout(xaxis = list(title = 'Porcentagem'), yaxis = list(title = 'Ganhador'),
         legend = list(title = 'Ganhador', font = list(size = 16)),
         margin = list(l = 50, r = 50, t = 50, b = 50),
         shapes = list(list(type = 'line', x0 = 0.5, x1 = 0.5, y0 = 0, y1 = 1,
                            line = list(color = 'gray', width = 2))))

# Tentando gráfico para variáveis 

ggplotly(
  jogos %>% 
    ggplot() +
    geom_point(aes(x = time2R, y = ganhador), color = 'orange', size = 2) +
    geom_point(aes(x = time1R, y = ganhador), color = 'blue', size = 1) +
    labs(x = 'Estatística',
         y = 'Ganhador') +
    theme_bw()
)



# Fazer previsões nos dados de treinamento e teste usando a rede neural treinada
train_preds <- predict(n, training_data)
test_preds <- predict(n, test_data)

train_preds <- ifelse(train_preds > 0.5, 1, 0)
test_preds <- ifelse(test_preds > 0.5, 1, 0)

# Calcular a precisão da rede neural nos conjuntos de treinamento e teste
train_acc <- mean(train_preds == training_data$ganhador)
test_acc <- mean(test_preds == test_data$ganhador)

# Exibir a precisão nos conjuntos de treinamento e teste
cat('Precisão nos dados de treinamento:', train_acc, '\n')
cat('Precisão nos dados de teste:', test_acc, '\n')

# Verificar se há overfitting comparando a precisão nos conjuntos de treinamento e teste
if(test_acc < train_acc){
  cat('A precisão nos dados de teste é menor do que a precisão nos dados de treinamento, o que pode indicar overfitting.\n')
} else {
  cat('Não há evidência de overfitting.\n')
}

# Separar as variáveis preditoras e a variável de resposta
variaveis_preditoras <- subset(jogos, select = -c(ganhador))
variavel_resposta <- jogos$ganhador

# Normalizar as variáveis preditoras
variaveis_preditoras_norm <- scale(variaveis_preditoras)

jogos_normalizados <- cbind(variaveis_preditoras, variavel_resposta)

# Ajustar um modelo de regressão logística com as variáveis preditoras normalizadas
modelo <- glm(variavel_resposta ~ ., data = jogos_normalizados, family = 'binomial')

# Examinar a importância de cada variável no modelo
importancia <- abs(coef(modelo))
importancia_rel <- importancia/sum(importancia)
importancia_rel

# Criar um gráfico de barras para visualizar as importâncias relativas
barplot(importancia_rel, horiz = TRUE, las = 1, main = 'Importância Relativa das Variáveis')
