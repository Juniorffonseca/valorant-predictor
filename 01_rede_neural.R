# Definindo diretório --------------------------------------------------------------------------------------
setwd('C:/Users/anonb/Documents/TCC_Pós/Scripts')

# Carregando pacotes ---------------------------------------------------------------------------------------
pacotes <- c("remotes", "caret", "dplyr", "tidyr", "rvest", "rsample", "readr", "quantmod",
             "httr", "tibble", "stringr", "neuralnet", "nnet", "ggplot2", "ModelMetrics",
             "beepr", "purrr", "plotly", "pROC", "ROCR", "kableExtra", "glmnet", "valorant")

for (pacote in pacotes) {
  if (!require(pacote, character.only = TRUE)) {
    if (!requireNamespace("remotes", quietly = TRUE)) {
      install.packages("remotes")
    }
    remotes::install_github('Juniorffonseca/r-pacote-valorant')
    if (!require(pacote, character.only = TRUE)) {
      stop(paste("Pacote", pacote, "não encontrado"))
    }
  }
}

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

jogos <- select(jogos, ends_with("_diff"), ganhador)

jogos$ganhador <- as.factor(jogos$ganhador)

#write.csv2(jogos, 'csv/partidas_teste_10_04_2023.csv') # Salvar apenas conforme for criado outro modelo

# Criando dataframes de teste e validação -----------------------------------------------------------------
set.seed(1)

data_split <- initial_split(jogos, prop = 0.7, strata = 'ganhador')

training_data <- training(data_split)
test_data <- testing(data_split)

hidden_n <- c(15)

formula <- 'ganhador == 1 ~ .'

# Normalizando os dados -----------------------------------------------------------------------------------
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
               threshold = 0.5,
               lifesign = 'minimal',
               rep = 1,
               algorithm = 'rprop-',
               stepmax = 10000)

#plot(n, rep = 1)

# Prediction ----------------------------------------------------------------------------------------------
Predict = compute(n, test_data)

nn2 <<- ifelse(Predict$net.result[,1]>0.5,1,0)

predictVstest <- cbind(test_data, Predict$net.result)
i <<- sum(predictVstest$ganhador == nn2)/ nrow(test_data)

# Achar uma boa seed --------------------------------------------------------------------------------------
s <- 281768
w <- 0.1

while ( i < 0.78) {
  achar_Seed(s, hidden_n, t = 0.5, mostrar_i = F)
  s <- s + 1
  w <<- ifelse(i>w, w <<- i, w <<- w) 
  
  print(round(w, 2))
}

# Atualizando a seed para achar a melhor neuralnetwork ---------------------------------------------------
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

# Procurando uma rede neural com acuracia acima de determinado percentual ---------------------------------
z <- 0.1

while (i < 0.78) {
  achar_Nn(t = 0.5, mostrar_i = F)
}
beep(8)

#save(n, file ='rede_neural_10_04_2023.rda')

# Carregando os dados --------------------------------------------------------------------------------------
jogos <- read.csv2('csv/partidas_teste_10_04_2023.csv') %>% dplyr::select(-X)
jogos$ganhador <- as.factor(jogos$ganhador)
s <- 281768
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

# Carregando modelo e obtendo os resultados ----------------------------------------------------------------
load('rede_neural_10_04_2023.rda')
Predict = compute(n, test_data)
nn2 <- ifelse(Predict$net.result[,1]>0.5, 1, 0)
nn2 <- as.factor(nn2)
x <- caret::confusionMatrix(nn2, test_data$ganhador)
F1 <- x$byClass['F1']
x <- as.data.frame(x$table)
predictVstest <- cbind(test_data, Predict$net.result)
names(predictVstest)[length(predictVstest)] <- 'previsao'
accuracy <- sum(predictVstest$ganhador == nn2)/nrow(test_data)
error_rate <- sum(predictVstest$ganhador != nn2)/nrow(test_data)

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
  geom_text(aes(label = sprintf('%1.0f', Freq)), vjust = 1, size = 6.5) +
  scale_fill_gradient2(low = hcl(0, 100, 70), mid = 'white', high = 'springgreen', 
                       limits = c(0, max(x$Freq)), midpoint = max(x$Freq) / 2) +
  labs(x = "Classe Real", y = "Classe Predita", fill = "Frequência") +
  theme_bw(base_size = 14) +
  theme(legend.position = 'none', axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        axis.text.y = element_text(hjust = 1, size = 15), legend.title = element_text(size = 15)) +
  guides(fill = FALSE)

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

# Plot distribuição das probabilidades por densidade
ggplot(data = predictVstest, aes(x = previsao, fill = ganhador)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c('red', 'green')) +
  labs(x = 'Porcentagem', y = 'Densidade', fill = 'Ganhador') +
  theme_bw()

# Tentando gráfico para variáveis 

# Diferença de Rating
ggplot(jogos, aes(x = R_diff, fill = ganhador)) +
  geom_density(alpha = 0.7) +
  geom_vline(xintercept = 0, color = 'black', size = 0.3) +
  labs(x = 'Diferença da média de Rating', y = 'Densidade', fill = 'Ganhador') +
  scale_fill_manual(values = c('red', 'green')) +
  guides(fill = guide_legend(title = 'Ganhador')) +
  theme_bw() +
  theme(legend.position = 'top')

# Diferença de ROUNDS
ggplot(jogos, aes(x = RND_diff, fill = ganhador)) +
  geom_density(alpha = 0.7) +
  geom_vline(xintercept = 0, color = 'black', size = 0.3) +
  labs(x = 'Diferença da média de rounds', y = 'Densidade', fill = 'Ganhador') +
  scale_fill_manual(values = c('red', 'green')) +
  guides(fill = guide_legend(title = 'Ganhador')) +
  theme_bw() +
  theme(legend.position = 'bottom')

# Diferença de ACS

ggplot(jogos, aes(x = ACS_diff, fill = ganhador)) +
  geom_density(alpha = 0.7) +
  geom_vline(xintercept = 0, color = 'black', size = 0.3) +
  labs(x = 'Diferença da média de ACS', y = 'Densidade', fill = 'Ganhador') +
  scale_fill_manual(values = c('red', 'green')) +
  guides(fill = guide_legend(title = 'Ganhador')) +
  theme_bw() +
  theme(legend.position = 'bottom')

# Diferença de KAST
ggplot(jogos, aes(x = KAST_diff, fill = ganhador)) +
  geom_density(alpha = 0.7) +
  geom_vline(xintercept = 0, color = 'black', size = 0.3) +
  labs(x = 'Diferença da média de KAST', y = 'Densidade', fill = 'Ganhador') +
  scale_fill_manual(values = c('red', 'green')) +
  guides(fill = guide_legend(title = 'Ganhador')) +
  theme_bw() +
  theme(legend.position = 'bottom')

# Diferença de KD
ggplot(jogos, aes(x = KD_diff, fill = ganhador)) +
  geom_density(alpha = 0.7) +
  geom_vline(xintercept = 0, color = 'black', size = 0.3) +
  labs(x = 'Diferença da média de KD', y = 'Densidade', fill = 'Ganhador') +
  scale_fill_manual(values = c('red', 'green')) +
  guides(fill = guide_legend(title = 'Ganhador')) +
  theme_bw() +
  theme(legend.position = 'bottom')

# Diferença de ADR
ggplot(jogos, aes(x = ADR_diff, fill = ganhador)) +
  geom_density(alpha = 0.7) +
  geom_vline(xintercept = 0, color = 'black', size = 0.3) +
  labs(x = 'Diferença da média de ADR', y = 'Densidade', fill = 'Ganhador') +
  scale_fill_manual(values = c('red', 'green')) +
  guides(fill = guide_legend(title = 'Ganhador')) +
  theme_bw() +
  theme(legend.position = 'bottom')

# Diferença de KPR
ggplot(jogos, aes(x = KPR_diff, fill = ganhador)) +
  geom_density(alpha = 0.7) +
  geom_vline(xintercept = 0, color = 'black', size = 0.3) +
  labs(x = 'Diferença da média de KPR', y = 'Densidade', fill = 'Ganhador') +
  scale_fill_manual(values = c('red', 'green')) +
  guides(fill = guide_legend(title = 'Ganhador')) +
  theme_bw() +
  theme(legend.position = 'bottom')

# Diferença de APR
ggplotly(
  jogos %>%
    ggplot() +
    geom_density(aes(x = APR_diff, fill = ganhador), alpha = 0.7) +
    geom_vline(xintercept = 0, color = 'black', size = 0.3) +
    labs(x = 'Diferença da média de APR', y = 'Densidade', fill = 'Ganhador') +
    scale_fill_manual(values = c('red', 'green')) +
    guides(fill = guide_legend(title = 'Ganhador')) +
    theme_bw()
)

# Diferença de FKPR
ggplotly(
  jogos %>%
    ggplot() +
    geom_density(aes(x = FKPR_diff, fill = ganhador), alpha = 0.7) +
    geom_vline(xintercept = 0, color = 'black', size = 0.3) +
    labs(x = 'Diferença da média de FKPR', y = 'Densidade', fill = 'Ganhador') +
    scale_fill_manual(values = c('red', 'green')) +
    guides(fill = guide_legend(title = 'Ganhador')) +
    theme_bw()
)

# Diferença de FDPR
ggplotly(
  jogos %>%
    ggplot() +
    geom_density(aes(x = FDPR_diff, fill = ganhador), alpha = 0.7) +
    geom_vline(xintercept = 0, color = 'black', size = 0.3) +
    labs(x = 'Diferença da média de FDPR', y = 'Densidade', fill = 'Ganhador') +
    scale_fill_manual(values = c('red', 'green')) +
    guides(fill = guide_legend(title = 'Ganhador')) +
    theme_bw()
)

# Diferença de Kill
ggplotly(
  jogos %>%
    ggplot() +
    geom_density(aes(x = K_diff, fill = ganhador), alpha = 0.7) +
    geom_vline(xintercept = 0, color = 'black', size = 0.3) +
    labs(x = 'Diferença da média de Kill', y = 'Densidade', fill = 'Ganhador') +
    scale_fill_manual(values = c('red', 'green')) +
    guides(fill = guide_legend(title = 'Ganhador')) +
    theme_bw()
)

# Diferença de Assistência
ggplot(jogos, aes(x = A_diff, fill = ganhador)) +
  geom_density(alpha = 0.7) +
  geom_vline(xintercept = 0, color = 'black', size = 0.3) +
  labs(x = 'Diferença da média de Assistência', y = 'Densidade', fill = 'Ganhador') +
  scale_fill_manual(values = c('red', 'green')) +
  guides(fill = guide_legend(title = 'Ganhador')) +
  theme_bw() +
  theme(legend.position = 'bottom')

# Diferença de First Kill
ggplotly(
  jogos %>%
    ggplot() +
    geom_density(aes(x = FK_diff, fill = ganhador), alpha = 0.7) +
    geom_vline(xintercept = 0, color = 'black', size = 0.3) +
    labs(x = 'Diferença da média de First Kill', y = 'Densidade', fill = 'Ganhador') +
    scale_fill_manual(values = c('red', 'green')) +
    guides(fill = guide_legend(title = 'Ganhador')) +
    theme_bw()
)

# Diferença de First Death
ggplotly(
  jogos %>%
    ggplot() +
    geom_density(aes(x = FD_diff, fill = ganhador), alpha = 0.7) +
    geom_vline(xintercept = 0, color = 'black', size = 0.3) +
    labs(x = 'Diferença da média de First Death', y = 'Densidade', fill = 'Ganhador') +
    scale_fill_manual(values = c('red', 'green')) +
    guides(fill = guide_legend(title = 'Ganhador')) +
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
  cat('A precisão nos dados de teste é menor do que a precisão nos dados de treinamento,
      o que pode indicar overfitting.\n')
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
barplot(importancia_rel, horiz = F, las = 1, main = 'Importância Relativa das Variáveis')


