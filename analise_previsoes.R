# Instalando (se necessário) e carregando pacotes ----------------------------------------------------------
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
setwd('C:/Users/anonb/Documents/TCC_Pós/Scripts')


# Carregando partidas diarias e unindo em um df ------------------------------------------------------------
datas <- seq(as.Date('2023-04-11'), Sys.Date() - 1, by = 'day')
nomes_arquivos <- paste0('csv/previsao_diaria/', format(datas, '%Y-%m-%d'), '_previsoes.csv')

previsoes_lista <- list()

for (arquivo in nomes_arquivos) {
  previsoes_lista[[arquivo]] <- possibly(read.csv2, otherwise = NULL)(arquivo)
}

previsoes_lista <- lapply(previsoes_lista, function(df) {
  df %>% mutate(X = as.character(X))
})

previsoes <- bind_rows(previsoes_lista)

previsoes$ganhador <- as.factor(previsoes$ganhador)
previsoes$prev <- as.factor(previsoes$prev)

#Plot distribuição
plot_ly(data = previsoes, x = ~V1_n, y = ~ganhador,
        color = ~factor(ganhador), colors = c('red', 'green'), type = 'scatter',
        mode = 'markers', marker = list(size = 3)) %>%
  layout(xaxis = list(title = 'Porcentagem'), yaxis = list(title = 'Ganhador'),
         legend = list(title = 'Ganhador', font = list(size = 16)),
         margin = list(l = 50, r = 50, t = 50, b = 50),
         shapes = list(list(type = 'line', x0 = 50, x1 = 50, y0 = 0, y1 = 1,
                            line = list(color = 'gray', width = 2))))

# Plot distribuição das probabilidades por densidade
ggplot(data = previsoes, aes(x = V1_n, fill = ganhador)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c('red', 'green')) +
  labs(x = 'Porcentagem', y = 'Densidade', fill = 'Ganhador') +
  theme_bw()

# Loop através das URLs em previsoes$b e extrai as odds
for (i in 1:nrow(previsoes)) {
  url <- previsoes$b[i]
  odds <- read_html(url) %>% 
    html_nodes('div.match-bet-item-half') %>% 
    html_text() %>% 
    str_replace_all('\t', '') %>% 
    str_replace_all('\n', '') %>% 
    .[1] %>% 
    gsub(".*?(\\d+\\.\\d+).*", "\\1", .)
  previsoes$odds[i] <- odds
}

previsoes$odds <- as.numeric(previsoes$odds)

acertos <- previsoes %>% filter(ganhador == prev)
erros <- previsoes %>% filter(ganhador != prev)

write.csv2(previsoes, 'csv/previsao_diaria/previsoes_odds.csv')
previsoes <- read.csv2('csv/previsao_diaria/previsoes_odds.csv') %>% select(-X)

previsoes$ganhador <- as.factor(previsoes$ganhador)
previsoes$prev <- as.factor(previsoes$prev)

# Definir o valor da aposta
valor_aposta <- 5

# Calcular o lucro ou prejuízo de cada aposta, ignorando valores NA nas odds
# previsoes <- previsoes %>%
#   mutate(lucro = ifelse(is.na(odds), NA, ifelse(ganhador == prev, valor_aposta * (as.numeric(odds) - 1), -valor_aposta)))

# Calcular o resultado total das apostas, ignorando valores NA na coluna lucro
#resultado <- sum(previsoes$lucro, na.rm = TRUE)

# Imprimir o resultado
#cat("Resultado total das apostas: R$", resultado, "\n")

# PAREI AQUI... PRECISO RESOLVER ISSO
# Definir o valor mínimo de probabilidade para considerar uma aposta
prob_minima <- 60

# Calcular o lucro ou prejuízo de cada aposta, considerando apenas previsões com probabilidade acima da prob_minima e que estão corretas
previsoes <- previsoes %>%
  mutate(lucro = ifelse(((V1_n > prob_minima & ganhador == 1 & prev == 1) | (V2_n > prob_minima & ganhador == 0 & prev == 0)), valor_aposta * (odds - 1), NA))

# Calcular o resultado total das apostas, ignorando valores NA na coluna lucro
resultado <- sum(previsoes$lucro, na.rm = TRUE)

# Imprimir o resultado
cat("Resultado total das apostas: R$", resultado, "\n")

# Definir o valor mínimo de odd para considerar uma aposta
odd_minima <- 1.5

# Calcular o lucro ou prejuízo de cada aposta, considerando apenas previsões com probabilidade acima da prob_minima e que estão corretas
previsoes <- previsoes %>%
  mutate(lucro = ifelse((V1_n > prob_minima*100 & ganhador == 1 & prev == 1) | (V2_n > prob_minima*100 & ganhador == 2 & prev == 2), valor_aposta * (odds - 1), -valor_aposta))

# ver quantos jogos seriam
sum(!is.na(previsoes$lucro))

