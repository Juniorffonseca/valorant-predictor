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

# Tentar dividir por região/país
paises_times <- matrix(nrow = 0, ncol = 2)

urls <- previsoes$b

for(url in urls){
  paises_times <- read_html(url) %>% html_nodes('div.match-header a') %>% 
    html_attr('href') %>% .[2:3] %>% rbind(paises_times)
}

paises_times <- as.data.frame(paises_times) %>%
  `rownames<-`(NULL) %>% slice(rev(row_number()))

paises_times$V1 <- paste0('https://www.vlr.gg', paises_times$V1)
paises_times$V2 <- paste0('https://www.vlr.gg', paises_times$V2)

paises_times_2 <- matrix(nrow = 0, ncol = 2)

for(url in 1:nrow(paises_times)){
  url1 <- paises_times[url, 'V1']
  url2 <- paises_times[url, 'V2']
  pais1 <- read_html(url1) %>% html_nodes('div.team-header-country') %>% 
    html_text() %>% str_replace_all('\n', '') %>% str_replace_all('\t', '')
  pais2 <- read_html(url2) %>% html_nodes('div.team-header-country') %>% 
    html_text() %>% str_replace_all('\n', '') %>% str_replace_all('\t', '')
  
  paises_times_2 <- rbind(paises_times_2, cbind(pais1, pais2))
}

paises_times_2 <- as.data.frame(paises_times_2)

previsoes$pais_1 <- paises_times_2$pais1
previsoes$pais_2 <- paises_times_2$pais2
previsoes$pais_comum <- ifelse(previsoes$pais_1 == previsoes$pais_2, previsoes$pais_1, 'N')

write.csv2(previsoes, 'csv/previsao_diaria/previsoes_odds.csv')
previsoes <- read.csv2('csv/previsao_diaria/previsoes_odds.csv') %>% select(-X)

# Definir o valor da aposta
valor_aposta <- 10
prob_minima <- 50
odd_minima <- 1
#pais_desejado <- 'China'

#previsoes <- previsoes %>% filter(pais_1 == 'China' | pais_2 == 'China')
previsoes <- previsoes %>% filter(!is.na(odds))

# Calcular o lucro ou prejuízo de cada aposta, considerando apenas previsões com probabilidade acima da prob_minima e que estão corretas
previsoes <- previsoes %>%
  mutate(lucro = ifelse((V1_n > prob_minima & ganhador == 1 & prev == 1 & odds > odd_minima) | (V2_n > prob_minima & ganhador == 0 & prev == 0 & odds > odd_minima), valor_aposta * (odds - 1), ifelse((V1_n > prob_minima & V1_n < 100-prob_minima & ganhador != 1 & prev == 1 & odds > odd_minima) | (V2_n > prob_minima & V2_n < 100-prob_minima & ganhador != 0 & prev == 0 & odds > odd_minima), -valor_aposta, ifelse((V1_n > prob_minima & ganhador != 1 & prev == 1 & odds > odd_minima) | (V2_n > prob_minima & ganhador != 0 & prev == 0 & odds > odd_minima), -valor_aposta, 0))))

# Calcular o resultado total das apostas, ignorando valores NA na coluna lucro
resultado <- sum(previsoes$lucro, na.rm = TRUE)

# Imprimir o resultado
cat("Resultado total das apostas: R$", resultado, "\n")

################################################################################

# ver quantos jogos seriam
sum(!is.na(previsoes$lucro))

# Definir o valor mínimo de probabilidade e odd para considerar uma aposta
prob_minima <- seq(50, 95, by = 5)
odd_minima <- seq(1.0, 2.0, by = 0.1)

# Criar um vetor para armazenar os resultados
resultados <- numeric(length(prob_minima) * length(odd_minima))

# Loop para calcular o resultado para cada combinação de prob_minima e odd_minima
k <- 1
for (i in 1:length(prob_minima)) {
  for (j in 1:length(odd_minima)) {
    previsoes_temp <- previsoes %>%
      mutate(lucro = ifelse((V1_n > prob_minima[i] & ganhador == 1 & prev == 1 & odds > odd_minima[j]) | (V2_n > prob_minima[i] & ganhador == 0 & prev == 0 & odds > odd_minima[j]), valor_aposta * (odds - 1), ifelse((V1_n > prob_minima[i] & V1_n < 100-prob_minima[i] & ganhador != 1 & prev == 1 & odds > odd_minima[j]) | (V2_n > prob_minima[i] & V2_n < 100-prob_minima[i] & ganhador != 0 & prev == 0 & odds > odd_minima[j]), -valor_aposta, ifelse((V1_n > prob_minima[i] & ganhador != 1 & prev == 1 & odds > odd_minima[j]) | (V2_n > prob_minima[i] & ganhador != 0 & prev == 0 & odds > odd_minima[j]), -valor_aposta, 0))))
    resultados[k] <- sum(previsoes_temp$lucro, na.rm = TRUE)
    k <- k + 1
  }
}

# Encontrar a combinação que resulta no maior lucro
ind_max <- which.max(resultados)
prob_minima_max <- prob_minima[(ind_max - 1) %/% length(odd_minima) + 1]
odd_minima_max <- odd_minima[(ind_max - 1) %% length(odd_minima) + 1]

# Imprimir o resultado máximo e a combinação correspondente
cat("Maior resultado total das apostas: R$", resultados[ind_max], "\n")
cat("Combinação que resulta no maior resultado total das apostas: prob_minima =", prob_minima_max, ", odd_minima =", odd_minima_max, "\n")

