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

odds <- read_html(previsoes$b[1]) %>% html_nodes('div.match-bet-item-half') %>% html_text() %>% 
  str_replace_all('\t', '') %>% str_replace_all('\n', '') %>% .[1] %>% gsub(".*?(\\d+\\.\\d+).*", "\\1", .)


