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
nomes_arquivos <- paste0('csv/catalogacao_diaria/', format(datas, '%Y-%m-%d'), '_urls.csv')

urls_lista <- list()

for (arquivo in nomes_arquivos) {
  urls_lista[[arquivo]] <- possibly(read.csv2, otherwise = NULL)(arquivo) %>% select(-X)
}

urls <- do.call(rbind, urls_lista)

row.names(urls) <- NULL

url_teste <- urls[1,]

# H2H
x <- read_html(url_teste) %>% html_nodes('div.match-h2h-matches-score') %>% html_text() %>%
  str_replace_all('\n', ' ') %>% str_replace_all('\t', '')
x

# Past Matchs
z <- read_html(url_teste) %>% html_nodes('div.wf-card.mod-dark.match-histories.mod-first.mod-loss') %>%
  html_text() %>% str_replace_all('\n', ' ') %>% str_replace_all('\t', '')

z

# Parei aqui