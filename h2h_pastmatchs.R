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

# Limpando o Environment
rm(list=ls())

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

# H2H -------------------------------------------------------------------------------------------------------
h2h <- list()

for (url in urls[,]){
  tryCatch({
    x <- read_html(url) %>% html_nodes('div.match-h2h-matches-score') %>% html_text() %>%
      str_replace_all('\n', ' ') %>% str_replace_all('\t', '') %>%
      str_match("^\\s*(\\d+)\\s*(\\d+)\\s*$")
    
    if (is.null(x)) {
      # Não houve partida no histórico
      h2h_temp <- cbind(NA, NA)
    } else if (nrow(x) == 1) {
      # Houve apenas uma partida no histórico
      x <- x[,-1]
      t1 <- as.numeric(x[1])
      t2 <- as.numeric(x[2])
      h2h_temp <- cbind(t1, t2)
    } else {
      # Houve duas ou mais partidas no histórico
      x <- x[,-1]
      t1 <- sum(as.numeric(x[,1]))
      t2 <- sum(as.numeric(x[,2]))
      h2h_temp <- cbind(t1, t2)
    }
    
    h2h[[length(h2h)+1]] <- h2h_temp
  }
  , error = function(e){cat('error:', conditionMessage(e), '\n')})
}

h2h <- h2h %>% map_df(as_tibble)

write.csv2(h2h, 'h2h.csv')

# Past Matchs -----------------------------------------------------------------------------------------------
p_matchs <- list()

for (url in urls[,]){
  tryCatch({
    z <- read_html(url) %>% html_nodes('div.match-histories-item-result span') %>%
      html_text() %>% str_replace_all('\n', ' ') %>% str_replace_all('\t', '')
    
    z <- as.numeric(z)
    
    wins_t1 <- sum(z[1], z[3], z[5], z[7], z[9])
    loses_t1 <- sum(z[2], z[4], z[6], z[8], z[10])
    profit_t1 <- (wins_t1 - loses_t1)
    
    wins_t2 <- sum(z[11], z[13], z[15], z[17], z[19])
    loses_t2 <- sum(z[12], z[14], z[16], z[18], z[20])
    profit_t2 <- (wins_t2 - loses_t2)
    
    past_matches <- cbind(profit_t1, profit_t2) 
    
    p_matchs[[length(p_matchs)+1]] <- past_matches
    ## Finalizado praticamente, agr só preciso colocar para iterar em todos os links ----------------------------
    
  }
  , error = function(e){cat('error:', conditionMessage(e), '\n')})
}

p_matchs <- p_matchs %>% map_df(as_tibble)

p_matchs[is.na(p_matchs)] <- 0

write.csv2(p_matchs, 'p_matchs.csv')
