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
library(googleLanguageR)
library(countrycode)
library(valorant)
setwd('C:/Users/anonb/Documents/TCC_Pós/Scripts')

# Carregando partidas diarias e unindo em um df ------------------------------------------------------------
datas <- seq(as.Date('2023-02-19'), as.Date('2023-04-09'), by = 'day')
nomes_arquivos <- paste0('csv/catalogacao_diaria/', format(datas, '%Y-%m-%d'), '_urls.csv')

urls_lista <- list()

for (arquivo in nomes_arquivos) {
  urls_lista[[arquivo]] <- possibly(read.csv2, otherwise = NULL)(arquivo)
}

urls_lista <- lapply(urls_lista, function(df) {
  df$x <- as.character(df$x)
  return(df)
})

urls <- bind_rows(urls_lista) %>% select(-X)

write.csv2(urls, 'csv/urls_utilizados.csv')

nomes_times <- matrix(nrow = 0, ncol = 2)

for (url in urls[,]){
  nomes_times <- read_html(url) %>% html_nodes("div.wf-title-med") %>% 
    html_text() %>% str_replace_all("\n", "") %>% str_replace_all("\t", "") %>%
    rbind(nomes_times)
}

nomes_times <- as.data.frame(nomes_times) %>%
  `rownames<-`(NULL) %>% slice(rev(row_number()))

write.csv2(nomes_times, 'csv/times_catalogados.csv')

nomes_jogadores <- matrix(nrow = 0, ncol = 10)

for(url in urls[,]){
  nomes_jogadores <- read_html(url) %>% html_nodes("td.mod-player a") %>% 
    html_text() %>% str_replace_all("\n", "") %>% str_replace_all("\t", "") %>%
    .[1:10] %>% rbind(nomes_jogadores)
}

nomes_jogadores <- as.data.frame(nomes_jogadores) %>%
  `rownames<-`(NULL) %>% slice(rev(row_number()))

write.csv2(nomes_jogadores, 'csv/jogadores_catalogados.csv')

# Número de times únicos:
count(unique(nomes_times)) #628/674

# Número de jogadores únicos:
sum(sapply(nomes_jogadores, function(x) length(unique(x)))) #5466/6740

# Tentando pegar países de cada jogador na amostra
paises_jogadores <- matrix(nrow = 0, ncol = 10)

for(url in urls[,]){
  paises_jogadores <- read_html(url) %>% html_nodes('td.mod-player') %>% html_nodes('.flag') %>% 
    html_attr('title') %>% .[1:10] %>% rbind(paises_jogadores)
}

paises_jogadores <- as.data.frame(paises_jogadores) %>%
  `rownames<-`(NULL) %>% slice(rev(row_number()))

write.csv2(paises_jogadores, 'csv/paises_jogadores.csv')

# Tentando pegar países de cada time na amostra
paises_times <- matrix(nrow = 0, ncol = 2)
for(url in urls[,]){
  paises_times <- read_html(url) %>% html_nodes('div.match-header a') %>% 
    html_attr('href') %>% .[2:3] %>% rbind(paises_times)
}

paises_times <- as.data.frame(paises_times) %>%
  `rownames<-`(NULL) %>% slice(rev(row_number()))

paises_times$V1 <- paste0('https://www.vlr.gg', paises_times$V1)
paises_times$V2 <- paste0('https://www.vlr.gg', paises_times$V2)

write.csv2(paises_times_urls, 'csv/paises_times_urls.csv')

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

write.csv2(paises_times_2, 'csv/paises_times.csv')

# Carregando os dataframes
times_catalogados <- read.csv2('csv/times_catalogados.csv') %>% dplyr::select(-X)
jogadores_catalogados <- read.csv2('csv/jogadores_catalogados.csv') %>% dplyr::select(-X)
paises_jogadores <- read.csv2('csv/paises_jogadores.csv') %>% dplyr::select(-X)
paises_times <- read.csv2('csv/paises_times.csv') %>% dplyr::select(-X)

freq <- table(unlist(paises_jogadores))

df_freq <- data.frame(
  pais = names(freq),
  frequencia = as.numeric(freq)
)

# filtrar as 10 linhas com as maiores frequências
df_top10 <- df_freq[order(df_freq$frequencia, decreasing = TRUE), ][1:10, ]
df_top5 <- df_freq[order(df_freq$frequencia, decreasing = TRUE), ][1:5, ]
df_top20 <- df_freq[order(df_freq$frequencia, decreasing = TRUE), ][1:20, ]

# Soma a frequência dos países que não estão no top 5
df_top10 <- rbind(df_top10, list(pais = "Outros", frequencia = sum(df_freq$frequencia[!(df_freq$pais %in% df_top10$pais)])))
df_top20 <- rbind(df_top20, list(pais = "Outros", frequencia = sum(df_freq$frequencia[!(df_freq$pais %in% df_top20$pais)])))

# criar um plot de ggplot2 com barras de frequência
ggplot(df_top20, aes(x = pais, y = frequencia, fill = pais)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "País", y = "Frequência")

# Gráfico de pizza
ggplot(df_top20, aes(x = "", y = frequencia, fill = pais)) +
  geom_bar(width = 1, stat = "identity") +
  geom_text(aes(label = scales::percent(round(frequencia/sum(frequencia), 2))), 
            position = position_stack(vjust = 0.5), size = 4) +
  coord_polar(theta = "y") +
  theme_void() +
  scale_fill_viridis_d() +
  guides(fill = guide_legend(title = "Países")) +
  labs(x = "País", y = "Frequência")

ggplot(df_top5, aes(x = "", y = frequencia, fill = pais)) +
  geom_bar(width = 1, stat = "identity") +
  geom_text(aes(label = scales::percent(round(frequencia/sum(frequencia), 2))), 
            position = position_stack(vjust = 0.5), size = 4) +
  coord_polar(theta = "y") +
  theme_void() +
  scale_fill_viridis_d() +
  guides(fill = guide_legend(title = "Países")) +
  labs(x = "País", y = "Frequência")


freq <- table(unlist(paises_times))

df_freq <- data.frame(
  pais = names(freq),
  frequencia = as.numeric(freq)
)

# filtrar as 10 linhas com as maiores frequências
df_top10 <- df_freq[order(df_freq$frequencia, decreasing = TRUE), ][1:10, ]
df_top20 <- df_freq[order(df_freq$frequencia, decreasing = TRUE), ][1:20, ]
df_top20 <- rbind(df_top20, list(pais = "Outros", frequencia = sum(df_freq$frequencia[!(df_freq$pais %in% df_top20$pais)])))

# criar um plot de ggplot2 com barras de frequência
ggplot(df_top20, aes(x = reorder(pais, frequencia), y = frequencia, fill = pais)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "País", y = "Frequência")
