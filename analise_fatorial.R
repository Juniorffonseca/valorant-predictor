### Aplicando análise fatorial no meu tcc ###

# PACOTES
pacotes <- c("plotly", #plataforma gráfica
             "tidyverse", #carregar outros pacotes do R
             "ggrepel", #geoms de texto e rótulo para 'ggplot2' que ajudam a
             #evitar sobreposição de textos
             "knitr", "kableExtra", #formatação de tabelas
             "reshape2", #função 'melt'
             "PerformanceAnalytics", #função 'chart.Correlation' para plotagem
             "psych", #elaboração da fatorial e estatísticas
             "ltm", #determinação do alpha de Cronbach pela função 'cronbach.alpha'
             "Hmisc", # matriz de correlações com p-valor
             "readxl") # importar arquivo Excel

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

# Carregando base de dados
dados <- read.csv2('dados_gerais.csv')

# Arrumando o dataframe
dados <- dados[,-1]

# Visualização da base de dados
dados %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE,
                font_size = 20)

# Estatísticas descritivas
summary(dados)

# Scatter e ajuste linear entre duas variáveis que estejam no dataframe dados

dados %>%
  ggplot() +
  geom_point(aes(x = ACS, y = K.D),
             color = "darkorchid",
             size = 3) +
  geom_smooth(aes(x = ACS, y = K.D),
              color = "orange", 
              method = "lm", 
              formula = y ~ x, 
              se = FALSE,
              size = 1.3) +
  labs(x = "ACS",
       y = "K.D") +
  theme_bw()

# Coeficientes de correlação de Pearson para cada par de variável
rho <- rcorr(as.matrix(dados[,3:7]), type="pearson")
corr_coef <- rho$r # Matriz de correlações
corr_sig <- round(rho$P, 5) # Matriz com p-valor dos coeficientes

# Elaboração de um mapa de calor das correlações de Pearson entre as variáveis
ggplotly(
  dados[,3:7] %>%
    cor() %>%
    melt() %>%
    rename(Correlação = value) %>%
    ggplot() +
    geom_tile(aes(x = Var1, y = Var2, fill = Correlação)) +
    geom_text(aes(x = Var1, y = Var2, label = format(Correlação, digits = 1)),
              size = 5) +
    scale_fill_viridis_b() +
    labs(x = NULL, y = NULL) +
    theme_bw())


# Visualização das distribuições das variáveis, scatters, valores das correlações
chart.Correlation(dados[,3:7], histogram = TRUE, pch = "+")


### Elaboração a Análise Fatorial Por Componentes Principais ###

# Teste de esfericidade de Bartlett
cortest.bartlett(dados[,3:7])

# Elaboração da análise fatorial por componentes principais
fatorial <- principal(dados[,3:7],
                      nfactors = length(dados[,3:7]),
                      rotate = "none",
                      scores = TRUE)

fatorial
