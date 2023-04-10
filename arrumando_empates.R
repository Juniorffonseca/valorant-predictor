# Definir o número de dias a serem subtraídos a cada iteração
dias_subtraidos <- seq(1, 50)

# Iterar sobre o vetor de dias a serem subtraídos
for (i in dias_subtraidos) {
  # Definir a data correspondente
  data <- as.Date(Sys.Date() - i)
  
  # Definir os nomes dos arquivos correspondentes
  nome_arquivo_urls <- paste(data, "_urls.csv", sep = "")
  nome_arquivo_partidas <- paste(data, "_partidas.csv", sep = "")
  
  # Verificar se os arquivos existem para as datas específicas
  if (file.exists(paste('csv/catalogacao_diaria/', nome_arquivo_urls, sep = '')) &&
      file.exists(paste('csv/catalogacao_diaria/', nome_arquivo_partidas, sep = ''))) {
    
    # Carregar os arquivos CSV correspondentes
    b <- read.csv2(paste('csv/catalogacao_diaria/', nome_arquivo_urls, sep = '')) %>% select(-X)
    dff <- read.csv2(paste('csv/catalogacao_diaria/', nome_arquivo_partidas, sep = '')) %>% select(-X)
    
    # Obter o ganhador de cada partida
    ganhador <- '' %>% .[0]
    
    for (j in b[,]) {
      ganhador[length(ganhador)+1] <- get_Ganhadores(j)
    }
    
    dff$ganhador <- ganhador
    
    dff <- dff[!dff$ganhador %in% "empate", ]
    
    b <- b[as.numeric(rownames(dff)),]
    
    rownames(dff) <- NULL
    rownames(b) <- NULL
    
    # Salvar os resultados em arquivos CSV correspondentes
    write.csv2(dff, paste('csv/catalogacao_diaria/', nome_arquivo_partidas, sep = ''))
    write.csv2(b, paste('csv/catalogacao_diaria/', nome_arquivo_urls, sep = ''))
    
  } else {
    # Se os arquivos não existirem, imprimir uma mensagem de aviso
    cat(paste("Os arquivos correspondentes a", data, "não foram encontrados.\n"))
  }
}
