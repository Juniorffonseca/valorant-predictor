# Catalogar partidas por url

ur_string <- 'colocar o url aqui'

catalogarporUrl <- function (string){
  tryCatch(
 
    {
  
  # Carregando pacotes --------------------------------------------------------------------------------------
  library(tidyverse)
  library(dplyr)
  library(tidyr)
  library(rvest)
  library(quantmod)
  library(httr)
  library(tibble)
  library(stringr)
  library(reshape2)

  dados_gerais <- read.csv2("csv/jogadores.csv")
  
  dados_gerais <- dplyr::select(dados_gerais, Player, R, ACS, K.D, KAST, ADR)
  row.names(dados_gerais) <- make.names(dados_gerais[,1], unique = T)
  dados_gerais <- dplyr::select(dados_gerais, -Player)
  dados_gerais$KAST <- parse_number(dados_gerais$KAST)
  
  info <- read_html(string) %>% 
    html_nodes("table") %>% 
    html_table()
  
  placar <- read_html(string) %>% 
    html_nodes("div.js-spoiler") %>% html_text(trim=T)
  
  placar <- str_replace_all(placar, '\t', '') %>% str_replace_all('\n', '')
  
  placar <- as.data.frame(placar[1])
  
  placar <- separate(placar, 'placar[1]', into = c('Time1', 'Time2'), sep = ':', extra = 'merge')
  
  ifelse(placar$Time1 > placar$Time2, ganhador <- 1, ganhador <- 0)
  
  timeA <- info[[1]]
  timeB <- info[[2]]
  
  timeA <- lapply(timeA, str_replace_all, '\n', '') %>% 
    lapply(str_replace_all, '\t', '')
  timeB <- lapply(timeB, str_replace_all, '\n', '') %>% 
    lapply(str_replace_all, '\t', '')
  
  timeA <- as.data.frame(timeA[1])
  timeB <- as.data.frame(timeB[1])
  
  colnames(timeA) <- '1'
  colnames(timeB) <- '1'
  
  timeA <- separate(timeA, '1', into = c("Player", "Team"), sep = "\\s+", extra = "merge")
  timeB <- separate(timeB, '1', into = c("Player", "Team"), sep ="\\s+", extra = "merge")
  
  timeA <- timeA$Player
  timeB <- timeB$Player
  
  timeA <- paste0('\\b', timeA, '\\b') 
  dados_gerais$timeA <- ifelse(grepl(paste(timeA, collapse = '|'), rownames(dados_gerais), useBytes = T), 1, 0)
  
  timeB <- paste0('\\b', timeB, '\\b') 
  dados_gerais$timeB <- ifelse(grepl(paste(timeB, collapse = '|'), rownames(dados_gerais), useBytes = T), 1, 0)
  
  timeA_df <- filter(dados_gerais, dados_gerais$timeA == 1)
  timeA_df <- dplyr::select(timeA_df, R, ACS, K.D, KAST, ADR)
  timeB_df <- filter(dados_gerais, dados_gerais$timeB == 1) 
  timeB_df <- dplyr::select(timeB_df, R, ACS, K.D, KAST, ADR)
  
  if(nrow(timeA_df) == 5 && nrow(timeB_df) == 5){
  
  # Médias
  timeA_R <- mean(timeA_df$R)
  timeA_ACS <- mean(timeA_df$ACS)
  timeA_KAST <- mean(timeA_df$KAST)
  timeA_KD <- mean(timeA_df$K.D)
  timeA_ADR <- mean(timeA_df$ADR)
  timeB_R <- mean(timeB_df$R)
  timeB_ACS <- mean(timeB_df$ACS)
  timeB_KAST <- mean(timeB_df$KAST)
  timeB_KD <- mean(timeB_df$K.D)
  timeB_ADR <- mean(timeB_df$ADR)
  
  partida <- c(timeA_R, timeB_R, timeA_ACS, timeB_ACS, timeA_KAST, timeB_KAST, timeA_KD, timeB_KD,
               timeA_ADR, timeB_ADR)
  
  partida <- t(partida)
  
  partida <- as.data.frame(partida) %>% cbind(ganhador)
  
  colnames(partida) <- c('time1R', 'time2R', 'time1ACS', 'time2ACS', 'time1KAST', 'time2KAST', 'time1KD', 'time2KD',
                         'time1ADR', 'time2ADR', 'ganhador')
  
  return(partida)
  }
    }
  , error = function(e){cat('error:', conditionMessage(e), '\n')})

}



partida1 <- catalogarporUrl('https://www.vlr.gg/159483/nter-vs-attacking-soul-esports-fgc-valorant-invitational-2022-epilogue-w1')
partida2 <- catalogarporUrl('https://www.vlr.gg/161251/oq-academy-vs-over-quality-esl-clash-of-nations-kr-jp-uf')
partida3 <- catalogarporUrl('https://www.vlr.gg/161329/sr-nacague-vs-made-in-thailand-esl-clash-of-nations-sea-ubsf')
partida4 <- catalogarporUrl('https://www.vlr.gg/156889/enigma-gaming-vs-global-esports-skyesports-sea-championship-presented-by-amd-d3')
partida5 <- catalogarporUrl('https://www.vlr.gg/161145/generation-of-miracles-vs-funcrew-esl-clash-of-nations-oce-uf')
partida6 <- catalogarporUrl('https://www.vlr.gg/161144/bobo-vs-built-for-greatness-esl-clash-of-nations-oce-lr1')
partida7 <- catalogarporUrl('https://www.vlr.gg/161002/nearest-airport-vs-ex-soar-nerd-street-valorant-lockdown-open-1-gf')
partida8 <- catalogarporUrl('https://www.vlr.gg/157512/sealed-vs-oddik-copa-rakin-2022-playoffs-gf')
partida9 <- catalogarporUrl('https://www.vlr.gg/162851/soniqs-vs-moon-raccoons-knights-valorant-freezeout-ro16')
partida10 <- catalogarporUrl('https://www.vlr.gg/162852/shopify-rebellion-vs-the-silk-road-knights-valorant-freezeout-ro16')
partida11 <- catalogarporUrl('https://www.vlr.gg/162848/tsm-vs-no-future-life-is-over-knights-valorant-freezeout-ro16')
partida12 <- catalogarporUrl('https://www.vlr.gg/162850/ex-soar-vs-emotional-support-knights-valorant-freezeout-ro16')
partida13 <- catalogarporUrl('https://www.vlr.gg/161083/reign-vs-acad-micos-do-tucuruvi-challengers-league-brazil-split-1-lr4')
partida14 <- catalogarporUrl('https://www.vlr.gg/163319/no-future-life-is-over-vs-royalty-forgotten-nerd-street-valorant-lockdown-open-3-ro32')
partida15 <- catalogarporUrl('https://www.vlr.gg/163322/royal-pekingese-vs-cry-is-free-nerd-street-valorant-lockdown-open-3-ro32')
partida16 <- catalogarporUrl('https://www.vlr.gg/163327/da-biggest-birds-vs-neverdone-nerd-street-valorant-lockdown-open-3-ro32')
partida17 <- catalogarporUrl('https://www.vlr.gg/161980/reckoning-esports-vs-gods-reign-penta-pro-series-lr3')
partida18 <- catalogarporUrl('https://www.vlr.gg/159485/edward-gaming-vs-nter-fgc-valorant-invitational-2022-epilogue-w1')
partida19 <- catalogarporUrl('https://www.vlr.gg/158669/sparx-esports-vs-cgn-esports-valorant-regional-league-dach-evolution-unity-lr3')
partida20 <- catalogarporUrl('https://www.vlr.gg/153010/valorians-vs-bref-valorant-regional-league-france-revolution-coupe-de-france-playoffs-lr3')
partida21 <- catalogarporUrl('https://www.vlr.gg/158670/cowana-gaming-vs-dsyre-valorant-regional-league-dach-evolution-unity-lr3')
partida22 <- catalogarporUrl('https://www.vlr.gg/158668/mouz-vs-dsyre-valorant-regional-league-dach-evolution-unity-lr2')
partida23 <- catalogarporUrl('https://www.vlr.gg/158665/prinzen-von-stuttgart-vs-sparx-esports-valorant-regional-league-dach-evolution-unity-lr2')
partida24 <- catalogarporUrl('https://www.vlr.gg/159024/team-divinity-vs-parla-esports-g-loot-valorant-clash-grand-finals-qf')
partida25 <- catalogarporUrl('https://www.vlr.gg/159025/bbl-esports-vs-giants-gaming-g-loot-valorant-clash-grand-finals-qf')
partida26 <- catalogarporUrl('https://www.vlr.gg/159021/bonk-vs-fire-flux-esports-g-loot-valorant-clash-grand-finals-ro16')
partida27 <- catalogarporUrl('https://www.vlr.gg/158664/enro-griffins-vs-dsyre-valorant-regional-league-dach-evolution-unity-lr1')
partida28 <- catalogarporUrl('https://www.vlr.gg/159016/fut-esports-vs-team-divinity-g-loot-valorant-clash-grand-finals-ro16')
partida29 <- catalogarporUrl('https://www.vlr.gg/158663/into-the-breach-protocol-vs-cowana-gaming-valorant-regional-league-dach-evolution-unity-lr1')
partida30 <- catalogarporUrl('https://www.vlr.gg/156690/fut-esports-vs-parla-esports-esa-open-fire-all-stars-2022-playoffs-lbf')
partida31 <- catalogarporUrl('https://www.vlr.gg/159050/velocity-gaming-vs-global-esports-skyesports-sea-championship-presented-by-amd-lr1')
partida32 <- catalogarporUrl('https://www.vlr.gg/156689/fire-flux-esports-vs-fut-esports-esa-open-fire-all-stars-2022-playoffs-lr2')
partida33 <- catalogarporUrl('https://www.vlr.gg/158661/kommando-howl-vs-sparx-esports-valorant-regional-league-dach-evolution-unity-lr1')
partida34 <- catalogarporUrl('https://www.vlr.gg/157517/100jobs-vs-oddik-copa-rakin-2022-playoffs-lr3')
partida35 <- catalogarporUrl('https://www.vlr.gg/157757/edward-gaming-vs-nter-xyper-arena-weekly-showdown-4-uf')

npartidas <- rbind(partida1, partida2, partida3, partida4, partida5, partida6, partida7, partida8, partida9, partida10,
                   partida11, partida12, partida13, partida14, partida15, partida16, partida17, partida18, partida19,
                   partida20, partida21, partida22, partida23, partida24, partida25, partida26, partida27, partida28,
                   partida29, partida30, partida31, partida32, partida33, partida34, partida35)

write.csv2(npartidas, 'csv/npartidas.csv')


