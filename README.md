# Valorant
 Usando Aprendizado de Máquina para prever resultados de partidas do cenário competitivo de Valorant.

## 01_jogadores_vlr.R
Esse script contém todos os códigos utilizados para coletar os dados dos jogadores no site *vlr.gg*, contém ainda, códigos que limpam esses dados, organizam e filtram conforme a necessidade do projeto. Deixando apenas jogadores com times, tirando jogadores que não tenham um número minimo específico de rounds 

## 02_jogos.R
Com o intuito de obter uma base de dados do histórico de partidas para usar como input na rede neural, há esse script que utiliza do arquivo csv gerado pelo script *01_jogadores_vlr.R* para buscar os jogadores de cada time especifico e mesclar em dataframes dos times individualmente. 

Após isso, é realizada uma análise desses dados de forma a retirar aquelas informações que não são tão relevantes para aplicar na rede neural. São feitas médias de R (Rating), ACS (Average Combat Score), KAST (Kill Assistance Survive and Trading), KD (Kill/Death) e ADR (Average Damage per Round) do time como um todo e todas essas médias são comparadas em cada confronto e resultam em um novo dataframe que será exportado como arquivo csv.

Atualmente foram catalogadas 97 partidas das seguintes competições:


* [VCT Champions 2022](https://www.vlr.gg/event/1015/valorant-champions-2022) 

* [Game Changers 2022](https://www.vlr.gg/event/1092/champions-tour-game-changers-championship-berlin)

* [Champions Tour South America: Last Chance Qualifier 2022](https://www.vlr.gg/event/1111/champions-tour-south-america-last-chance-qualifier)

* [Champions Tour North America: Last Chance Qualifier 2022](https://www.vlr.gg/event/1130/champions-tour-north-america-last-chance-qualifier)

* [Champions Tour EMEA: Last Chance Qualifier 2022](https://www.vlr.gg/event/1117/champions-tour-emea-last-chance-qualifier)

* [Champions Tour East Asia: Last Chance Qualifier 2022](https://www.vlr.gg/event/1083/champions-tour-east-asia-last-chance-qualifier)

* [Game Changers Brazil - Series 2 2022](https://www.vlr.gg/event/1162/game-changers-brazil-series-2)