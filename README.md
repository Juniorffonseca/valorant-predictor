# Valorant
 Usando Aprendizado de Máquina para prever resultados de partidas do cenário competitivo de Valorant.

## 01_jogadores_vlr.R
Esse script contém todos os códigos utilizados para coletar os dados dos jogadores no site *vlr.gg*, contém ainda, códigos que limpam esses dados, organizam e filtram conforme a necessidade do projeto. Deixando apenas jogadores com times, tirando jogadores que não tenham um número minimo específico de rounds 

## 02_jogos.R
Com o intuito de obter uma base de dados do histórico de partidas para usar como input na rede neural, há esse script que utiliza do arquivo csv gerado pelo script *01_jogadores_vlr.R* para buscar os jogadores de cada time especifico e mesclar em dataframes dos times individualmente. 

Após isso, é realizada uma análise desses dados de forma a retirar aquelas informações que não são tão relevantes para aplicar na rede neural. São feitas médias de R (Rating), ACS (Average Combat Score), KAST (Kill Assistance Survive and Trading), KD (Kill/Death) e ADR (Average Damage per Round) do time como um todo e todas essas médias são comparadas em cada confronto e resultam em um novo dataframe que será exportado como arquivo csv.

Atualmente foram catalogadas 813 partidas de diferentes competições por meio do site vlr.gg

## 03_rede_neural.R
Script onde é carregado o arquivo csv com todos os jogos catalogados e passado para um dataframe. Após isso, é feita uma divisão de aproximadamente 70% desses dados para base de treinamento e 30% desses dados para base de teste. É feita uma rede neural e conforme resultados obtidos será possível guardar uma rede neural para carregar posteriormente com os mesmos parâmetros.

## 04_previsão.R
Último script, onde será possível definir dois times especificos e rodar na rede neural que foi salva no script anterior. Nesse script é dado load na rede neural e, após informar os jogadores de cada time será possível dizer qual time tem mais chance de ganhar a partida e qual tem menos.

## app.R
Arquivo utilizado para deploy de um app no qual é possível informar o url e receber como retorno o resultado final da rede neural para aquela partida em específico.


Link do app: 

## [Valorant Prediction](https://jrff.shinyapps.io/scripts/)