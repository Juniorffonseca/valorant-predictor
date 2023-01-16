# Valorant
Usando Aprendizado de Máquina para prever resultados de partidas do cenário competitivo de Valorant.

## 01_jogadores.R
Esse script contém todos os códigos utilizados para coletar os dados dos jogadores no site *vlr.gg*, contém ainda, códigos que limpam esses dados, organizam e filtram conforme a necessidade do projeto. Deixando apenas jogadores com times, tirando jogadores que não tenham um número minimo específico de rounds 

## 02_jogos.R
Com o intuito de obter uma base de dados do histórico de partidas para usar como input na rede neural, há esse script que utiliza do arquivo csv gerado pelo script *01_jogadores_vlr.R* para buscar os jogadores de cada time especifico e mesclar em dataframes dos times individualmente. 

Após isso, é realizada uma análise desses dados de forma a retirar aquelas informações que não são tão relevantes para aplicar na rede neural. São feitas médias de R (Rating), ACS (Average Combat Score), KAST (Kill Assistance Survive and Trading), KD (Kill/Death) e ADR (Average Damage per Round) do time como um todo e todas essas médias são comparadas em cada confronto e resultam em um novo dataframe que será exportado como arquivo csv.

Atualmente foram catalogadas 813 partidas de diferentes competições por meio do site vlr.gg

## 03_rede_neural.R
Script onde é carregado o arquivo csv com todos os jogos catalogados e passado para um dataframe. Após isso, é feita uma divisão de aproximadamente 70% (**566** partidas) desses dados para base de treinamento e 30% (**247** partidas) desses dados para base de teste. É feita uma rede neural e conforme resultados obtidos será possível guardar uma rede neural para carregar posteriormente com os mesmos parâmetros.

Das 247 partidas usadas na base de teste **199** foram previstas corretamente e **48** incorretamente, registrando uma acurácia de **81%**.

## 04_previsão.R
Último script, onde será possível definir dois times especificos e rodar na rede neural que foi salva no script anterior. Nesse script é dado load na rede neural e, após informar os jogadores de cada time será possível dizer qual time tem mais chance de ganhar a partida e qual tem menos.

## 05_testes.R
Script que estou utilizando para catalogar novas partidas e testar elas na rede neural para checar a acurácia. Atualmente foram testadas mais **277** partidas com **207** acertos e **70** erros. Mantendo uma acurácia de **75%**, muito próxima da obtida na base de teste (**81%**).

## global.R
Arquivo utilizado para deploy de um app no qual é possível informar o url e receber como retorno o resultado final da rede neural para aquela partida em específico.

## ui.R
Faz parte do deploy do shinyapp. Nesse script é possível alterar a forma como a página é apresentada ao usuário.

## server.R
A outra parte do deploy, responsável pela parte "backend". Contém script que faz com que seja possível rodar a rede neural no servidor e receber como resposta a predição da rede neural.

Link do app: 

## [Valorant Prediction](https://jrff.shinyapps.io/scripts/)

---------------------------------
# English:

# Valorant
Using Machine Learning to predict match results from the competitive Valorant scene.

## 01_players.R
This script contains all the codes used to collect player data on the vlr.gg website, it also contains codes that clean this data, organize and filter it as needed by the project. Leaving only players with teams, removing players who don't have a specific minimum number of rounds

## 02_games.R
In order to obtain a database of the history of matches to use as input in the neural network, there is this script that uses the csv file generated by the script 01_jogadores_vlr.R to search for the players of each specific team and merge them into dataframes of the teams individually .

After that, an analysis of these data is carried out in order to remove those information that are not so relevant to apply in the neural network. R (Rating), ACS (Average Combat Score), KAST (Kill Assistance Survive and Trading), KD (Kill/Death) and ADR (Average Damage per Round) averages are made for the team as a whole and all these averages are compared in each match and result in a new dataframe that will be exported as a csv file.

Currently, 813 matches from different competitions have been cataloged through the website vlr.gg

## 03_neural_network.R
Script where the csv file with all cataloged games is loaded and passed to a dataframe. After that, a division of approximately 70% (566 matches) of these data is made for the training base and 30% (247 matches) of these data for the test base. A neural network is made and according to the results obtained, it will be possible to save a neural network to load later with the same parameters.

Of the 247 matches used in the test base, 199 were predicted correctly and 48 incorrectly, registering an accuracy of 81%.

## 04_forecast.R
Last script, where it will be possible to define two specific teams and run in the neural network that was saved in the previous script. In this script, the neural network is loaded and, after informing the players of each team, it will be possible to say which team has more chances of winning the match and which has less.

## 05_tests.R
Script I'm using to catalog new matches and test them on the neural network to check accuracy. Currently, 277 matches have been tested, with 207 hits and 70 errors. Maintaining an accuracy of 75%, very close to that obtained in the test base (81%).

## global.R
File used to deploy an app in which it is possible to inform the url and receive the final result of the neural network for that specific game as a return.

## ui.R
It's part of the shinyapp deploy. In this script it is possible to change the way the page is presented to the user.

## server.R
The other part of the deploy, responsible for the "backend" part. It contains a script that makes it possible to run the neural network on the server and receive the neural network prediction as a response.

App link:

## [Valorant Prediction](https://jrff.shinyapps.io/scripts/)
