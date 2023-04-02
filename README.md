###### Português (BR):
# Valorant
Usando Aprendizado de Máquina para prever resultados de partidas do cenário competitivo de Valorant.

![image](https://user-images.githubusercontent.com/94936578/229330663-a3b5a254-9ae6-4c3a-9488-978eb94665a1.png)

## 01_jogos.R
Esse script contém todos os códigos utilizados para coletar os dados dos jogadores no site *vlr.gg*, contém ainda, códigos que limpam esses dados, organizam e filtram conforme a necessidade do projeto. Os dados dos jogadores são obtidos por meio dos urls das partidas. É feita uma média das estatísticas individuais de R (Rating), ACS (Average Combat Score), KAST (Kill Assistance Survive and Trading), KD (Kill/Death) e ADR (Average Damage per Round), em seguida é feita a média do time como um todo para cada estatística e armazenada em uma lista. Esse processo se repete para todas as páginas de partidas que forem socilitadas no começo do script. Cada página de confrontos do site vlr tem aproximadamente 50 partidas. No final de todo esse processo será exportado um csv em que cada linha conterá as médias dos dois times e o resultado da partida.

Atualmente foram catalogadas 848 partidas de diferentes competições por meio do site vlr.gg que estão dispostas no arquivo partidas_5.csv.

## 02_rede_neural.R
Script onde é carregado o arquivo csv com todos os jogos catalogados e passado para um dataframe. Após isso, é feita uma divisão de aproximadamente 70% (**617** partidas) desses dados para base de treinamento e 30% (**231** partidas) desses dados para base de teste. É feita uma rede neural e conforme resultados obtidos será possível guardar uma rede neural para carregar posteriormente com os mesmos parâmetros.

Das 231 partidas usadas na base de teste **181** foram previstas corretamente e **50** incorretamente, registrando uma acurácia de **80%**.

## global.R
Arquivo utilizado para deploy de um app no qual é possível informar o url e receber como retorno o resultado final da rede neural para aquela partida em específico.

## ui.R
Faz parte do deploy do shinyapp. Nesse script é possível alterar a forma como a página é apresentada ao usuário.

## server.R
A outra parte do deploy, responsável pela parte "backend". Contém script que faz com que seja possível rodar a rede neural no servidor e receber como resposta a predição da rede neural.

Link do app: 

## [Valorant Prediction](https://jrff.shinyapps.io/scripts/)

---------------------------------
###### English:

# Valorant
Using Machine Learning to predict match results from the competitive Valorant scene.

![image](https://user-images.githubusercontent.com/94936578/229330663-a3b5a254-9ae6-4c3a-9488-978eb94665a1.png)

## 01_jogos.R
This script contains all the codes used to collect player data on the *vlr.gg* site, it also contains codes that clean this data, organize and filter it as needed by the project. Player data is obtained through match urls. The individual statistics of R (Rating), ACS (Average Combat Score), KAST (Kill Assistance Survive and Trading), KD (Kill/Death) and ADR (Average Damage per Round) are averaged, then averaged of time as a whole for each statistic and stored in a list. This process is repeated for all departure pages that were requested at the beginning of the script. Each vlr site matches page has approximately 50 matches. At the end of this whole process, a csv will be exported in which each line will contain the averages of the two times and the result of the match.

Currently, 848 matches from different competitions have been cataloged through the site vlr.gg, which are arranged in the archive partidas_5.csv.

## 02_rede_neural.R
Script where the csv file with all cataloged games is loaded and passed to a dataframe. After that, a division of approximately 70% (**617** matches) of these data is made for the training base and 30% (**231** matches) of these data for the test base. A neural network is made and according to the results obtained, it will be possible to save a neural network to load later with the same parameters.

Of the 231 matches used in the test base **181** were predicted correctly and **50** incorrectly, registering an accuracy of **80%**.

## global.R
File used to deploy an app in which it is possible to inform the url and receive the final result of the neural network for that specific game as a return.

## ui.R
It's part of the shinyapp deploy. In this script it is possible to change the way the page is presented to the user.

## server.R
The other part of the deploy, responsible for the "backend" part. It contains a script that makes it possible to run the neural network on the server and receive the neural network prediction as a response.

App link:

## [Valorant Prediction](https://jrff.shinyapps.io/scripts/)
