###### Português (BR):
# Valorant
Usando Aprendizado de Máquina para prever resultados de partidas do cenário competitivo de Valorant.

![image](https://user-images.githubusercontent.com/94936578/229330663-a3b5a254-9ae6-4c3a-9488-978eb94665a1.png)

## catalogacao_diaria.R
Script executado diariamente para catalogar as partidas que serão utilizadas no treinamento da rede neural. Criei a execução diária por meio de Addin do R chamado Schedule R scripts on Windows (também seria possível fazer manualmente por meio do windows).

## 01_rede_neural.R
Esse script carrega todas as partidas que foram catalogadas, levando em conta a primeira data sendo '2023-02-19', data que pode ser alterado conforme a necessidade, cria a rede neural após achar uma acurácia acima da determinada no laço while e mostra alguns atributos como F1 score, Matriz de confusão, Curva ROC, AUC, acúracia total, distribuição dos resultados e outros.

## global.R
Arquivo utilizado para deploy de um app no qual é possível informar o url e receber como retorno o resultado final da rede neural para aquela partida em específico.

## ui.R
Faz parte do deploy do shinyapp. Nesse script é possível alterar a forma como a página é apresentada ao usuário.

## server.R
A outra parte do deploy, responsável pela parte "backend". Contém script que faz com que seja possível rodar a rede neural no servidor e receber como resposta a predição da rede neural.

## Distribuição dos resultados de acordo com a porcentagem atribuida:
![image](https://user-images.githubusercontent.com/94936578/231922013-8671ad7a-2883-4a4e-81cc-628cab5e1359.png)


Gráfico representando as probabilidades do time 1 ganhar baseadas na saída da rede neural. O vermelho representa partidas em que o time 1 não ganhou. O verde as que ganhou. É possível notar que os picos de densidades ocorrem antes e após o 50%, representando que quando a probabilidade era maior que 50%, na maioria dos casos, os times realmente ganharam e quando era menor que 50% eles perderam.

## Metodologia
![image](https://user-images.githubusercontent.com/94936578/231924446-3f319e07-02f4-45fd-9c5e-680a4297011a.png)

## Matriz de confusão
![matriz_de_confusao](https://user-images.githubusercontent.com/94936578/231940287-e96ea578-3e27-4c47-a356-6835cb7315c2.jpeg)

## Curva ROC
![curva_ROC](https://user-images.githubusercontent.com/94936578/231940421-f0a1f641-ef1c-467c-b6be-f4ef937090fa.jpeg)

## Plot da rede neural
![rede_neural](https://user-images.githubusercontent.com/94936578/231940654-65220b3f-1ac0-4f17-a3c1-2466dd8da8c7.jpeg)

Link do app: 

## [Valorant Prediction](https://jrff.shinyapps.io/scripts/)

---------------------------------
###### English:

# Valorant
Using Machine Learning to predict match results from the competitive Valorant scene.

![image](https://user-images.githubusercontent.com/94936578/229330663-a3b5a254-9ae6-4c3a-9488-978eb94665a1.png)

## catalogacao_diaria.R
Script executed daily to catalog the matches that will be used in neural network training. I created the daily execution through an R addin called Schedule R scripts on Windows (it would also be possible to do it manually through windows).

## 01_rede_neural.R
This script loads all matches that were cataloged, taking into account the first date being '2023-02-19', a date that can be changed as needed, creates the neural network after finding an accuracy above that determined in the while loop and shows some attributes such as F1 score, confusion matrix, ROC curve, AUC, total accuracy, distribution of results and others.

## global.R
File used to deploy an app in which it is possible to inform the url and receive the final result of the neural network for that specific game as a return.

## ui.R
It's part of the shinyapp deploy. In this script it is possible to change the way the page is presented to the user.

## server.R
The other part of the deploy, responsible for the "backend" part. It contains a script that makes it possible to run the neural network on the server and receive the neural network prediction as a response.

## Distribution of results according to the assigned percentage:
![image](https://user-images.githubusercontent.com/94936578/231922013-8671ad7a-2883-4a4e-81cc-628cab5e1359.png)



Graph representing the probabilities of team 1 to win based on the output of the neural network. Red represents matches in which team 1 did not win. The green ones won. It is possible to notice that the density peaks occur before and after 50%, representing that when the probability was greater than 50%, in most cases, the teams actually won and when it was less than 50%, they lost.

## Confusion matrix
![matriz_de_confusao](https://user-images.githubusercontent.com/94936578/231940287-e96ea578-3e27-4c47-a356-6835cb7315c2.jpeg)

## Methodology
![image](https://user-images.githubusercontent.com/94936578/231924435-03616c79-5f8c-4ee4-8b07-0aebd9178b73.png)

## ROC Curve
![curva_ROC](https://user-images.githubusercontent.com/94936578/231940421-f0a1f641-ef1c-467c-b6be-f4ef937090fa.jpeg)

## Neural Network Plot
![rede_neural](https://user-images.githubusercontent.com/94936578/231940624-5e4b778d-fa26-4333-af12-ff60c79bef4b.jpeg)

App link:

## [Valorant Prediction](https://jrff.shinyapps.io/scripts/)
