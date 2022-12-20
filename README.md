# Valorant
 Usando Aprendizado de Máquina para prever resultados de partidas do cenário competitivo de Valorant.


# Organização
Arquivos com números antes do nome (01_exemplo.R) demonstram a ordem que deve ser seguidas na execução dos scripts. No caso de números repetidos é quando há duas ou mais possibilidades de script que podem ser usados nessa etapa, dependendo da sua finalidade ao rodar os códigos.

# 01_jogadores_vlr.R
Esse script contém todos os códigos utilizados para coletar os dados dos jogadores no site vlr.gg, contém ainda, códigos que limpam esses dados, organizam e filtram conforme a necessidade do projeto. Deixando apenas jogadores com times, tirando jogadores que não tenham um número minimo específico de rounds 

# 02_jogos.R
Com o intuito de obter uma base de dados do histórico de partidas para usar como input na rede neural, há esse script que utiliza do arquivo csv gerado pelo script 01_jogadores_vlr.R para buscar os jogadores de cada time especifico e mesclar em dataframes dos times individualmente. 

Após isso, é realizada uma análise desses dados de forma a retirar aquelas informações que não são tão relevantes para aplicar na rede neural. São feitas médias de R (Rating), ACS (Average Combat Score), KAST (Kill Assistance Survive and Trading), KD (KILL/DEATH) e ADR (Average Damage per Round) do time como um todo e todas essas médias são comparadas em cada confronto e resultam em um novo dataframe que será exportado como arquivo csv.

[VCT Champions 2022](https://www.vlr.gg/event/1015/valorant-champions-2022) 

[Game Changers 2022](https://www.vlr.gg/event/1092/champions-tour-game-changers-championship-berlin)

# Diretórios e pastas
Ao rodar partes dos scripts que tenham diretórios especificos (tal como: "C:/Users/anonb/Documents/TCC Pós/Scripts/scripts_times_gc/ds_adversarios_g2.csv") é necessário trocar as informações das pastas de acordo como estiver organizado dentro do ambiente em que você estiver usando os scripts, colocando o caminho corretamente para a pasta ou arquivo específico.

# Pastas scripts_times_champions e scripts_times_gc
Essas duas pastas contém apenas arquivos referentes às duas competições, sendo scripts e arquivos com formato csv.