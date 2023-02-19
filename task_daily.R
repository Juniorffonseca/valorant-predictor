library(taskscheduleR)
# Define a tarefa
catalogacao_diaria <- taskscheduler_create(taskname = 'Catalogar_partidas_diariamente',
                             rscript = "C:/Users/anonb/Documents/TCC_Pós/Scripts/catalogacao_diaria.R",
                             schedule = "DAILY",
                             starttime = "00:00")
# Agendar a tarefa
taskscheduler_add(catalogacao_diaria)
