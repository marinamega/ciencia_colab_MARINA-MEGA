### Disciplina Base de dados e colaboração
## Juntando as tabelas
setwd('C:/Users/marin/Documents/Mestrado/Disciplinas/Base de dados e colaboração')

library(tidyr)
library(dplyr)
library(tidyverse)
library(readxl)

#Importar as tabelas
df_mm <- read.csv("atividade1_MARINA-MEGA.csv", sep=";")
df_ac <- read.csv("atividade1_ANACLARA.csv")
df_nm <- read.csv("atividade1_NATALIA-MELO.csv", sep=",")
df_mf <- read.csv("atividade1_Mariana_Faitanin.csv", sep=",")
df_vl <- read.csv("atividade1_VICTOR-LUPINACCI.csv", sep=",")
df_le <- read.csv("atividade1_LETICIA-EVANGELISTA.csv", sep=",")
df_lv <- read.csv("atividade1_LuizaVieira.csv", sep=",")


#### Padronizando os dataframes ####

## ANA CLARA ##
#Mudar sequência
df_ac_ok <- df_ac[ ,c(1,2,4,5,6,7,3,8,9,10,11)]

#Renomear colunas
names(df_ac_ok) <- c('sample_card', 'sample_pdf', 'site', 'latitude', 'longitude', 'date', 
                   'specie', 'sepal_lenght_cm', 'sepal_width_cm', 'petal_lenght_cm', 'petal_width_cm')
#site
df_ac_ok$site <- sub("Site1", "1", df_ac_ok$site)
df_ac_ok$site <- sub("Site2", "2", df_ac_ok$site)
df_ac_ok$site <- sub("Site3", "3", df_ac_ok$site)

#data
df_ac_ok$date <- sub('1929-12-01', "01_12_1929", df_ac_ok$date)
df_ac_ok$date <- sub('1929-12-02', "02_12_1929", df_ac_ok$date)



## NATHÁLIA MELO ##
#Mudar sequência
df_nm_ok <- df_nm[ ,c(1,6,2,3,4,5,7,8,9,10,11)]

#Renomear colunas
names(df_nm_ok) <- c('sample_card', 'sample_pdf', 'site', 'latitude', 'longitude', 'date', 
                  'specie', 'sepal_lenght_cm', 'sepal_width_cm', 'petal_lenght_cm', 'petal_width_cm')
#site
df_nm_ok$site <- sub("site_1", "1", df_nm_ok$site)
df_nm_ok$site <- sub("site_2", "2", df_nm_ok$site)
df_nm_ok$site <- sub("site_3", "3", df_nm_ok$site)

#specie
df_nm_ok$specie <- sub("iris_virginica", "Iris virginica", df_nm_ok$specie)
df_nm_ok$specie <- sub("iris_versicolor", "Iris versicolor", df_nm_ok$specie)
df_nm_ok$specie <- sub("iris_setosa", "Iris setosa", df_nm_ok$specie)



## MARIANA FAITANIN ##
#Unindo colunas de data (ano, mês, dia)
df_mf$date <- paste(df_mf$Dia,df_mf$Mês, df_mf$Ano, sep = "_")

#Removendo colunas
df_mf_ok<- df_mf[-(3:5)]
df_mf_ok<- df_mf_ok[-(4:5)]

#Mudar sequência
df_mf_ok <- df_mf_ok[ ,c(2,1,3,5,6,11,4,7,8,9,10)]

#Renomear colunas
names(df_mf_ok) <- c('sample_card', 'sample_pdf', 'site', 'latitude', 'longitude', 'date', 
                   'specie', 'sepal_lenght_cm', 'sepal_width_cm', 'petal_lenght_cm', 'petal_width_cm')
#site
df_mf_ok$site <- sub("Site1", "1", df_mf_ok$site)
df_mf_ok$site <- sub("Site2", "2", df_mf_ok$site)
df_mf_ok$site <- sub("Site3", "2", df_mf_ok$site)

#data
df_mf_ok$date <- sub('1-12-1929', "01_12_1929", df_mf_ok$date)
df_mf_ok$date <- sub("13-2-1930", "13_02_1930", df_mf_ok$date)



## VICTOR LUPINACCI ##

#removendo os NA's
df_vl <- na.omit(df_vl)

#Unindo colunas de data (ano, mês, dia)
df_vl$date <- paste(df_vl$Dia,df_vl$Mes, df_vl$Ano, sep = "_")

#Removendo colunas
df_vl_ok<- df_vl[-(7:9)]

#Mudar sequência
df_vl_ok <- df_vl_ok[ ,c(2,1,4,5,6,11,3,7,8,9,10)]

#Renomear colunas
names(df_vl_ok) <- c('sample_card', 'sample_pdf', 'site', 'latitude', 'longitude', 'date', 
                   'specie', 'sepal_lenght_cm', 'sepal_width_cm', 'petal_lenght_cm', 'petal_width_cm')

#specie
df_vl_ok$specie <- sub("Iris_virginica", "Iris virginica", df_vl_ok$specie)
df_vl_ok$specie <- sub("Iris_versicolor", "Iris versicolor", df_vl_ok$specie)
df_vl_ok$specie <- sub("Iris_setosa", "Iris setosa", df_vl_ok$specie)



## LETICIA EVAGELISTA ### 
#Mudar sequência
df_le_ok <- df_le[ ,c(1,2,8,9,10,11,3,4,5,6,7)]

#Renomear colunas
names(df_le_ok) <- c('sample_card', 'sample_pdf', 'site', 'latitude', 'longitude', 'date', 
                  'specie', 'sepal_lenght_cm', 'sepal_width_cm', 'petal_lenght_cm', 'petal_width_cm')
#site
df_le_ok$site <- sub("Site1", "1", df_le_ok$site)
df_le_ok$site <- sub("Site2", "2", df_le_ok$site)
df_le_ok$site <- sub("Site3", "3", df_le_ok$site)

#data
df_le_ok$date <- sub("1930-02-13", "13_02_1930", df_le_ok$date)
df_le_ok$date <- sub("1929-12-01", "01_12_1929", df_le_ok$date)


## LUIZA VIEIRA ###
#Mudar sequ?ncia
df_lv_ok <- df_lv[ ,c(1,2,4,5,6,7,3,8,9,10,11)]

#Renomear colunas
names(df_lv_ok) <- c('sample_card', 'sample_pdf', 'site', 'latitude', 'longitude', 'date', 
                     'specie', 'sepal_lenght_cm', 'sepal_width_cm', 'petal_lenght_cm', 'petal_width_cm')
#site
df_lv_ok$site <- sub("Site1", "1", df_lv_ok$site)
df_lv_ok$site <- sub("Site2", "2", df_lv_ok$site)
df_lv_ok$site <- sub("Site3", "3", df_lv_ok$site)

#specie
df_lv_ok$specie <- sub("Iris_virginica", "Iris virginica", df_lv_ok$specie)
df_lv_ok$specie <- sub("Iris_versicolor", "Iris versicolor", df_lv_ok$specie)
df_lv_ok$specie <- sub("Iris_setosa", "Iris setosa", df_lv_ok$specie)

#data
df_lv_ok$date <- sub("13/02/1930", "13_02_1930", df_lv_ok$date)
df_lv_ok$date <- sub("01/12/1929", "01_12_1929", df_lv_ok$date)



## MARINA MEGA ##
#Colocando letra maiúscula 
df_mm$sample_pdf <- sub("a", "A", df_mm$sample_pdf)
df_mm$sample_card <- sub("a", "A", df_mm$sample_card)

df_mm_ok <- df_mm

#Renomear colunas
names(df_mm_ok) <- c('sample_card', 'sample_pdf', 'site', 'latitude', 'longitude', 'date', 
                     'specie', 'sepal_lenght_cm', 'sepal_width_cm', 'petal_lenght_cm', 'petal_width_cm')

#data
## achei o da nathália melhor que o meu, então aqui estou padronizando a MINHA data para a DELA
df_mm_ok$date <- sub('01/12/1929', "01_12_1929", df_mm_ok$date)
df_mm_ok$date <- sub("13/02/1930", "13_02_1930", df_mm_ok$date)


#### Juntando as planilhas ####
all_data<- rbind(df_ac_ok, df_le_ok, df_lv_ok, df_mf_ok, df_mm_ok, df_nm_ok, df_vl_ok)


write.csv(all_data, "atividade1_tudo_MarinaMega.csv", row.names = FALSE) 
