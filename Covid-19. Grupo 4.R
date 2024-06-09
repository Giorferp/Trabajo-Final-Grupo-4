#Llamado a librerias

library(tidyverse)
library(dplyr)
library(readr)
Covid_G4_Base_de_Datos <- read_csv("BBDD/Covid G4 Base de Datos.csv")
#Cambio de nombre a la base de datos

df <- Covid_G4_Base_de_Datos

#Visializacion de variables en la Base de Datos 

str(df)

#Visualizacion de los paises que se tienen en el df

Lista_Paises_Continente <- df %>% 
  group_by(continent)%>% summarize (Paises= paste(unique(location),collapse = ","))

#Seleccion de los paises a estudiar 
#Ver informacion de United States, Russia, Germany, China,
#Cuba, United Kingdom, Spain, Turkey, Iran and India

df [df$location=="United States",]
df [df$location== "Russia",]
df [df$location== "Germany",]
df [df$location== "China",]
df [df$location== "Cuba",]
df [df$location== "United Kingdom",]
df [df$location== "Spain",]
df [df$location== "Turkey",]
df [df$location== "Iran",]
df [df$location== "India",]

#Se crean nuevos df para visualizar mejor la informacion
#Los df seran por pais 

US <- data.frame(df [df$location=="United States",])
RUS <- data.frame(df [df$ location == "Russia",])
CHN <- data.frame(df [ df$ location == "China",])
DEU <- data.frame(df [df$ location== "Germany",])
CUB <- data.frame(df [df$ location == "Cuba",])
ESP <- data.frame(df [df$ location == "Spain",])
GBR <- data.frame(df [df$ location == "United Kingdom",])
TUR <- data_frame(df [df$location == "Turkey",])
IRN <- data_frame(df [df$location == "Iran",])
IND <- data_frame(df [df$location == "India",])

#Se procede a describir cada df

str (US)
str (RUS)
str (CHN)
str (DEU)
str (CUB)
str (GBR)
str (TUR)
str (IRN)
str (IND)

#Se decide hacer un ajuste en la informacion de cada pais
#Por lo que se deja solo la informacion util para la investigacion

DfCHN <- CHN %>% select(index, iso_code, continent, location, date, 
                        total_cases, new_cases, new_cases_per_million,
                        total_cases_per_million, 
                        total_deaths, new_deaths, new_deaths_per_million, 
                        total_deaths_per_million, 
                        total_vaccinations, new_vaccinations, 
                        total_vaccinations_per_hundred, people_vaccinated, 
                        people_fully_vaccinated, people_vaccinated_per_hundred, 
                        people_fully_vaccinated_per_hundred, 
                        population_density, population, median_age)

DfCUB <- CUB %>% select(index, iso_code, continent, location, date, 
                        total_cases, new_cases, new_cases_per_million,
                        total_cases_per_million, 
                        total_deaths, new_deaths, new_deaths_per_million, 
                        total_deaths_per_million,
                        total_vaccinations, new_vaccinations, 
                        total_vaccinations_per_hundred, people_vaccinated, 
                        people_fully_vaccinated, people_vaccinated_per_hundred, 
                        people_fully_vaccinated_per_hundred, 
                        population_density, population, median_age)

DfDEU <- DEU %>% select(index, iso_code, continent, location, date, 
                        total_cases, new_cases, new_cases_per_million,
                        total_cases_per_million, 
                        total_deaths, new_deaths, new_deaths_per_million, 
                        total_deaths_per_million, 
                        total_vaccinations, new_vaccinations, 
                        total_vaccinations_per_hundred, people_vaccinated, 
                        people_fully_vaccinated, people_vaccinated_per_hundred, 
                        people_fully_vaccinated_per_hundred, 
                        population_density, population, median_age)

DfESP <- ESP %>% select(index, iso_code, continent, location, date, 
                        total_cases, new_cases, new_cases_per_million,
                        total_cases_per_million, 
                        total_deaths, new_deaths, new_deaths_per_million, 
                        total_deaths_per_million,
                        total_vaccinations, new_vaccinations, 
                        total_vaccinations_per_hundred, people_vaccinated, 
                        people_fully_vaccinated, people_vaccinated_per_hundred, 
                        people_fully_vaccinated_per_hundred, 
                        population_density, population, median_age)

DfGBR <- GBR %>% select(index, iso_code, continent, location, date, 
                        total_cases, new_cases, new_cases_per_million,
                        total_cases_per_million, 
                        total_deaths, new_deaths, new_deaths_per_million, 
                        total_deaths_per_million,
                        total_vaccinations, new_vaccinations, 
                        total_vaccinations_per_hundred, people_vaccinated, 
                        people_fully_vaccinated, people_vaccinated_per_hundred, 
                        people_fully_vaccinated_per_hundred, 
                        population_density, population, median_age)

DfIND <- IND %>% select(index, iso_code, continent, location, date, 
                        total_cases, new_cases, new_cases_per_million,
                        total_cases_per_million, 
                        total_deaths, new_deaths, new_deaths_per_million, 
                        total_deaths_per_million,
                        total_vaccinations, new_vaccinations, 
                        total_vaccinations_per_hundred, people_vaccinated, 
                        people_fully_vaccinated, people_vaccinated_per_hundred, 
                        people_fully_vaccinated_per_hundred, 
                        population_density, population, median_age)

DfIRN <- IRN %>% select(index, iso_code, continent, location, date, 
                        total_cases, new_cases, new_cases_per_million,
                        total_cases_per_million, 
                        total_deaths, new_deaths, new_deaths_per_million, 
                        total_deaths_per_million, 
                        total_vaccinations, new_vaccinations, 
                        total_vaccinations_per_hundred, people_vaccinated, 
                        people_fully_vaccinated, people_vaccinated_per_hundred, 
                        people_fully_vaccinated_per_hundred, 
                        population_density, population, median_age)

DfRUS <- RUS %>% select(index, iso_code, continent, location, date, 
                        total_cases, new_cases, new_cases_per_million,
                        total_cases_per_million, 
                        total_deaths, new_deaths, new_deaths_per_million, 
                        total_deaths_per_million, 
                        total_vaccinations, new_vaccinations, 
                        total_vaccinations_per_hundred, people_vaccinated, 
                        people_fully_vaccinated, people_vaccinated_per_hundred, 
                        people_fully_vaccinated_per_hundred, 
                        population_density, population, median_age)

DfTUR <- TUR %>% select(index, iso_code, continent, location, date, 
                        total_cases, new_cases, new_cases_per_million,
                        total_cases_per_million, 
                        total_deaths, new_deaths, new_deaths_per_million, 
                        total_deaths_per_million, 
                        total_vaccinations, new_vaccinations, 
                        total_vaccinations_per_hundred, people_vaccinated, 
                        people_fully_vaccinated, people_vaccinated_per_hundred, 
                        people_fully_vaccinated_per_hundred, 
                        population_density, population, median_age)

DfUS <- US %>% select(index, iso_code, continent, location, date, 
                      total_cases, new_cases, new_cases_per_million,
                      total_cases_per_million, 
                      total_deaths, new_deaths, new_deaths_per_million, 
                      total_deaths_per_million, 
                      total_vaccinations, new_vaccinations, 
                      total_vaccinations_per_hundred, people_vaccinated, 
                      people_fully_vaccinated, people_vaccinated_per_hundred, 
                      people_fully_vaccinated_per_hundred, 
                      population_density, population, median_age)


#La variable de "nuevos vacunados por cien" no existe


#Uso de "summary" para visualizar informacion estadistica de cada pais


summary(DfCHN, na.rm = TRUE)
summary(DfCUB, na.rm = TRUE)
summary(DfDEU, na.rm = TRUE)
summary(DfESP, na.rm = TRUE)
summary(DfGBR, na.rm = TRUE)
summary(DfIND, na.rm = TRUE)
summary(DfIRN, na.rm = TRUE)
summary(DfRUS, na.rm = TRUE)
summary(DfTUR, na.rm = TRUE)
summary(DfUS, na.rm = TRUE)


#Alugunos datos estadisticos de US por cada año

summary (DfUS [DfUS$date<"2021-01-01",], na.rm = TRUE)
summary (DfUS [365:729,], na.rm = TRUE)
summary (DfUS [730:1094,], na.rm = TRUE)
summary (DfUS [DfUS$date>"2022-12-31",], na.rm = TRUE)

#Alugunos datos estadisticos de DEU por año
summary (DfDEU [DfDEU$date<"2021-01-01",], na.rm = TRUE)
summary (DfDEU [365:729,], na.rm = TRUE)
summary (DfDEU [730:1094,], na.rm = TRUE)
summary (DfDEU [DfDEU$date>"2022-12-31",], na.rm = TRUE)

#Algunos datos estadisticos de TUR por año

summary (DfTUR [DfTUR$date<"2021-01-01",], na.rm = TRUE)
summary (DfTUR [365:729,], na.rm = TRUE)
summary (DfTUR [730:1094,], na.rm = TRUE)
summary (DfTUR [DfTUR$date>"2022-12-31",], na.rm = TRUE)

#Valores NA de US

apply(DfUS [DfUS$date<"2021-01-01",], MARGIN = 2, function(x) sum(is.na(x))) %>% view()
#En 2020 los datos NA estan en los primeros dias del registro
#Hasta el primer contagio, el primer deceso, primer vacunado etc
#Parece existir un error en New Vaccinations
apply(DfUS [365:729,], MARGIN = 2, function(x) sum(is.na(x))) %>% view()
#No existen datos NA en 2021
apply(DfUS [730:1094,], MARGIN = 2, function(x) sum(is.na(x))) %>% view()
#En el 2022-03-16 falta el valor de New Deaths
apply(DfUS [DfUS$date>"2022-12-31",], MARGIN = 2, function(x) sum(is.na(x))) %>% view()
#No hay valores de vaccinations

###Rellenar valores faltantes de US por año

DfUS20 <- data.frame (DfUS [DfUS$date<"2021-01-01",])
DfUS20 <-DfUS20 %>% mutate(total_cases = if_else(is.na(total_cases), 0, total_cases))
DfUS20 <-DfUS20 %>% mutate (total_cases_per_million = if_else
                            (is.na(total_cases_per_million), 0, total_cases_per_million))
DfUS20 <-DfUS20 %>% mutate (total_deaths = if_else
                            (is.na(total_deaths), 0, total_deaths))
DfUS20 <-DfUS20 %>% mutate (total_deaths_per_million = if_else
                            (is.na(total_deaths_per_million), 0, total_deaths_per_million))
DfUS20 <-DfUS20 %>% mutate (total_vaccinations = if_else
                            (is.na(total_vaccinations), 0, total_vaccinations))
DfUS20 <-DfUS20 %>% mutate (new_vaccinations = if_else
                            (is.na(new_vaccinations), 0, new_vaccinations))
DfUS20 <-DfUS20 %>% mutate (total_vaccinations_per_hundred = if_else
                            (is.na(total_vaccinations_per_hundred), 0, total_vaccinations_per_hundred))
DfUS20 <-DfUS20 %>% mutate (people_vaccinated = if_else
                            (is.na(people_vaccinated), 0, people_vaccinated))
DfUS20 <-DfUS20 %>% mutate (people_fully_vaccinated = if_else
                            (is.na(people_fully_vaccinated), 0, people_fully_vaccinated))
DfUS20 <-DfUS20 %>% mutate (people_vaccinated_per_hundred = if_else
                            (is.na(people_vaccinated_per_hundred), 0, people_vaccinated_per_hundred))
DfUS20 <-DfUS20 %>% mutate (people_fully_vaccinated_per_hundred = if_else
                            (is.na(people_fully_vaccinated_per_hundred), 0, people_fully_vaccinated_per_hundred))
#Dia 346
DfUS20 [346,15] <- 45620

#Verificacion de que no existan datos NA en US20
apply(DfUS20, MARGIN = 2, function(x) sum(is.na(x))) %>% view()



DfUS21 <- data_frame (DfUS [365:729,])


DfUS22 <-data.frame (DfUS [730:1094,])
apply(DfUS22, MARGIN = 2, function(x) sum(is.na(x))) %>% view()
DfUS23 <- data.frame(DfUS [DfUS$date>"2022-12-31",])


#Valores NA de DEU

apply(DfDEU [DfDEU$date<"2021-01-01",], MARGIN = 2, function(x) sum(is.na(x))) %>% view()
apply(DfDEU [365:729,], MARGIN = 2, function(x) sum(is.na(x))) %>% view()
apply(DfDEU [730:1094,], MARGIN = 2, function(x) sum(is.na(x))) %>% view()
apply(DfDEU [DfDEU$date>"2022-12-31",], MARGIN = 2, function(x) sum(is.na(x))) %>% view()

#Rellenar valores NA de DEU por año

DfDEU20 <- data.frame(DfDEU [DfDEU$date<"2021-01-01",])
DfDEU20 <-DfDEU20 %>% mutate(total_cases = if_else(is.na(total_cases), 0, total_cases))
DfDEU20 <-DfDEU20 %>% mutate (total_cases_per_million = if_else
                              (is.na(total_cases_per_million), 0, total_cases_per_million))
DfDEU20 <-DfDEU20 %>% mutate (total_deaths = if_else
                              (is.na(total_deaths), 0, total_deaths))
DfUS20 <-DfUS20 %>% mutate (total_deaths = if_else
                            (is.na(total_deaths), 0, total_deaths))
DfDEU20 <-DfDEU20 %>% mutate (total_deaths_per_million = if_else
                              (is.na(total_deaths_per_million), 0, total_deaths_per_million))
DfDEU20 <-DfDEU20 %>% mutate (people_fully_vaccinated = if_else
                              (is.na(people_fully_vaccinated), 0, people_fully_vaccinated))
DfDEU20 <-DfDEU20 %>% mutate (people_fully_vaccinated_per_hundred = if_else
                              (is.na(people_fully_vaccinated_per_hundred), 0, people_fully_vaccinated_per_hundred))


#Verificacion de que no existan datos NA en DEU20
apply(DfDEU20, MARGIN = 2, function(x) sum(is.na(x))) %>% view()
#Algunos valores NA podrian ser ceros, no seria correcto rellenar con media o mediana
DfDEU20 [1:24,6:20] <- 0
DfDEU20 [25,6:7] <- 1
DfDEU20 [26,7] <- 1
DfDEU20 [25:26,8] <- 0.011
DfDEU20 [25,9] <- 0.011
DfDEU20 [1:359,14:20] <- 0
DfDEU20 [360,15] <- 24427

DfDEU21 <- data.frame(DfDEU [365:729,])
DfDEU21 <-DfDEU21 %>% mutate (people_fully_vaccinated = if_else
                              (is.na(people_fully_vaccinated), 0, people_fully_vaccinated))
DfDEU21 <-DfDEU21 %>% mutate (people_fully_vaccinated_per_hundred = if_else
                              (is.na(people_fully_vaccinated_per_hundred), 0, people_fully_vaccinated_per_hundred))

DfDEU22 <- data.frame(DfUS [730:1094,])
DfUS22 <-DfUS22 %>% mutate (new_deaths = if_else
                            (is.na(new_deaths), 2437, new_deaths))
DfUS22 <-DfUS22 %>% mutate (new_deaths_per_million = if_else
                            (is.na(new_deaths_per_million), 7.203, new_deaths_per_million))


DfDEU23 <- data.frame(DfDEU [DfDEU$date>"2022-12-31",])
DfDEU23 [172,7:8] <- 0

#Valores NA de TUR

apply(DfTUR [DfTUR$date<"2021-01-01",], MARGIN = 2, function(x) sum(is.na(x))) %>% view()
apply(DfTUR [365:729,], MARGIN = 2, function(x) sum(is.na(x))) %>% view()
apply(DfTUR [730:1094,], MARGIN = 2, function(x) sum(is.na(x))) %>% view()
apply(DfTUR [DfTUR$date>"2022-12-31",], MARGIN = 2, function(x) sum(is.na(x))) %>% view()

#Rellenar valores NA de TUR por año

DfTUR20 <- data.frame(DfTUR [DfTUR$date<"2021-01-01",])

DfTUR20 <-DfTUR20 %>% mutate(total_cases = if_else(is.na(total_cases), 0, total_cases))
DfTUR20 <-DfTUR20 %>% mutate (total_cases_per_million = if_else
                              (is.na(total_cases_per_million), 0, total_cases_per_million))
DfTUR20 <-DfTUR20 %>% mutate (total_deaths = if_else
                              (is.na(total_deaths), 0, total_deaths))
DfTUR20 <-DfTUR20 %>% mutate (total_deaths_per_million = if_else
                              (is.na(total_deaths_per_million), 0, total_deaths_per_million))
DfTUR20 <-DfTUR20 %>% mutate (total_vaccinations = if_else
                              (is.na(total_vaccinations), 0, total_vaccinations))
DfTUR20 <-DfTUR20 %>% mutate (new_vaccinations = if_else
                              (is.na(new_vaccinations), 0, new_vaccinations))
DfTUR20 <-DfTUR20 %>% mutate (total_vaccinations_per_hundred = if_else
                              (is.na(total_vaccinations_per_hundred), 0, total_vaccinations_per_hundred))
DfTUR20 <-DfTUR20 %>% mutate (people_vaccinated = if_else
                              (is.na(people_vaccinated), 0, people_vaccinated))
DfTUR20 <-DfTUR20 %>% mutate (people_fully_vaccinated = if_else
                              (is.na(people_fully_vaccinated), 0, people_fully_vaccinated))
DfTUR20 <-DfTUR20 %>% mutate (people_vaccinated_per_hundred = if_else
                              (is.na(people_vaccinated_per_hundred), 0, people_vaccinated_per_hundred))
DfTUR20 <-DfTUR20 %>% mutate (people_fully_vaccinated_per_hundred = if_else
                              (is.na(people_fully_vaccinated_per_hundred), 0, people_fully_vaccinated_per_hundred))

#Verificacion de que no existan datos NA en TUR20
apply(DfTUR20, MARGIN = 2, function(x) sum(is.na(x))) %>% view()
###No hay ningun dato de los Vacunados pues empezaron en 2021

DfTUR21 <- data.frame(DfTUR [365:729,])
DfTUR21 <-DfTUR21 %>% mutate (total_vaccinations = if_else
                              (is.na(total_vaccinations), 0, total_vaccinations))
DfTUR21 <-DfTUR21 %>% mutate (new_vaccinations = if_else
                              (is.na(new_vaccinations), 0, new_vaccinations))
DfTUR21 <-DfTUR21 %>% mutate (total_vaccinations_per_hundred = if_else
                              (is.na(total_vaccinations_per_hundred), 0, total_vaccinations_per_hundred))
DfTUR21 <-DfTUR21 %>% mutate (people_vaccinated = if_else
                              (is.na(people_vaccinated), 0, people_vaccinated))
DfTUR21 <-DfTUR21 %>% mutate (people_fully_vaccinated = if_else
                              (is.na(people_fully_vaccinated), 0, people_fully_vaccinated))
DfTUR21 <-DfTUR21 %>% mutate (people_vaccinated_per_hundred = if_else
                              (is.na(people_vaccinated_per_hundred), 0, people_vaccinated_per_hundred))
DfTUR21 <-DfTUR21 %>% mutate (people_fully_vaccinated_per_hundred = if_else
                              (is.na(people_fully_vaccinated_per_hundred), 0, people_fully_vaccinated_per_hundred))
#Verificacion de que no existan datos NA en TUR21
apply(DfTUR21, MARGIN = 2, function(x) sum(is.na(x))) %>% view()

DfTUR22 <- data.frame(DfTUR [730:1094,])
#Valores NA de DfTUR22
#Dia 4
DfTUR22 [4,14] <- 132897714
DfTUR22 [4,15] <- 0
DfTUR22 [4,16] <- 155.73
DfTUR22 [4,17] <- 56960334
DfTUR22 [4,18] <- 51677320
DfTUR22 [4,19] <- 66.74
DfTUR22 [4,20] <- 60.55
#Dia 5
DfTUR22 [5,15] <- (134195065 - 132897714) 
#Dia 91
DfTUR22 [91,14] <- 146995383
DfTUR22 [91,15] <- 0
DfTUR22 [91,16] <- 172.24
DfTUR22 [91,17] <- 57781315
DfTUR22 [91,18] <- 52977660
DfTUR22 [91,19] <- 67.71
DfTUR22 [91,20] <- 62.08
#Dia 92
DfTUR22 [92,14] <- 146995383
DfTUR22 [92,15] <- 0
DfTUR22 [92,16] <- 172.24
DfTUR22 [92,17] <- 57781315
DfTUR22 [92,18] <- 52977660
DfTUR22 [92,19] <- 67.71
DfTUR22 [92,20] <- 62.08
#Dia 93
DfTUR22 [93,15] <- (147070173 - 146995383) 
#Dia 105
DfTUR22 [105,14] <- 147301011
DfTUR22 [105,15] <- 0
DfTUR22 [105,16] <- 172.60
DfTUR22 [105,17] <- 57801926
DfTUR22 [105,18] <- 53010344
DfTUR22 [105,19] <- 67.73
DfTUR22 [105,20] <- 62.12
#Dia 106
DfTUR22 [106,14] <- 147301011
DfTUR22 [106,15] <- 0
DfTUR22 [106,16] <- 172.60
DfTUR22 [106,17] <- 57801926
DfTUR22 [106,18] <- 53010344
DfTUR22 [106,19] <- 67.73
DfTUR22 [106,20] <- 62.12
#Dia 107
DfTUR22 [107,15] <- (147333131 - 147301011)
#Dia 112
DfTUR22 [112,14] <- 147402858
DfTUR22 [112,15] <- 0
DfTUR22 [112,16] <- 172.72
DfTUR22 [112,17] <- 57810012
DfTUR22 [112,18] <- 53022686
DfTUR22 [112,19] <- 67.74
DfTUR22 [112,20] <- 62.13
#Dia 113
DfTUR22 [113,15] <- (147423229 - 147402858)
#Dia 140
DfTUR22 [140,14] <- 147663432
DfTUR22 [140,15] <- 0
DfTUR22 [140,16] <- 173.03
DfTUR22 [140,17] <- 57834671
DfTUR22 [140,18] <- 53055541
DfTUR22 [140,19] <- 67.77
DfTUR22 [140,20] <- 62.17
#Dia 141
DfTUR22 [141,15] <- (147677323 - 147663432)
#Del dia 152 al 163
DfTUR22 [152:163,14] <- 147734958
DfTUR22 [152:163,15] <- 0
DfTUR22 [152:163,16] <- 173.11
DfTUR22 [152:163,17] <- 57843685
DfTUR22 [152:163,18] <- 53067556
DfTUR22 [152:163,19] <- 67.78
DfTUR22 [152:163,20] <- 62.18
#Dia 164
DfTUR22 [164,15] <- (147781738 - 147734958)
#Dia 177
DfTUR22 [177,14] <- 147814534
DfTUR22 [177,15] <- 0
DfTUR22 [177,16] <- 173.20
DfTUR22 [177,17] <- 57856676
DfTUR22 [177,18] <- 53085724
DfTUR22 [177,19] <- 67.79
DfTUR22 [177,20] <- 62.20
#Dia 178
DfTUR22 [178,15] <- (147818598 - 147814534)
#Dia 182
DfTUR22 [182,14] <- 147828801
DfTUR22 [182,15] <- 0
DfTUR22 [182,16] <- 173.22
DfTUR22 [182,17] <- 57858728
DfTUR22 [182,18] <- 53088281
DfTUR22 [182,19] <- 67.80
DfTUR22 [182,20] <- 62.21
#Dia 183
DfTUR22 [183,15] <- (147833636 - 147828801)
#Dia 189
DfTUR22 [189,14] <- 147857274
DfTUR22 [189,15] <- 0
DfTUR22 [189,16] <- 173.25
DfTUR22 [189,17] <- 57861505
DfTUR22 [189,18] <- 53091777
DfTUR22 [189,19] <- 67.80
DfTUR22 [189,20] <- 62.21
#Dia 190
DfTUR22 [190,15] <- (147861309 - 147857274)
#Dia 285
DfTUR22 [285,14] <- 152381745
DfTUR22 [285,15] <- 0
DfTUR22 [285,16] <- 178.56
DfTUR22 [285,17] <- 57931643
DfTUR22 [285,18] <- 53165778
DfTUR22 [285,19] <- 67.88
DfTUR22 [285,20] <- 62.30
#Dia 286
DfTUR22 [286,15] <- (152396067 - 152381745)
#Dia 303
DfTUR22 [303,14] <- 152465212
DfTUR22 [303,15] <- 0
DfTUR22 [303,16] <- 178.65
DfTUR22 [303,17] <- 57936171
DfTUR22 [303,18] <- 53171054
DfTUR22 [303,19] <- 67.89
DfTUR22 [303,20] <- 62.30
#Dia 304 
DfTUR22[304,15] <- (152469836 - 152465212)
#Del dia 306 al 325
DfTUR22 [306:325,14] <- 152475057
DfTUR22 [306:325,15] <- 0
DfTUR22 [306:325,16] <- 178.67
DfTUR22 [306:325,17] <- 57936783
DfTUR22 [306:325,18] <- 53171790
DfTUR22 [306:325,19] <- 67.89
DfTUR22 [306:325,20] <- 62.30
#Dia 326
DfTUR22 [326,15] <- (152543341 - 152475057)

#Falta dato de vacunacion en 2022-01-04
#Faltan algunos datos de vacunaciones que podrian o no ser ceros


DfTUR23 <- data.frame(DfTUR [DfTUR$date>"2022-12-31",])
#No hay informacion sobre vacunas en 2023, podrian o no ser ceros

#Creacion de la variable new_vaccinations_per_hundred para cada pais

DfUS20 <- mutate(DfUS20, new_vaccinations_per_hundred=new_vaccinations/population*100)
DfUS21 <- mutate(DfUS21, new_vaccinations_per_hundred=new_vaccinations/population*100)
DfUS22 <- mutate(DfUS22, new_vaccinations_per_hundred=new_vaccinations/population*100)
DfUS23 <- mutate(DfUS23, new_vaccinations_per_hundred=new_vaccinations/population*100)

DfDEU20 <- mutate(DfDEU20, new_vaccinations_per_hundred=new_vaccinations/population*100)
DfDEU21 <- mutate(DfDEU21, new_vaccinations_per_hundred=new_vaccinations/population*100)
DfDEU22 <- mutate(DfDEU22, new_vaccinations_per_hundred=new_vaccinations/population*100)
DfDEU23 <- mutate(DfDEU23, new_vaccinations_per_hundred=new_vaccinations/population*100)

DfTUR20 <- mutate(DfTUR20, new_vaccinations_per_hundred=new_vaccinations/population*100)
DfTUR21 <- mutate(DfTUR21, new_vaccinations_per_hundred=new_vaccinations/population*100)
DfTUR22 <- mutate(DfTUR22, new_vaccinations_per_hundred=new_vaccinations/population*100)
DfTUR23 <- mutate(DfTUR23, new_vaccinations_per_hundred=new_vaccinations/population*100)

#Estadisticas de DfUS, DfDEU y DfTURpor año

#Estados Unidos, Casos
Casos_DfUS20 <- DfUS20 %>% summarise(Promedio_total_cases=mean(total_cases),
                                     D.Est_total_cases=sd(total_cases),
                                     CV_total_cases=sd(total_cases)/mean(total_cases)*100,
                                     Min_total_cases=min(total_cases),
                                     Max_total_cases=max(total_cases),
                                     Promedio_new_cases=mean(new_cases),
                                     D.Est_new_cases=sd(new_cases),
                                     CV_new_cases=sd(new_cases)/mean(new_cases)*100,
                                     Min_new_cases=min(new_cases),
                                     Max_new_cases=max(new_cases))

Tasa_de_casos_DfUS20 <- DfUS20 %>% summarise(Promedio_total_cases=mean(total_cases_per_million),
                                             D.Est_total_cases=sd(total_cases_per_million),
                                             CV_total_cases=
                                               sd(total_cases_per_million)/mean(total_cases_per_million)*100,
                                             Min_total_cases=min(total_cases_per_million),
                                             Max_total_cases=max(total_cases_per_million),
                                             Promedio_new_cases=mean(new_cases_per_million),
                                             D.Est_new_cases=sd(new_cases_per_million),
                                             CV_new_cases=
                                               sd(new_cases_per_million)/mean(new_cases_per_million)*100,
                                             Min_new_cases=min(new_cases_per_million),
                                             Max_new_cases=max(new_cases_per_million))


Casos_DfUS21 <- DfUS21 %>% summarise(Promedio_total_cases=mean(total_cases),
                                     D.Est_total_cases=sd(total_cases),
                                     CV_total_cases=sd(total_cases)/mean(total_cases)*100,
                                     Min_total_cases=min(total_cases),
                                     Max_total_cases=max(total_cases),
                                     Promedio_new_cases=mean(new_cases),
                                     D.Est_new_cases=sd(new_cases),
                                     CV_new_cases=sd(new_cases)/mean(new_cases)*100,
                                     Min_new_cases=min(new_cases),
                                     Max_new_cases=max(new_cases))

Tasa_de_casos_DfUS21 <- DfUS21 %>% summarise(Promedio_total_cases=mean(total_cases_per_million),
                                             D.Est_total_cases=sd(total_cases_per_million),
                                             CV_total_cases=
                                               sd(total_cases_per_million)/mean(total_cases_per_million)*100,
                                             Min_total_cases=min(total_cases_per_million),
                                             Max_total_cases=max(total_cases_per_million),
                                             Promedio_new_cases=mean(new_cases_per_million),
                                             D.Est_new_cases=sd(new_cases_per_million),
                                             CV_new_cases=
                                               sd(new_cases_per_million)/mean(new_cases_per_million)*100,
                                             Min_new_cases=min(new_cases_per_million),
                                             Max_new_cases=max(new_cases_per_million))

Casos_DfUS22 <- DfUS22 %>% summarise(Promedio_total_cases=mean(total_cases),
                                     D.Est_total_cases=sd(total_cases),
                                     CV_total_cases=sd(total_cases)/mean(total_cases)*100,
                                     Min_total_cases=min(total_cases),
                                     Max_total_cases=max(total_cases),
                                     Promedio_new_cases=mean(new_cases),
                                     D.Est_new_cases=sd(new_cases),
                                     CV_new_cases=sd(new_cases)/mean(new_cases)*100,
                                     Min_new_cases=min(new_cases),
                                     Max_new_cases=max(new_cases))

Tasa_de_casos_DfUS22 <- DfUS22 %>% summarise(Promedio_total_cases=mean(total_cases_per_million),
                                             D.Est_total_cases=sd(total_cases_per_million),
                                             CV_total_cases=
                                               sd(total_cases_per_million)/mean(total_cases_per_million)*100,
                                             Min_total_cases=min(total_cases_per_million),
                                             Max_total_cases=max(total_cases_per_million),
                                             Promedio_new_cases=mean(new_cases_per_million),
                                             D.Est_new_cases=sd(new_cases_per_million),
                                             CV_new_cases=
                                               sd(new_cases_per_million)/mean(new_cases_per_million)*100,
                                             Min_new_cases=min(new_cases_per_million),
                                             Max_new_cases=max(new_cases_per_million))

Casos_DfUS23 <- DfUS23 %>% summarise(Promedio_total_cases=mean(total_cases),
                                     D.Est_total_cases=sd(total_cases),
                                     CV_total_cases=sd(total_cases)/mean(total_cases)*100,
                                     Min_total_cases=min(total_cases),
                                     Max_total_cases=max(total_cases),
                                     Promedio_new_cases=mean(new_cases),
                                     D.Est_new_cases=sd(new_cases),
                                     CV_new_cases=sd(new_cases)/mean(new_cases)*100,
                                     Min_new_cases=min(new_cases),
                                     Max_new_cases=max(new_cases))

Tasa_de_casos_DfUS23 <- DfUS23 %>% summarise(Promedio_total_cases=mean(total_cases_per_million),
                                             D.Est_total_cases=sd(total_cases_per_million),
                                             CV_total_cases=
                                               sd(total_cases_per_million)/mean(total_cases_per_million)*100,
                                             Min_total_cases=min(total_cases_per_million),
                                             Max_total_cases=max(total_cases_per_million),
                                             Promedio_new_cases=mean(new_cases_per_million),
                                             D.Est_new_cases=sd(new_cases_per_million),
                                             CV_new_cases=
                                               sd(new_cases_per_million)/mean(new_cases_per_million)*100,
                                             Min_new_cases=min(new_cases_per_million),
                                             Max_new_cases=max(new_cases_per_million))
#Estados Unidos Vacunaciones 
Vacunas_DfUS20 <- DfUS20 %>% summarise(Promedio_total_vaccinations=mean(total_vaccinations),
                                       D.Est_total_vaccinations=sd(total_vaccinations),
                                       CV_total_vaccinations=sd(total_vaccinations)/mean(total_vaccinations)*100,
                                       Min_total_vaccinations=min(total_vaccinations),
                                       Max_total_vaccinations=max(total_vaccinations),
                                       Promedio_new_vaccinations=mean(new_vaccinations),
                                       D.Est_new_vaccinations=sd(new_vaccinations),
                                       CV_new_vaccinations=sd(new_vaccinations)/mean(new_vaccinations)*100,
                                       Min_new_vaccinations=min(new_vaccinations),
                                       Max_new_vaccinations=max(new_vaccinations))

Tasa_de_vacunas_DfUS20 <- DfUS20 %>% summarise(Promedio_total_vaccinations=mean(total_vaccinations_per_hundred),
                                               D.Est_total_vaccinations=sd(total_vaccinations_per_hundred),
                                               CV_total_vaccinations=
                                                 sd(total_vaccinations_per_hundred)/
                                                 mean(total_vaccinations_per_hundred)*100,
                                               Min_total_vaccinations=min(total_vaccinations_per_hundred),
                                               Max_total_vaccinations=max(total_vaccinations_per_hundred),
                                               Promedio_new_vaccinations=mean(new_vaccinations_per_hundred),
                                               D.Est_new_vaccinations=sd(new_vaccinations_per_hundred),
                                               CV_new_vaccinations=
                                                 sd(new_vaccinations_per_hundred)/
                                                 mean(new_vaccinations_per_hundred)*100,
                                               Min_new_vaccinations=min(new_vaccinations_per_hundred),
                                               Max_new_vaccinations=max(new_vaccinations_per_hundred))

Vacunas_DfUS21 <- DfUS21 %>% summarise(Promedio_total_vaccinations=mean(total_vaccinations),
                                       D.Est_total_vaccinations=sd(total_vaccinations),
                                       CV_total_vaccinations=sd(total_vaccinations)/mean(total_vaccinations)*100,
                                       Min_total_vaccinations=min(total_vaccinations),
                                       Max_total_vaccinations=max(total_vaccinations),
                                       Promedio_new_vaccinations=mean(new_vaccinations),
                                       D.Est_new_vaccinations=sd(new_vaccinations),
                                       CV_new_vaccinations=sd(new_vaccinations)/mean(new_vaccinations)*100,
                                       Min_new_vaccinations=min(new_vaccinations),
                                       Max_new_vaccinations=max(new_vaccinations))

Tasa_de_vacunas_DfUS21 <- DfUS21 %>% summarise(Promedio_total_vaccinations=mean(total_vaccinations_per_hundred),
                                               D.Est_total_vaccinations=sd(total_vaccinations_per_hundred),
                                               CV_total_vaccinations=
                                                 sd(total_vaccinations_per_hundred)/
                                                 mean(total_vaccinations_per_hundred)*100,
                                               Min_total_vaccinations=min(total_vaccinations_per_hundred),
                                               Max_total_vaccinations=max(total_vaccinations_per_hundred),
                                               Promedio_new_vaccinations=mean(new_vaccinations_per_hundred),
                                               D.Est_new_vaccinations=sd(new_vaccinations_per_hundred),
                                               CV_new_vaccinations=
                                                 sd(new_vaccinations_per_hundred)/
                                                 mean(new_vaccinations_per_hundred)*100,
                                               Min_new_vaccinations=min(new_vaccinations_per_hundred),
                                               Max_new_vaccinations=max(new_vaccinations_per_hundred))

Vacunas_DfUS22 <- DfUS22 %>% summarise(Promedio_total_vaccinations=mean(total_vaccinations),
                                       D.Est_total_vaccinations=sd(total_vaccinations),
                                       CV_total_vaccinations=sd(total_vaccinations)/mean(total_vaccinations)*100,
                                       Min_total_vaccinations=min(total_vaccinations),
                                       Max_total_vaccinations=max(total_vaccinations),
                                       Promedio_new_vaccinations=mean(new_vaccinations),
                                       D.Est_new_vaccinations=sd(new_vaccinations),
                                       CV_new_vaccinations=sd(new_vaccinations)/mean(new_vaccinations)*100,
                                       Min_new_vaccinations=min(new_vaccinations),
                                       Max_new_vaccinations=max(new_vaccinations))

Tasa_de_vacunas_DfUS22 <- DfUS22 %>% summarise(Promedio_total_vaccinations=mean(total_vaccinations_per_hundred),
                                               D.Est_total_vaccinations=sd(total_vaccinations_per_hundred),
                                               CV_total_vaccinations=
                                                 sd(total_vaccinations_per_hundred)/
                                                 mean(total_vaccinations_per_hundred)*100,
                                               Min_total_vaccinations=min(total_vaccinations_per_hundred),
                                               Max_total_vaccinations=max(total_vaccinations_per_hundred),
                                               Promedio_new_vaccinations=mean(new_vaccinations_per_hundred),
                                               D.Est_new_vaccinations=sd(new_vaccinations_per_hundred),
                                               CV_new_vaccinations=
                                                 sd(new_vaccinations_per_hundred)/
                                                 mean(new_vaccinations_per_hundred)*100,
                                               Min_new_vaccinations=min(new_vaccinations_per_hundred),
                                               Max_new_vaccinations=max(new_vaccinations_per_hundred))

Vacunas_DfUS23 <- DfUS23 [1:129,] %>% summarise (Promedio_total_vaccinations=mean(total_vaccinations),
                                                 D.Est_total_vaccinations=sd(total_vaccinations),
                                                 CV_total_vaccinations=sd(total_vaccinations)/mean(total_vaccinations)*100,
                                                 Min_total_vaccinations=min(total_vaccinations),
                                                 Max_total_vaccinations=max(total_vaccinations),
                                                 Promedio_new_vaccinations=mean(new_vaccinations),
                                                 D.Est_new_vaccinations=sd(new_vaccinations),
                                                 CV_new_vaccinations=sd(new_vaccinations)/mean(new_vaccinations)*100,
                                                 Min_new_vaccinations=min(new_vaccinations),
                                                 Max_new_vaccinations=max(new_vaccinations))

Tasa_de_vacunas_DfUS23 <- DfUS23 [1:129,] %>% summarise(Promedio_total_vaccinations=
                                                          mean(total_vaccinations_per_hundred),
                                                        D.Est_total_vaccinations=sd(total_vaccinations_per_hundred),
                                                        CV_total_vaccinations=
                                                          sd(total_vaccinations_per_hundred)/
                                                          mean(total_vaccinations_per_hundred)*100,
                                                        Min_total_vaccinations=min(total_vaccinations_per_hundred),
                                                        Max_total_vaccinations=max(total_vaccinations_per_hundred),
                                                        Promedio_new_vaccinations=mean(new_vaccinations_per_hundred),
                                                        D.Est_new_vaccinations=sd(new_vaccinations_per_hundred),
                                                        CV_new_vaccinations=
                                                          sd(new_vaccinations_per_hundred)/
                                                          mean(new_vaccinations_per_hundred)*100,
                                                        Min_new_vaccinations=min(new_vaccinations_per_hundred),
                                                        Max_new_vaccinations=max(new_vaccinations_per_hundred))



#Estados Unidos, Decesos

Decesos_DfUS20 <- DfUS20 %>% summarise(Promedio_total_deaths=mean(total_deaths),
                                       D.Est_total_deaths=sd(total_deaths),
                                       CV_total_deaths=sd(total_deaths)/mean(total_deaths)*100,
                                       Min_total_deaths=min(total_deaths),
                                       Max_total_deaths=max(total_deaths),
                                       Promedio_new_deaths=mean(new_deaths),
                                       D.Est_new_deaths=sd(new_deaths),
                                       CV_new_deaths=sd(new_deaths)/mean(new_deaths)*100,
                                       Min_new_deaths=min(new_deaths),
                                       Max_new_deaths=max(new_deaths))

Tasa_de_decesos_DfUS20 <- DfUS20 %>% summarise(Promedio_total_deaths=mean(total_deaths_per_million),
                                               D.Est_total_deaths=sd(total_deaths_per_million),
                                               CV_total_deaths=
                                                 sd(total_deaths_per_million)/mean(total_deaths_per_million)*100,
                                               Min_total_deaths=min(total_deaths_per_million),
                                               Max_total_deaths=max(total_deaths_per_million),
                                               Promedio_new_deaths=mean(new_deaths_per_million),
                                               D.Est_new_deaths=sd(new_deaths_per_million),
                                               CV_new_deaths=
                                                 sd(new_deaths_per_million)/mean(new_deaths_per_million)*100,
                                               Min_new_deaths=min(new_deaths_per_million),
                                               Max_new_deaths=max(new_deaths_per_million))

Decesos_DfUS21 <- DfUS21 %>% summarise(Promedio_total_deaths=mean(total_deaths),
                                       D.Est_total_deaths=sd(total_deaths),
                                       CV_total_deaths=sd(total_deaths)/mean(total_deaths)*100,
                                       Min_total_deaths=min(total_deaths),
                                       Max_total_deaths=max(total_deaths),
                                       Promedio_new_deaths=mean(new_deaths),
                                       D.Est_new_deaths=sd(new_deaths),
                                       CV_new_deaths=sd(new_deaths)/mean(new_deaths)*100,
                                       Min_new_deaths=min(new_deaths),
                                       Max_new_deaths=max(new_deaths))

Tasa_de_decesos_DfUS21 <- DfUS21 %>% summarise(Promedio_total_deaths=mean(total_deaths_per_million),
                                               D.Est_total_deaths=sd(total_deaths_per_million),
                                               CV_total_deaths=
                                                 sd(total_deaths_per_million)/mean(total_deaths_per_million)*100,
                                               Min_total_deaths=min(total_deaths_per_million),
                                               Max_total_deaths=max(total_deaths_per_million),
                                               Promedio_new_deaths=mean(new_deaths_per_million),
                                               D.Est_new_deaths=sd(new_deaths_per_million),
                                               CV_new_deaths=
                                                 sd(new_deaths_per_million)/mean(new_deaths_per_million)*100,
                                               Min_new_deaths=min(new_deaths_per_million),
                                               Max_new_deaths=max(new_deaths_per_million))

Decesos_DfUS22 <- DfUS22 %>% summarise(Promedio_total_deaths=mean(total_deaths),
                                       D.Est_total_deaths=sd(total_deaths),
                                       CV_total_deaths=sd(total_deaths)/mean(total_deaths)*100,
                                       Min_total_deaths=min(total_deaths),
                                       Max_total_deaths=max(total_deaths),
                                       Promedio_new_deaths=mean(new_deaths),
                                       D.Est_new_deaths=sd(new_deaths),
                                       CV_new_deaths=sd(new_deaths)/mean(new_deaths)*100,
                                       Min_new_deaths=min(new_deaths),
                                       Max_new_deaths=max(new_deaths))

Tasa_de_decesos_DfUS22 <- DfUS22 %>% summarise(Promedio_total_deaths=mean(total_deaths_per_million),
                                               D.Est_total_deaths=sd(total_deaths_per_million),
                                               CV_total_deaths=
                                                 sd(total_deaths_per_million)/mean(total_deaths_per_million)*100,
                                               Min_total_deaths=min(total_deaths_per_million),
                                               Max_total_deaths=max(total_deaths_per_million),
                                               Promedio_new_deaths=mean(new_deaths_per_million),
                                               D.Est_new_deaths=sd(new_deaths_per_million),
                                               CV_new_deaths=
                                                 sd(new_deaths_per_million)/mean(new_deaths_per_million)*100,
                                               Min_new_deaths=min(new_deaths_per_million),
                                               Max_new_deaths=max(new_deaths_per_million))

Decesos_DfUS23 <- DfUS23 %>% summarise(Promedio_total_deaths=mean(total_deaths),
                                       D.Est_total_deaths=sd(total_deaths),
                                       CV_total_deaths=sd(total_deaths)/mean(total_deaths)*100,
                                       Min_total_deaths=min(total_deaths),
                                       Max_total_deaths=max(total_deaths),
                                       Promedio_new_deaths=mean(new_deaths),
                                       D.Est_new_deaths=sd(new_deaths),
                                       CV_new_deaths=sd(new_deaths)/mean(new_deaths)*100,
                                       Min_new_deaths=min(new_deaths),
                                       Max_new_deaths=max(new_deaths))

Tasa_de_decesos_DfUS23 <- DfUS23 %>% summarise(Promedio_total_deaths=mean(total_deaths_per_million),
                                               D.Est_total_deaths=sd(total_deaths_per_million),
                                               CV_total_deaths=
                                                 sd(total_deaths_per_million)/mean(total_deaths_per_million)*100,
                                               Min_total_deaths=min(total_deaths_per_million),
                                               Max_total_deaths=max(total_deaths_per_million),
                                               Promedio_new_deaths=mean(new_deaths_per_million),
                                               D.Est_new_deaths=sd(new_deaths_per_million),
                                               CV_new_deaths=
                                                 sd(new_deaths_per_million)/mean(new_deaths_per_million)*100,
                                               Min_new_deaths=min(new_deaths_per_million),
                                               Max_new_deaths=max(new_deaths_per_million))
#Alemania, Casos

Casos_DfDEU20 <- DfDEU20 %>% summarise(Promedio_total_cases=mean(total_cases),
                                       D.Est_total_cases=sd(total_cases),
                                       CV_total_cases=sd(total_cases)/mean(total_cases)*100,
                                       Min_total_cases=min(total_cases),
                                       Max_total_cases=max(total_cases),
                                       Promedio_new_cases=mean(new_cases),
                                       D.Est_new_cases=sd(new_cases),
                                       CV_new_cases=sd(new_cases)/mean(new_cases)*100,
                                       Min_new_cases=min(new_cases),
                                       Max_new_cases=max(new_cases))

Tasa_de_casos_DfDEU20 <- DfDEU20 %>% summarise(Promedio_total_cases=mean(total_cases_per_million),
                                               D.Est_total_cases=sd(total_cases_per_million),
                                               CV_total_cases=
                                                 sd(total_cases_per_million)/mean(total_cases_per_million)*100,
                                               Min_total_cases=min(total_cases_per_million),
                                               Max_total_cases=max(total_cases_per_million),
                                               Promedio_new_cases=mean(new_cases_per_million),
                                               D.Est_new_cases=sd(new_cases_per_million),
                                               CV_new_cases=
                                                 sd(new_cases_per_million)/mean(new_cases_per_million)*100,
                                               Min_new_cases=min(new_cases_per_million),
                                               Max_new_cases=max(new_cases_per_million))

Casos_DfDEU21 <- DfDEU21 %>% summarise(Promedio_total_cases=mean(total_cases),
                                       D.Est_total_cases=sd(total_cases),
                                       CV_total_cases=sd(total_cases)/mean(total_cases)*100,
                                       Min_total_cases=min(total_cases),
                                       Max_total_cases=max(total_cases),
                                       Promedio_new_cases=mean(new_cases),
                                       D.Est_new_cases=sd(new_cases),
                                       CV_new_cases=sd(new_cases)/mean(new_cases)*100,
                                       Min_new_cases=min(new_cases),
                                       Max_new_cases=max(new_cases))

Tasa_de_casos_DfDEU21 <- DfDEU21 %>% summarise(Promedio_total_cases=mean(total_cases_per_million),
                                               D.Est_total_cases=sd(total_cases_per_million),
                                               CV_total_cases=
                                                 sd(total_cases_per_million)/mean(total_cases_per_million)*100,
                                               Min_total_cases=min(total_cases_per_million),
                                               Max_total_cases=max(total_cases_per_million),
                                               Promedio_new_cases=mean(new_cases_per_million),
                                               D.Est_new_cases=sd(new_cases_per_million),
                                               CV_new_cases=
                                                 sd(new_cases_per_million)/mean(new_cases_per_million)*100,
                                               Min_new_cases=min(new_cases_per_million),
                                               Max_new_cases=max(new_cases_per_million))

Casos_DfDEU22 <- DfDEU22 %>% summarise(Promedio_total_cases=mean(total_cases),
                                       D.Est_total_cases=sd(total_cases),
                                       CV_total_cases=sd(total_cases)/mean(total_cases)*100,
                                       Min_total_cases=min(total_cases),
                                       Max_total_cases=max(total_cases),
                                       Promedio_new_cases=mean(new_cases),
                                       D.Est_new_cases=sd(new_cases),
                                       CV_new_cases=sd(new_cases)/mean(new_cases)*100,
                                       Min_new_cases=min(new_cases),
                                       Max_new_cases=max(new_cases))

Tasa_de_casos_DfDEU22 <- DfDEU22 %>% summarise(Promedio_total_cases=mean(total_cases_per_million),
                                               D.Est_total_cases=sd(total_cases_per_million),
                                               CV_total_cases=
                                                 sd(total_cases_per_million)/mean(total_cases_per_million)*100,
                                               Min_total_cases=min(total_cases_per_million),
                                               Max_total_cases=max(total_cases_per_million),
                                               Promedio_new_cases=mean(new_cases_per_million),
                                               D.Est_new_cases=sd(new_cases_per_million),
                                               CV_new_cases=
                                                 sd(new_cases_per_million)/mean(new_cases_per_million)*100,
                                               Min_new_cases=min(new_cases_per_million),
                                               Max_new_cases=max(new_cases_per_million))

Casos_DfDEU23 <- DfDEU23[1:172,] %>% summarise(Promedio_total_cases=mean(total_cases),
                                       D.Est_total_cases=sd(total_cases),
                                       CV_total_cases=sd(total_cases)/mean(total_cases)*100,
                                       Min_total_cases=min(total_cases),
                                       Max_total_cases=max(total_cases),
                                       Promedio_new_cases=mean(new_cases),
                                       D.Est_new_cases=sd(new_cases),
                                       CV_new_cases=sd(new_cases)/mean(new_cases)*100,
                                       Min_new_cases=min(new_cases),
                                       Max_new_cases=max(new_cases))

Tasa_de_casos_DfDEU23 <- DfDEU23[1:172,] %>% summarise(Promedio_total_cases=mean(total_cases_per_million),
                                               D.Est_total_cases=sd(total_cases_per_million),
                                               CV_total_cases=
                                                 sd(total_cases_per_million)/mean(total_cases_per_million)*100,
                                               Min_total_cases=min(total_cases_per_million),
                                               Max_total_cases=max(total_cases_per_million),
                                               Promedio_new_cases=mean(new_cases_per_million),
                                               D.Est_new_cases=sd(new_cases_per_million),
                                               CV_new_cases=
                                                 sd(new_cases_per_million)/mean(new_cases_per_million)*100,
                                               Min_new_cases=min(new_cases_per_million),
                                               Max_new_cases=max(new_cases_per_million))

#Vacunas en Alemania

Vacunas_DfDEU20 <- DfDEU20 %>% summarise(Promedio_total_vaccinations=mean(total_vaccinations),
                                         D.Est_total_vaccinations=sd(total_vaccinations),
                                         CV_total_vaccinations=sd(total_vaccinations)/mean(total_vaccinations)*100,
                                         Min_total_vaccinations=min(total_vaccinations),
                                         Max_total_vaccinations=max(total_vaccinations),
                                         Promedio_new_vaccinations=mean(new_vaccinations),
                                         D.Est_new_vaccinations=sd(new_vaccinations),
                                         CV_new_vaccinations=sd(new_vaccinations)/mean(new_vaccinations)*100,
                                         Min_new_vaccinations=min(new_vaccinations),
                                         Max_new_vaccinations=max(new_vaccinations))

Tasa_de_vacunas_DfDEU20 <- DfDEU20 %>% summarise(Promedio_total_vaccinations=mean(total_vaccinations_per_hundred),
                                                 D.Est_total_vaccinations=sd(total_vaccinations_per_hundred),
                                                 CV_total_vaccinations=
                                                   sd(total_vaccinations_per_hundred)/
                                                   mean(total_vaccinations_per_hundred)*100,
                                                 Min_total_vaccinations=min(total_vaccinations_per_hundred),
                                                 Max_total_vaccinations=max(total_vaccinations_per_hundred),
                                                 Promedio_new_vaccinations=mean(new_vaccinations_per_hundred),
                                                 D.Est_new_vaccinations=sd(new_vaccinations_per_hundred),
                                                 CV_new_vaccinations=
                                                   sd(new_vaccinations_per_hundred)/
                                                   mean(new_vaccinations_per_hundred)*100,
                                                 Min_new_vaccinations=min(new_vaccinations_per_hundred),
                                                 Max_new_vaccinations=max(new_vaccinations_per_hundred))

Vacunas_DfDEU21 <- DfDEU21 %>% summarise(Promedio_total_vaccinations=mean(total_vaccinations),
                                         D.Est_total_vaccinations=sd(total_vaccinations),
                                         CV_total_vaccinations=sd(total_vaccinations)/mean(total_vaccinations)*100,
                                         Min_total_vaccinations=min(total_vaccinations),
                                         Max_total_vaccinations=max(total_vaccinations),
                                         Promedio_new_vaccinations=mean(new_vaccinations),
                                         D.Est_new_vaccinations=sd(new_vaccinations),
                                         CV_new_vaccinations=sd(new_vaccinations)/mean(new_vaccinations)*100,
                                         Min_new_vaccinations=min(new_vaccinations),
                                         Max_new_vaccinations=max(new_vaccinations))

Tasa_de_vacunas_DfDEU21 <- DfDEU21 %>% summarise(Promedio_total_vaccinations=mean(total_vaccinations_per_hundred),
                                                 D.Est_total_vaccinations=sd(total_vaccinations_per_hundred),
                                                 CV_total_vaccinations=
                                                   sd(total_vaccinations_per_hundred)/
                                                   mean(total_vaccinations_per_hundred)*100,
                                                 Min_total_vaccinations=min(total_vaccinations_per_hundred),
                                                 Max_total_vaccinations=max(total_vaccinations_per_hundred),
                                                 Promedio_new_vaccinations=mean(new_vaccinations_per_hundred),
                                                 D.Est_new_vaccinations=sd(new_vaccinations_per_hundred),
                                                 CV_new_vaccinations=
                                                   sd(new_vaccinations_per_hundred)/
                                                   mean(new_vaccinations_per_hundred)*100,
                                                 Min_new_vaccinations=min(new_vaccinations_per_hundred),
                                                 Max_new_vaccinations=max(new_vaccinations_per_hundred))

Vacunas_DfDEU22 <- DfDEU22 %>% summarise(Promedio_total_vaccinations=mean(total_vaccinations),
                                         D.Est_total_vaccinations=sd(total_vaccinations),
                                         CV_total_vaccinations=sd(total_vaccinations)/mean(total_vaccinations)*100,
                                         Min_total_vaccinations=min(total_vaccinations),
                                         Max_total_vaccinations=max(total_vaccinations),
                                         Promedio_new_vaccinations=mean(new_vaccinations),
                                         D.Est_new_vaccinations=sd(new_vaccinations),
                                         CV_new_vaccinations=sd(new_vaccinations)/mean(new_vaccinations)*100,
                                         Min_new_vaccinations=min(new_vaccinations),
                                         Max_new_vaccinations=max(new_vaccinations))

Tasa_de_vacunas_DfDEU22 <- DfDEU22 %>% summarise(Promedio_total_vaccinations=mean(total_vaccinations_per_hundred),
                                                 D.Est_total_vaccinations=sd(total_vaccinations_per_hundred),
                                                 CV_total_vaccinations=
                                                   sd(total_vaccinations_per_hundred)/
                                                   mean(total_vaccinations_per_hundred)*100,
                                                 Min_total_vaccinations=min(total_vaccinations_per_hundred),
                                                 Max_total_vaccinations=max(total_vaccinations_per_hundred),
                                                 Promedio_new_vaccinations=mean(new_vaccinations_per_hundred),
                                                 D.Est_new_vaccinations=sd(new_vaccinations_per_hundred),
                                                 CV_new_vaccinations=
                                                   sd(new_vaccinations_per_hundred)/
                                                   mean(new_vaccinations_per_hundred)*100,
                                                 Min_new_vaccinations=min(new_vaccinations_per_hundred),
                                                 Max_new_vaccinations=max(new_vaccinations_per_hundred))


Vacunas_DfDEU23 <- DfDEU23 [1:97,] %>% summarise(Promedio_total_vaccinations=mean(total_vaccinations),
                                                 D.Est_total_vaccinations=sd(total_vaccinations),
                                                 CV_total_vaccinations=sd(total_vaccinations)/mean(total_vaccinations)*100,
                                                 Min_total_vaccinations=min(total_vaccinations),
                                                 Max_total_vaccinations=max(total_vaccinations),
                                                 Promedio_new_vaccinations=mean(new_vaccinations),
                                                 D.Est_new_vaccinations=sd(new_vaccinations),
                                                 CV_new_vaccinations=sd(new_vaccinations)/mean(new_vaccinations)*100,
                                                 Min_new_vaccinations=min(new_vaccinations),
                                                 Max_new_vaccinations=max(new_vaccinations))

Tasa_de_vacunas_DfDEU23 <- DfDEU23 [1:97,] %>% summarise(Promedio_total_vaccinations=
                                                           mean(total_vaccinations_per_hundred),
                                                         D.Est_total_vaccinations=sd(total_vaccinations_per_hundred),
                                                         CV_total_vaccinations=
                                                           sd(total_vaccinations_per_hundred)/
                                                           mean(total_vaccinations_per_hundred)*100,
                                                         Min_total_vaccinations=min(total_vaccinations_per_hundred),
                                                         Max_total_vaccinations=max(total_vaccinations_per_hundred),
                                                         Promedio_new_vaccinations=mean(new_vaccinations_per_hundred),
                                                         D.Est_new_vaccinations=sd(new_vaccinations_per_hundred),
                                                         CV_new_vaccinations=
                                                           sd(new_vaccinations_per_hundred)/
                                                           mean(new_vaccinations_per_hundred)*100,
                                                         Min_new_vaccinations=min(new_vaccinations_per_hundred),
                                                         Max_new_vaccinations=max(new_vaccinations_per_hundred))

#Alemania, Decesos

Decesos_DfDEU20 <- DfDEU20 %>% summarise(Promedio_total_deaths=mean(total_deaths),
                                         D.Est_total_deaths=sd(total_deaths),
                                         CV_total_deaths=sd(total_deaths)/mean(total_deaths)*100,
                                         Min_total_deaths=min(total_deaths),
                                         Max_total_deaths=max(total_deaths),
                                         Promedio_new_deaths=mean(new_deaths),
                                         D.Est_new_deaths=sd(new_deaths),
                                         CV_new_deaths=sd(new_deaths)/mean(new_deaths)*100,
                                         Min_new_deaths=min(new_deaths),
                                         Max_new_deaths=max(new_deaths))

Tasa_de_decesos_DfDEU20 <- DfDEU20 %>% summarise(Promedio_total_deaths=mean(total_deaths_per_million),
                                                 D.Est_total_deaths=sd(total_deaths_per_million),
                                                 CV_total_deaths=
                                                   sd(total_deaths_per_million)/mean(total_deaths_per_million)*100,
                                                 Min_total_deaths=min(total_deaths_per_million),
                                                 Max_total_deaths=max(total_deaths_per_million),
                                                 Promedio_new_deaths=mean(new_deaths_per_million),
                                                 D.Est_new_deaths=sd(new_deaths_per_million),
                                                 CV_new_deaths=
                                                   sd(new_deaths_per_million)/mean(new_deaths_per_million)*100,
                                                 Min_new_deaths=min(new_deaths_per_million),
                                                 Max_new_deaths=max(new_deaths_per_million))

Decesos_DfDEU21 <- DfDEU21 %>% summarise(Promedio_total_deaths=mean(total_deaths),
                                         D.Est_total_deaths=sd(total_deaths),
                                         CV_total_deaths=sd(total_deaths)/mean(total_deaths)*100,
                                         Min_total_deaths=min(total_deaths),
                                         Max_total_deaths=max(total_deaths),
                                         Promedio_new_deaths=mean(new_deaths),
                                         D.Est_new_deaths=sd(new_deaths),
                                         CV_new_deaths=sd(new_deaths)/mean(new_deaths)*100,
                                         Min_new_deaths=min(new_deaths),
                                         Max_new_deaths=max(new_deaths))

Tasa_de_decesos_DfDEU21 <- DfDEU21 %>% summarise(Promedio_total_deaths=mean(total_deaths_per_million),
                                                 D.Est_total_deaths=sd(total_deaths_per_million),
                                                 CV_total_deaths=
                                                   sd(total_deaths_per_million)/mean(total_deaths_per_million)*100,
                                                 Min_total_deaths=min(total_deaths_per_million),
                                                 Max_total_deaths=max(total_deaths_per_million),
                                                 Promedio_new_deaths=mean(new_deaths_per_million),
                                                 D.Est_new_deaths=sd(new_deaths_per_million),
                                                 CV_new_deaths=
                                                   sd(new_deaths_per_million)/mean(new_deaths_per_million)*100,
                                                 Min_new_deaths=min(new_deaths_per_million),
                                                 Max_new_deaths=max(new_deaths_per_million))

Decesos_DfDEU22 <- DfDEU22 %>% summarise(Promedio_total_deaths=mean(total_deaths),
                                         D.Est_total_deaths=sd(total_deaths),
                                         CV_total_deaths=sd(total_deaths)/mean(total_deaths)*100,
                                         Min_total_deaths=min(total_deaths),
                                         Max_total_deaths=max(total_deaths),
                                         Promedio_new_deaths=mean(new_deaths),
                                         D.Est_new_deaths=sd(new_deaths),
                                         CV_new_deaths=sd(new_deaths)/mean(new_deaths)*100,
                                         Min_new_deaths=min(new_deaths),
                                         Max_new_deaths=max(new_deaths))

Tasa_de_decesos_DfDEU22 <- DfDEU22 %>% summarise(Promedio_total_deaths=mean(total_deaths_per_million),
                                                 D.Est_total_deaths=sd(total_deaths_per_million),
                                                 CV_total_deaths=
                                                   sd(total_deaths_per_million)/mean(total_deaths_per_million)*100,
                                                 Min_total_deaths=min(total_deaths_per_million),
                                                 Max_total_deaths=max(total_deaths_per_million),
                                                 Promedio_new_deaths=mean(new_deaths_per_million),
                                                 D.Est_new_deaths=sd(new_deaths_per_million),
                                                 CV_new_deaths=
                                                   sd(new_deaths_per_million)/mean(new_deaths_per_million)*100,
                                                 Min_new_deaths=min(new_deaths_per_million),
                                                 Max_new_deaths=max(new_deaths_per_million))

Decesos_DfDEU23 <- DfDEU23[1:172,] %>% summarise(Promedio_total_deaths=mean(total_deaths),
                                         D.Est_total_deaths=sd(total_deaths),
                                         CV_total_deaths=sd(total_deaths)/mean(total_deaths)*100,
                                         Min_total_deaths=min(total_deaths),
                                         Max_total_deaths=max(total_deaths),
                                         Promedio_new_deaths=mean(new_deaths),
                                         D.Est_new_deaths=sd(new_deaths),
                                         CV_new_deaths=sd(new_deaths)/mean(new_deaths)*100,
                                         Min_new_deaths=min(new_deaths),
                                         Max_new_deaths=max(new_deaths))

Tasa_de_decesos_DfDEU23 <- DfDEU23[1:172,] %>% summarise(Promedio_total_deaths=mean(total_deaths_per_million),
                                                 D.Est_total_deaths=sd(total_deaths_per_million),
                                                 CV_total_deaths=
                                                   sd(total_deaths_per_million)/mean(total_deaths_per_million)*100,
                                                 Min_total_deaths=min(total_deaths_per_million),
                                                 Max_total_deaths=max(total_deaths_per_million),
                                                 Promedio_new_deaths=mean(new_deaths_per_million),
                                                 D.Est_new_deaths=sd(new_deaths_per_million),
                                                 CV_new_deaths=
                                                   sd(new_deaths_per_million)/mean(new_deaths_per_million)*100,
                                                 Min_new_deaths=min(new_deaths_per_million),
                                                 Max_new_deaths=max(new_deaths_per_million))
#Turquia, Casos

Casos_DfTUR20 <- DfTUR20 %>% summarise(Promedio_total_cases=mean(total_cases),
                                       D.Est_total_cases=sd(total_cases),
                                       CV_total_cases=sd(total_cases)/mean(total_cases)*100,
                                       Min_total_cases=min(total_cases),
                                       Max_total_cases=max(total_cases),
                                       Promedio_new_cases=mean(new_cases),
                                       D.Est_new_cases=sd(new_cases),
                                       CV_new_cases=sd(new_cases)/mean(new_cases)*100,
                                       Min_new_cases=min(new_cases),
                                       Max_new_cases=max(new_cases))

Tasa_de_casos_DfTUR20 <- DfTUR20 %>% summarise(Promedio_total_cases=mean(total_cases_per_million),
                                               D.Est_total_cases=sd(total_cases_per_million),
                                               CV_total_cases=
                                                 sd(total_cases_per_million)/mean(total_cases_per_million)*100,
                                               Min_total_cases=min(total_cases_per_million),
                                               Max_total_cases=max(total_cases_per_million),
                                               Promedio_new_cases=mean(new_cases_per_million),
                                               D.Est_new_cases=sd(new_cases_per_million),
                                               CV_new_cases=
                                                 sd(new_cases_per_million)/mean(new_cases_per_million)*100,
                                               Min_new_cases=min(new_cases_per_million),
                                               Max_new_cases=max(new_cases_per_million))


Casos_DfTUR21 <- DfTUR21 %>% summarise(Promedio_total_cases=mean(total_cases),
                                       D.Est_total_cases=sd(total_cases),
                                       CV_total_cases=sd(total_cases)/mean(total_cases)*100,
                                       Min_total_cases=min(total_cases),
                                       Max_total_cases=max(total_cases),
                                       Promedio_new_cases=mean(new_cases),
                                       D.Est_new_cases=sd(new_cases),
                                       CV_new_cases=sd(new_cases)/mean(new_cases)*100,
                                       Min_new_cases=min(new_cases),
                                       Max_new_cases=max(new_cases))

Tasa_de_casos_DfTUR21 <- DfTUR21 %>% summarise(Promedio_total_cases=mean(total_cases_per_million),
                                               D.Est_total_cases=sd(total_cases_per_million),
                                               CV_total_cases=
                                                 sd(total_cases_per_million)/mean(total_cases_per_million)*100,
                                               Min_total_cases=min(total_cases_per_million),
                                               Max_total_cases=max(total_cases_per_million),
                                               Promedio_new_cases=mean(new_cases_per_million),
                                               D.Est_new_cases=sd(new_cases_per_million),
                                               CV_new_cases=
                                                 sd(new_cases_per_million)/mean(new_cases_per_million)*100,
                                               Min_new_cases=min(new_cases_per_million),
                                               Max_new_cases=max(new_cases_per_million))

Casos_DfTUR22 <- DfTUR22 %>% summarise(Promedio_total_cases=mean(total_cases),
                                       D.Est_total_cases=sd(total_cases),
                                       CV_total_cases=sd(total_cases)/mean(total_cases)*100,
                                       Min_total_cases=min(total_cases),
                                       Max_total_cases=max(total_cases),
                                       Promedio_new_cases=mean(new_cases),
                                       D.Est_new_cases=sd(new_cases),
                                       CV_new_cases=sd(new_cases)/mean(new_cases)*100,
                                       Min_new_cases=min(new_cases),
                                       Max_new_cases=max(new_cases))

Tasa_de_casos_DfTUR22 <- DfTUR22 %>% summarise(Promedio_total_cases=mean(total_cases_per_million),
                                               D.Est_total_cases=sd(total_cases_per_million),
                                               CV_total_cases=
                                                 sd(total_cases_per_million)/mean(total_cases_per_million)*100,
                                               Min_total_cases=min(total_cases_per_million),
                                               Max_total_cases=max(total_cases_per_million),
                                               Promedio_new_cases=mean(new_cases_per_million),
                                               D.Est_new_cases=sd(new_cases_per_million),
                                               CV_new_cases=
                                                 sd(new_cases_per_million)/mean(new_cases_per_million)*100,
                                               Min_new_cases=min(new_cases_per_million),
                                               Max_new_cases=max(new_cases_per_million))

Casos_DfTUR23 <- DfTUR23 %>% summarise(Promedio_total_cases=mean(total_cases),
                                       D.Est_total_cases=sd(total_cases),
                                       CV_total_cases=sd(total_cases)/mean(total_cases)*100,
                                       Min_total_cases=min(total_cases),
                                       Max_total_cases=max(total_cases),
                                       Promedio_new_cases=mean(new_cases),
                                       D.Est_new_cases=sd(new_cases),
                                       CV_new_cases=sd(new_cases)/mean(new_cases)*100,
                                       Min_new_cases=min(new_cases),
                                       Max_new_cases=max(new_cases))

Tasa_de_casos_DfTUR23 <- DfTUR23 %>% summarise(Promedio_total_cases=mean(total_cases_per_million),
                                               D.Est_total_cases=sd(total_cases_per_million),
                                               CV_total_cases=
                                                 sd(total_cases_per_million)/mean(total_cases_per_million)*100,
                                               Min_total_cases=min(total_cases_per_million),
                                               Max_total_cases=max(total_cases_per_million),
                                               Promedio_new_cases=mean(new_cases_per_million),
                                               D.Est_new_cases=sd(new_cases_per_million),
                                               CV_new_cases=
                                                 sd(new_cases_per_million)/mean(new_cases_per_million)*100,
                                               Min_new_cases=min(new_cases_per_million),
                                               Max_new_cases=max(new_cases_per_million))
#Vacunaciones en Turquia

Vacunas_DfTUR20 <- DfTUR20 %>% summarise(Promedio_total_vaccinations=mean(total_vaccinations),
                                         D.Est_total_vaccinations=sd(total_vaccinations),
                                         CV_total_vaccinations=sd(total_vaccinations)/mean(total_vaccinations)*100,
                                         Min_total_vaccinations=min(total_vaccinations),
                                         Max_total_vaccinations=max(total_vaccinations),
                                         Promedio_new_vaccinations=mean(new_vaccinations),
                                         D.Est_new_vaccinations=sd(new_vaccinations),
                                         CV_new_vaccinations=sd(new_vaccinations)/mean(new_vaccinations)*100,
                                         Min_new_vaccinations=min(new_vaccinations),
                                         Max_new_vaccinations=max(new_vaccinations))

Tasa_de_vacunas_DfTUR20 <- DfTUR20 %>% summarise(Promedio_total_vaccinations=mean(total_vaccinations_per_hundred),
                                                 D.Est_total_vaccinations=sd(total_vaccinations_per_hundred),
                                                 CV_total_vaccinations=
                                                   sd(total_vaccinations_per_hundred)/
                                                   mean(total_vaccinations_per_hundred)*100,
                                                 Min_total_vaccinations=min(total_vaccinations_per_hundred),
                                                 Max_total_vaccinations=max(total_vaccinations_per_hundred),
                                                 Promedio_new_vaccinations=mean(new_vaccinations_per_hundred),
                                                 D.Est_new_vaccinations=sd(new_vaccinations_per_hundred),
                                                 CV_new_vaccinations=
                                                   sd(new_vaccinations_per_hundred)/
                                                   mean(new_vaccinations_per_hundred)*100,
                                                 Min_new_vaccinations=min(new_vaccinations_per_hundred),
                                                 Max_new_vaccinations=max(new_vaccinations_per_hundred))

Vacunas_DfTUR21 <- DfTUR21 %>% summarise(Promedio_total_vaccinations=mean(total_vaccinations),
                                         D.Est_total_vaccinations=sd(total_vaccinations),
                                         CV_total_vaccinations=sd(total_vaccinations)/mean(total_vaccinations)*100,
                                         Min_total_vaccinations=min(total_vaccinations),
                                         Max_total_vaccinations=max(total_vaccinations),
                                         Promedio_new_vaccinations=mean(new_vaccinations),
                                         D.Est_new_vaccinations=sd(new_vaccinations),
                                         CV_new_vaccinations=sd(new_vaccinations)/mean(new_vaccinations)*100,
                                         Min_new_vaccinations=min(new_vaccinations),
                                         Max_new_vaccinations=max(new_vaccinations))

Tasa_de_vacunas_DfTUR21 <- DfTUR21 %>% summarise(Promedio_total_vaccinations=mean(total_vaccinations_per_hundred),
                                                 D.Est_total_vaccinations=sd(total_vaccinations_per_hundred),
                                                 CV_total_vaccinations=
                                                   sd(total_vaccinations_per_hundred)/
                                                   mean(total_vaccinations_per_hundred)*100,
                                                 Min_total_vaccinations=min(total_vaccinations_per_hundred),
                                                 Max_total_vaccinations=max(total_vaccinations_per_hundred),
                                                 Promedio_new_vaccinations=mean(new_vaccinations_per_hundred),
                                                 D.Est_new_vaccinations=sd(new_vaccinations_per_hundred),
                                                 CV_new_vaccinations=
                                                   sd(new_vaccinations_per_hundred)/
                                                   mean(new_vaccinations_per_hundred)*100,
                                                 Min_new_vaccinations=min(new_vaccinations_per_hundred),
                                                 Max_new_vaccinations=max(new_vaccinations_per_hundred))

Vacunas_DfTUR22 <- DfTUR22 [1:326,] %>% summarise(Promedio_total_vaccinations=mean(total_vaccinations),
                                                  D.Est_total_vaccinations=sd(total_vaccinations),
                                                  CV_total_vaccinations=sd(total_vaccinations)/mean(total_vaccinations)*100,
                                                  Min_total_vaccinations=min(total_vaccinations),
                                                  Max_total_vaccinations=max(total_vaccinations),
                                                  Promedio_new_vaccinations=mean(new_vaccinations),
                                                  D.Est_new_vaccinations=sd(new_vaccinations),
                                                  CV_new_vaccinations=sd(new_vaccinations)/mean(new_vaccinations)*100,
                                                  Min_new_vaccinations=min(new_vaccinations),
                                                  Max_new_vaccinations=max(new_vaccinations))

Tasa_de_vacunas_DfTUR22 <- DfTUR22 [1:326,] %>% summarise(Promedio_total_vaccinations=
                                                            mean(total_vaccinations_per_hundred),
                                                          D.Est_total_vaccinations=sd(total_vaccinations_per_hundred),
                                                          CV_total_vaccinations=
                                                            sd(total_vaccinations_per_hundred)/
                                                            mean(total_vaccinations_per_hundred)*100,
                                                          Min_total_vaccinations=min(total_vaccinations_per_hundred),
                                                          Max_total_vaccinations=max(total_vaccinations_per_hundred),
                                                          Promedio_new_vaccinations=mean(new_vaccinations_per_hundred),
                                                          D.Est_new_vaccinations=sd(new_vaccinations_per_hundred),
                                                          CV_new_vaccinations=
                                                            sd(new_vaccinations_per_hundred)/
                                                            mean(new_vaccinations_per_hundred)*100,
                                                          Min_new_vaccinations=min(new_vaccinations_per_hundred),
                                                          Max_new_vaccinations=max(new_vaccinations_per_hundred))

Vacunas_DfTUR23 <- DfTUR23 %>%  summarise(Promedio_total_vaccinations=mean(total_vaccinations),
                                          D.Est_total_vaccinations=sd(total_vaccinations),
                                          CV_total_vaccinations=sd(total_vaccinations)/mean(total_vaccinations)*100,
                                          Min_total_vaccinations=min(total_vaccinations),
                                          Max_total_vaccinations=max(total_vaccinations),
                                          Promedio_new_vaccinations=mean(new_vaccinations),
                                          D.Est_new_vaccinations=sd(new_vaccinations),
                                          CV_new_vaccinations=sd(new_vaccinations)/mean(new_vaccinations)*100,
                                          Min_new_vaccinations=min(new_vaccinations),
                                          Max_new_vaccinations=max(new_vaccinations))

Tasa_de_vacunas_DfTUR23 <- DfTUR23 %>% summarise(Promedio_total_vaccinations=mean(total_vaccinations_per_hundred),
                                                 D.Est_total_vaccinations=sd(total_vaccinations_per_hundred),
                                                 CV_total_vaccinations=
                                                   sd(total_vaccinations_per_hundred)/
                                                   mean(total_vaccinations_per_hundred)*100,
                                                 Min_total_vaccinations=min(total_vaccinations_per_hundred),
                                                 Max_total_vaccinations=max(total_vaccinations_per_hundred),
                                                 Promedio_new_vaccinations=mean(new_vaccinations_per_hundred),
                                                 D.Est_new_vaccinations=sd(new_vaccinations_per_hundred),
                                                 CV_new_vaccinations=
                                                   sd(new_vaccinations_per_hundred)/
                                                   mean(new_vaccinations_per_hundred)*100,
                                                 Min_new_vaccinations=min(new_vaccinations_per_hundred),
                                                 Max_new_vaccinations=max(new_vaccinations_per_hundred))

#Turquia, Decesos

Decesos_DfTUR20 <- DfTUR20 %>% summarise(Promedio_total_deaths=mean(total_deaths),
                                         D.Est_total_deaths=sd(total_deaths),
                                         CV_total_deaths=sd(total_deaths)/mean(total_deaths)*100,
                                         Min_total_deaths=min(total_deaths),
                                         Max_total_deaths=max(total_deaths),
                                         Promedio_new_deaths=mean(new_deaths),
                                         D.Est_new_deaths=sd(new_deaths),
                                         CV_new_deaths=sd(new_deaths)/mean(new_deaths)*100,
                                         Min_new_deaths=min(new_deaths),
                                         Max_new_deaths=max(new_deaths))

Tasa_de_decesos_DfTUR20 <- DfTUR20 %>% summarise(Promedio_total_deaths=mean(total_deaths_per_million),
                                                 D.Est_total_deaths=sd(total_deaths_per_million),
                                                 CV_total_deaths=
                                                   sd(total_deaths_per_million)/mean(total_deaths_per_million)*100,
                                                 Min_total_deaths=min(total_deaths_per_million),
                                                 Max_total_deaths=max(total_deaths_per_million),
                                                 Promedio_new_deaths=mean(new_deaths_per_million),
                                                 D.Est_new_deaths=sd(new_deaths_per_million),
                                                 CV_new_deaths=
                                                   sd(new_deaths_per_million)/mean(new_deaths_per_million)*100,
                                                 Min_new_deaths=min(new_deaths_per_million),
                                                 Max_new_deaths=max(new_deaths_per_million))

Decesos_DfTUR21 <- DfTUR21 %>% summarise(Promedio_total_deaths=mean(total_deaths),
                                         D.Est_total_deaths=sd(total_deaths),
                                         CV_total_deaths=sd(total_deaths)/mean(total_deaths)*100,
                                         Min_total_deaths=min(total_deaths),
                                         Max_total_deaths=max(total_deaths),
                                         Promedio_new_deaths=mean(new_deaths),
                                         D.Est_new_deaths=sd(new_deaths),
                                         CV_new_deaths=sd(new_deaths)/mean(new_deaths)*100,
                                         Min_new_deaths=min(new_deaths),
                                         Max_new_deaths=max(new_deaths))

Tasa_de_decesos_DfTUR21 <- DfTUR21 %>% summarise(Promedio_total_deaths=mean(total_deaths_per_million),
                                                 D.Est_total_deaths=sd(total_deaths_per_million),
                                                 CV_total_deaths=
                                                   sd(total_deaths_per_million)/mean(total_deaths_per_million)*100,
                                                 Min_total_deaths=min(total_deaths_per_million),
                                                 Max_total_deaths=max(total_deaths_per_million),
                                                 Promedio_new_deaths=mean(new_deaths_per_million),
                                                 D.Est_new_deaths=sd(new_deaths_per_million),
                                                 CV_new_deaths=
                                                   sd(new_deaths_per_million)/mean(new_deaths_per_million)*100,
                                                 Min_new_deaths=min(new_deaths_per_million),
                                                 Max_new_deaths=max(new_deaths_per_million))

Decesos_DfTUR22 <- DfTUR22 %>% summarise(Promedio_total_deaths=mean(total_deaths),
                                         D.Est_total_deaths=sd(total_deaths),
                                         CV_total_deaths=sd(total_deaths)/mean(total_deaths)*100,
                                         Min_total_deaths=min(total_deaths),
                                         Max_total_deaths=max(total_deaths),
                                         Promedio_new_deaths=mean(new_deaths),
                                         D.Est_new_deaths=sd(new_deaths),
                                         CV_new_deaths=sd(new_deaths)/mean(new_deaths)*100,
                                         Min_new_deaths=min(new_deaths),
                                         Max_new_deaths=max(new_deaths))

Tasa_de_decesos_DfTUR22 <- DfTUR22 %>% summarise(Promedio_total_deaths=mean(total_deaths_per_million),
                                                 D.Est_total_deaths=sd(total_deaths_per_million),
                                                 CV_total_deaths=
                                                   sd(total_deaths_per_million)/mean(total_deaths_per_million)*100,
                                                 Min_total_deaths=min(total_deaths_per_million),
                                                 Max_total_deaths=max(total_deaths_per_million),
                                                 Promedio_new_deaths=mean(new_deaths_per_million),
                                                 D.Est_new_deaths=sd(new_deaths_per_million),
                                                 CV_new_deaths=
                                                   sd(new_deaths_per_million)/mean(new_deaths_per_million)*100,
                                                 Min_new_deaths=min(new_deaths_per_million),
                                                 Max_new_deaths=max(new_deaths_per_million))

Decesos_DfTUR23 <- DfTUR23 %>% summarise(Promedio_total_deaths=mean(total_deaths),
                                         D.Est_total_deaths=sd(total_deaths),
                                         CV_total_deaths=sd(total_deaths)/mean(total_deaths)*100,
                                         Min_total_deaths=min(total_deaths),
                                         Max_total_deaths=max(total_deaths),
                                         Promedio_new_deaths=mean(new_deaths),
                                         D.Est_new_deaths=sd(new_deaths),
                                         CV_new_deaths=sd(new_deaths)/mean(new_deaths)*100,
                                         Min_new_deaths=min(new_deaths),
                                         Max_new_deaths=max(new_deaths))

Tasa_de_decesos_DfTUR23 <- DfTUR23 %>% summarise(Promedio_total_deaths=mean(total_deaths_per_million),
                                                 D.Est_total_deaths=sd(total_deaths_per_million),
                                                 CV_total_deaths=
                                                   sd(total_deaths_per_million)/mean(total_deaths_per_million)*100,
                                                 Min_total_deaths=min(total_deaths_per_million),
                                                 Max_total_deaths=max(total_deaths_per_million),
                                                 Promedio_new_deaths=mean(new_deaths_per_million),
                                                 D.Est_new_deaths=sd(new_deaths_per_million),
                                                 CV_new_deaths=
                                                   sd(new_deaths_per_million)/mean(new_deaths_per_million)*100,
                                                 Min_new_deaths=min(new_deaths_per_million),
                                                 Max_new_deaths=max(new_deaths_per_million))
