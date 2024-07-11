#Llamado a librerias

library(tidyverse)
library(dplyr)
library(readr)
Covid_G4_Base_de_Datos <-
  read_csv ("GitHub/Trabajo-Final-Grupo-4/BBDD/Covid G4 Base de Datos.csv")
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

DfCHN <- CHN %>% select(iso_code, continent, location, date, 
                        total_cases, new_cases, new_cases_per_million,
                        total_cases_per_million, 
                        total_deaths, new_deaths, new_deaths_per_million, 
                        total_deaths_per_million, 
                        total_vaccinations, new_vaccinations, 
                        total_vaccinations_per_hundred, people_vaccinated,
                        people_vaccinated_per_hundred, 
                        population)

DfCUB <- CUB %>% select(iso_code, continent, location, date, 
                        total_cases, new_cases, new_cases_per_million,
                        total_cases_per_million, 
                        total_deaths, new_deaths, new_deaths_per_million, 
                        total_deaths_per_million, 
                        total_vaccinations, new_vaccinations, 
                        total_vaccinations_per_hundred, people_vaccinated,
                        people_vaccinated_per_hundred, 
                        population)

DfDEU <- DEU %>% select(iso_code, continent, location, date, 
                        total_cases, new_cases, new_cases_per_million,
                        total_cases_per_million, 
                        total_deaths, new_deaths, new_deaths_per_million, 
                        total_deaths_per_million, 
                        total_vaccinations, new_vaccinations, 
                        total_vaccinations_per_hundred, people_vaccinated,
                        people_vaccinated_per_hundred, 
                        population)

DfESP <- ESP %>% select(iso_code, continent, location, date, 
                        total_cases, new_cases, new_cases_per_million,
                        total_cases_per_million, 
                        total_deaths, new_deaths, new_deaths_per_million, 
                        total_deaths_per_million, 
                        total_vaccinations, new_vaccinations, 
                        total_vaccinations_per_hundred, people_vaccinated,
                        people_vaccinated_per_hundred, 
                        population)

DfGBR <- GBR %>% select(iso_code, continent, location, date, 
                        total_cases, new_cases, new_cases_per_million,
                        total_cases_per_million, 
                        total_deaths, new_deaths, new_deaths_per_million, 
                        total_deaths_per_million, 
                        total_vaccinations, new_vaccinations, 
                        total_vaccinations_per_hundred, people_vaccinated,
                        people_vaccinated_per_hundred, 
                        population)

DfIND <- IND %>% select(iso_code, continent, location, date, 
                        total_cases, new_cases, new_cases_per_million,
                        total_cases_per_million, 
                        total_deaths, new_deaths, new_deaths_per_million, 
                        total_deaths_per_million, 
                        total_vaccinations, new_vaccinations, 
                        total_vaccinations_per_hundred, people_vaccinated,
                        people_vaccinated_per_hundred, 
                        population)

DfIRN <- IRN %>% select(iso_code, continent, location, date, 
                        total_cases, new_cases, new_cases_per_million,
                        total_cases_per_million, 
                        total_deaths, new_deaths, new_deaths_per_million, 
                        total_deaths_per_million, 
                        total_vaccinations, new_vaccinations, 
                        total_vaccinations_per_hundred, people_vaccinated,
                        people_vaccinated_per_hundred, 
                        population)

DfRUS <- RUS %>% select(iso_code, continent, location, date, 
                        total_cases, new_cases, new_cases_per_million,
                        total_cases_per_million, 
                        total_deaths, new_deaths, new_deaths_per_million, 
                        total_deaths_per_million, 
                        total_vaccinations, new_vaccinations, 
                        total_vaccinations_per_hundred, people_vaccinated,
                        people_vaccinated_per_hundred, 
                        population)

DfTUR <- TUR %>% select(iso_code, continent, location, date, 
                        total_cases, new_cases, new_cases_per_million,
                        total_cases_per_million, 
                        total_deaths, new_deaths, new_deaths_per_million, 
                        total_deaths_per_million, 
                        total_vaccinations, new_vaccinations, 
                        total_vaccinations_per_hundred, people_vaccinated,
                        people_vaccinated_per_hundred, 
                        population)

DfUS <- US %>% select(iso_code, continent, location, date, 
                      total_cases, new_cases, new_cases_per_million,
                      total_cases_per_million, 
                      total_deaths, new_deaths, new_deaths_per_million, 
                      total_deaths_per_million, 
                      total_vaccinations, new_vaccinations, 
                      total_vaccinations_per_hundred, people_vaccinated,
                      people_vaccinated_per_hundred, 
                      population)


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
DfUS20 <-DfUS20 %>% mutate (people_vaccinated_per_hundred = if_else
                            (is.na(people_vaccinated_per_hundred), 0, people_vaccinated_per_hundred))

#Dia 346
DfUS20 [346,14] <- 45620

#Verificacion de que no existan datos NA en US20
apply(DfUS20, MARGIN = 2, function(x) sum(is.na(x))) %>% view()



DfUS21 <- data_frame (DfUS [365:729,])


DfUS22 <-data.frame (DfUS [730:1094,])
#Inconsistencia de datos en 2022-03-15, al dia siguiente hay menos muertos totales

apply(DfUS22, MARGIN = 2, function(x) sum(is.na(x))) %>% view()
DfUS22 <-DfUS22 %>% mutate (new_deaths = if_else
                            (is.na(new_deaths), 0, new_deaths))
DfUS22 <-DfUS22 %>% mutate (new_deaths_per_million = if_else
                            (is.na(new_deaths_per_million), 0, new_deaths_per_million))

DfUS23 <- data.frame(DfUS [DfUS$date>"2022-12-31",])
DfUS23 [130:172,13] <- 676728782
DfUS23 [130:172,14] <- 0
DfUS23 [130:172,15] <- 203.83
DfUS23 [130:172,16] <- 270227181
DfUS23 [130:172,17] <- 81.39
DfUS23 [130:172,18] <- 338289856
DfUS23 [130:172,19] <- 0.003473648

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



#Verificacion de que no existan datos NA en DEU20
apply(DfDEU20, MARGIN = 2, function(x) sum(is.na(x))) %>% view()

DfDEU20 [1:24,5:17] <- 0
DfDEU20 [25,5:6] <- 1
DfDEU20 [26,6] <- 1
DfDEU20 [25:26,7] <- 0.011
DfDEU20 [25,8] <- 0.011
DfDEU20 [1:359,13:17] <- 0
DfDEU20 [360,14] <- 24427

DfDEU21 <- data.frame(DfDEU [365:729,])


DfDEU22 <- data.frame(DfDEU [730:1094,])
apply(DfDEU22, MARGIN = 2, function(x) sum(is.na(x))) %>% view()


DfDEU23 <- data.frame(DfDEU [1095:1266,])
DfDEU23 [172,6:7] <- 0
DfDEU23 [74:172,13] <- 192221468
DfDEU23 [74:172,14] <- 0
DfDEU23 [74:172,15] <- 230.56
DfDEU23 [74:172,16] <- 64876299
DfDEU23 [74:172,17] <- 77.8




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
DfTUR20 <-DfTUR20 %>% mutate (people_vaccinated_per_hundred = if_else
                              (is.na(people_vaccinated_per_hundred), 0, people_vaccinated_per_hundred))

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
DfTUR21 <-DfTUR21 %>% mutate (people_vaccinated_per_hundred = if_else
                              (is.na(people_vaccinated_per_hundred), 0, people_vaccinated_per_hundred))
#Verificacion de que no existan datos NA en TUR21
apply(DfTUR21, MARGIN = 2, function(x) sum(is.na(x))) %>% view()

DfTUR22 <- data.frame(DfTUR [730:1094,])
#Valores NA de DfTUR22
#Dia 4
DfTUR22 [4,13] <- 132897714
DfTUR22 [4,14] <- 0
DfTUR22 [4,15] <- 155.73
DfTUR22 [4,16] <- 56960334
DfTUR22 [4,17] <- 66.74
#Dia 5
DfTUR22 [5,14] <- (134195065 - 132897714) 
#Dia 91
DfTUR22 [91,13] <- 146995383
DfTUR22 [91,14] <- 0
DfTUR22 [91,15] <- 172.24
DfTUR22 [91,16] <- 57781315
DfTUR22 [91,17] <- 67.71
#Dia 92
DfTUR22 [92,13] <- 146995383
DfTUR22 [92,14] <- 0
DfTUR22 [92,15] <- 172.24
DfTUR22 [92,16] <- 57781315
DfTUR22 [92,17] <- 67.71
#Dia 93
DfTUR22 [93,14] <- (147070173 - 146995383) 
#Dia 105
DfTUR22 [105,13] <- 147301011
DfTUR22 [105,14] <- 0
DfTUR22 [105,15] <- 172.60
DfTUR22 [105,16] <- 57801926
DfTUR22 [105,17] <- 67.73
#Dia 106
DfTUR22 [106,13] <- 147301011
DfTUR22 [106,14] <- 0
DfTUR22 [106,15] <- 172.60
DfTUR22 [106,16] <- 57801926
DfTUR22 [106,17] <- 67.73
#Dia 107
DfTUR22 [107,14] <- (147333131 - 147301011)
#Dia 112
DfTUR22 [112,13] <- 147402858
DfTUR22 [112,14] <- 0
DfTUR22 [112,15] <- 172.72
DfTUR22 [112,16] <- 57810012
DfTUR22 [112,17] <- 67.74
#Dia 113
DfTUR22 [113,14] <- (147423229 - 147402858)
#Dia 140
DfTUR22 [140,13] <- 147663432
DfTUR22 [140,14] <- 0
DfTUR22 [140,15] <- 173.03
DfTUR22 [140,16] <- 57834671
DfTUR22 [140,17] <- 67.77
#Dia 141
DfTUR22 [141,14] <- (147677323 - 147663432)
#Del dia 152 al 163
DfTUR22 [152:163,13] <- 147734958
DfTUR22 [152:163,14] <- 0
DfTUR22 [152:163,15] <- 173.11
DfTUR22 [152:163,16] <- 57843685
DfTUR22 [152:163,17] <- 67.78
#Dia 164
DfTUR22 [164,14] <- (147781738 - 147734958)
#Dia 177
DfTUR22 [177,13] <- 147814534
DfTUR22 [177,14] <- 0
DfTUR22 [177,15] <- 173.20
DfTUR22 [177,16] <- 57856676
DfTUR22 [177,17] <- 67.79
#Dia 178
DfTUR22 [178,14] <- (147818598 - 147814534)
#Dia 182
DfTUR22 [182,13] <- 147828801
DfTUR22 [182,14] <- 0
DfTUR22 [182,15] <- 173.22
DfTUR22 [182,16] <- 57858728
DfTUR22 [182,17] <- 67.80
#Dia 183
DfTUR22 [183,14] <- (147833636 - 147828801)
#Dia 189
DfTUR22 [189,13] <- 147857274
DfTUR22 [189,14] <- 0
DfTUR22 [189,15] <- 173.25
DfTUR22 [189,16] <- 57861505
DfTUR22 [189,17] <- 67.80
#Dia 190
DfTUR22 [190,14] <- (147861309 - 147857274)
#Dia 285
DfTUR22 [285,13] <- 152381745
DfTUR22 [285,14] <- 0
DfTUR22 [285,15] <- 178.56
DfTUR22 [285,16] <- 57931643
DfTUR22 [285,17] <- 67.88
#Dia 286
DfTUR22 [286,14] <- (152396067 - 152381745)
#Dia 303
DfTUR22 [303,13] <- 152465212
DfTUR22 [303,14] <- 0
DfTUR22 [303,15] <- 178.65
DfTUR22 [303,16] <- 57936171
DfTUR22 [303,17] <- 67.89
#Dia 304 
DfTUR22[304,14] <- (152469836 - 152465212)
#Del dia 306 al 325
DfTUR22 [306:325,13] <- 152475057
DfTUR22 [306:325,14] <- 0
DfTUR22 [306:325,15] <- 178.67
DfTUR22 [306:325,16] <- 57936783
DfTUR22 [306:325,17] <- 67.89
#Dia 326
DfTUR22 [326,14] <- (152543341 - 152475057)

DfTUR22 [327:365,13] <- 152543341
DfTUR22 [327:365,14] <- 0
DfTUR22 [327:365,15] <- 178.75
DfTUR22 [327:365,16] <- 57941051
DfTUR22 [327:365,17] <- 67.89
DfTUR22 [327:365,18] <- 85341248
DfTUR22 [327:365,19] <- 0.0800128913

DfTUR23 <- data.frame(DfTUR [DfTUR$date>"2022-12-31",])

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
Casos_DfUS20 <- DfUS20 %>% summarise(Promedio_total=mean(total_cases),
                                     D.Est_total=sd(total_cases),
                                     CV_total=sd(total_cases)/mean(total_cases)*100,
                                     Min_total=min(total_cases),
                                     Max_total=max(total_cases),
                                     Promedio_new=mean(new_cases),
                                     D.Est_new=sd(new_cases),
                                     CV_new=sd(new_cases)/mean(new_cases)*100,
                                     Min_new=min(new_cases),
                                     Max_new=max(new_cases))

Tasa_de_casos_DfUS20 <- DfUS20 %>% summarise(Promedio_total=mean(total_cases_per_million),
                                             D.Est_total=sd(total_cases_per_million),
                                             CV_total=
                                               sd(total_cases_per_million)/mean(total_cases_per_million)*100,
                                             Min_total=min(total_cases_per_million),
                                             Max_total=max(total_cases_per_million),
                                             Promedio_new=mean(new_cases_per_million),
                                             D.Est_new=sd(new_cases_per_million),
                                             CV_new=
                                               sd(new_cases_per_million)/mean(new_cases_per_million)*100,
                                             Min_new=min(new_cases_per_million),
                                             Max_new=max(new_cases_per_million))


Casos_DfUS21 <- DfUS21 %>% summarise(Promedio_total=mean(total_cases),
                                     D.Est_total=sd(total_cases),
                                     CV_total=sd(total_cases)/mean(total_cases)*100,
                                     Min_total=min(total_cases),
                                     Max_total=max(total_cases),
                                     Promedio_new=mean(new_cases),
                                     D.Est_new=sd(new_cases),
                                     CV_new=sd(new_cases)/mean(new_cases)*100,
                                     Min_new=min(new_cases),
                                     Max_new=max(new_cases))

Tasa_de_casos_DfUS21 <- DfUS21 %>% summarise(Promedio_total=mean(total_cases_per_million),
                                             D.Est_total=sd(total_cases_per_million),
                                             CV_total=
                                               sd(total_cases_per_million)/mean(total_cases_per_million)*100,
                                             Min_total=min(total_cases_per_million),
                                             Max_total=max(total_cases_per_million),
                                             Promedio_new=mean(new_cases_per_million),
                                             D.Est_new=sd(new_cases_per_million),
                                             CV_new=
                                               sd(new_cases_per_million)/mean(new_cases_per_million)*100,
                                             Min_new=min(new_cases_per_million),
                                             Max_new=max(new_cases_per_million))

Casos_DfUS22 <- DfUS22 %>% summarise(Promedio_total=mean(total_cases),
                                     D.Est_total=sd(total_cases),
                                     CV_total=sd(total_cases)/mean(total_cases)*100,
                                     Min_total=min(total_cases),
                                     Max_total=max(total_cases),
                                     Promedio_new=mean(new_cases),
                                     D.Est_new=sd(new_cases),
                                     CV_new=sd(new_cases)/mean(new_cases)*100,
                                     Min_new=min(new_cases),
                                     Max_new=max(new_cases))

Tasa_de_casos_DfUS22 <- DfUS22 %>% summarise(Promedio_total=mean(total_cases_per_million),
                                             D.Est_total=sd(total_cases_per_million),
                                             CV_total=
                                               sd(total_cases_per_million)/mean(total_cases_per_million)*100,
                                             Min_total=min(total_cases_per_million),
                                             Max_total=max(total_cases_per_million),
                                             Promedio_new=mean(new_cases_per_million),
                                             D.Est_new=sd(new_cases_per_million),
                                             CV_new=
                                               sd(new_cases_per_million)/mean(new_cases_per_million)*100,
                                             Min_new=min(new_cases_per_million),
                                             Max_new=max(new_cases_per_million))

Casos_DfUS23 <- DfUS23 %>% summarise(Promedio_total=mean(total_cases),
                                     D.Est_total=sd(total_cases),
                                     CV_total=sd(total_cases)/mean(total_cases)*100,
                                     Min_total=min(total_cases),
                                     Max_total=max(total_cases),
                                     Promedio_new=mean(new_cases),
                                     D.Est_new=sd(new_cases),
                                     CV_new=sd(new_cases)/mean(new_cases)*100,
                                     Min_new=min(new_cases),
                                     Max_new=max(new_cases))

Tasa_de_casos_DfUS23 <- DfUS23 %>% summarise(Promedio_total=mean(total_cases_per_million),
                                             D.Est_total=sd(total_cases_per_million),
                                             CV_total=
                                               sd(total_cases_per_million)/mean(total_cases_per_million)*100,
                                             Min_total=min(total_cases_per_million),
                                             Max_total=max(total_cases_per_million),
                                             Promedio_new=mean(new_cases_per_million),
                                             D.Est_new=sd(new_cases_per_million),
                                             CV_new=
                                               sd(new_cases_per_million)/mean(new_cases_per_million)*100,
                                             Min_new=min(new_cases_per_million),
                                             Max_new=max(new_cases_per_million))
#Estados Unidos Vacunaciones 
Vacunas_DfUS20 <- DfUS20 %>% summarise(Promedio_total=mean(total_vaccinations),
                                       D.Est_total=sd(total_vaccinations),
                                       CV_total=sd(total_vaccinations)/mean(total_vaccinations)*100,
                                       Min_total=min(total_vaccinations),
                                       Max_total=max(total_vaccinations),
                                       Promedio_new=mean(new_vaccinations),
                                       D.Est_new=sd(new_vaccinations),
                                       CV_new=sd(new_vaccinations)/mean(new_vaccinations)*100,
                                       Min_new=min(new_vaccinations),
                                       Max_new=max(new_vaccinations))

Tasa_de_vacunas_DfUS20 <- DfUS20 %>% summarise(Promedio_total=mean(total_vaccinations_per_hundred),
                                               D.Est_total=sd(total_vaccinations_per_hundred),
                                               CV_total=
                                                 sd(total_vaccinations_per_hundred)/
                                                 mean(total_vaccinations_per_hundred)*100,
                                               Min_total=min(total_vaccinations_per_hundred),
                                               Max_total=max(total_vaccinations_per_hundred),
                                               Promedio_new=mean(new_vaccinations_per_hundred),
                                               D.Est_new=sd(new_vaccinations_per_hundred),
                                               CV_new=
                                                 sd(new_vaccinations_per_hundred)/
                                                 mean(new_vaccinations_per_hundred)*100,
                                               Min_new=min(new_vaccinations_per_hundred),
                                               Max_new=max(new_vaccinations_per_hundred))

Vacunas_DfUS21 <- DfUS21 %>% summarise(Promedio_total=mean(total_vaccinations),
                                       D.Est_total=sd(total_vaccinations),
                                       CV_total=sd(total_vaccinations)/mean(total_vaccinations)*100,
                                       Min_total=min(total_vaccinations),
                                       Max_total=max(total_vaccinations),
                                       Promedio_new=mean(new_vaccinations),
                                       D.Est_new=sd(new_vaccinations),
                                       CV_new=sd(new_vaccinations)/mean(new_vaccinations)*100,
                                       Min_new=min(new_vaccinations),
                                       Max_new=max(new_vaccinations))

Tasa_de_vacunas_DfUS21 <- DfUS21 %>% summarise(Promedio_total=mean(total_vaccinations_per_hundred),
                                               D.Est_total=sd(total_vaccinations_per_hundred),
                                               CV_total=
                                                 sd(total_vaccinations_per_hundred)/
                                                 mean(total_vaccinations_per_hundred)*100,
                                               Min_total=min(total_vaccinations_per_hundred),
                                               Max_total=max(total_vaccinations_per_hundred),
                                               Promedio_new=mean(new_vaccinations_per_hundred),
                                               D.Est_new=sd(new_vaccinations_per_hundred),
                                               CV_new=
                                                 sd(new_vaccinations_per_hundred)/
                                                 mean(new_vaccinations_per_hundred)*100,
                                               Min_new=min(new_vaccinations_per_hundred),
                                               Max_new=max(new_vaccinations_per_hundred))

Vacunas_DfUS22 <- DfUS22 %>% summarise(Promedio_total=mean(total_vaccinations),
                                       D.Est_total=sd(total_vaccinations),
                                       CV_total=sd(total_vaccinations)/mean(total_vaccinations)*100,
                                       Min_total=min(total_vaccinations),
                                       Max_total=max(total_vaccinations),
                                       Promedio_new=mean(new_vaccinations),
                                       D.Est_new=sd(new_vaccinations),
                                       CV_new=sd(new_vaccinations)/mean(new_vaccinations)*100,
                                       Min_new=min(new_vaccinations),
                                       Max_new=max(new_vaccinations))

Tasa_de_vacunas_DfUS22 <- DfUS22 %>% summarise(Promedio_total=mean(total_vaccinations_per_hundred),
                                               D.Est_total=sd(total_vaccinations_per_hundred),
                                               CV_total=
                                                 sd(total_vaccinations_per_hundred)/
                                                 mean(total_vaccinations_per_hundred)*100,
                                               Min_total=min(total_vaccinations_per_hundred),
                                               Max_total=max(total_vaccinations_per_hundred),
                                               Promedio_new=mean(new_vaccinations_per_hundred),
                                               D.Est_new=sd(new_vaccinations_per_hundred),
                                               CV_new=
                                                 sd(new_vaccinations_per_hundred)/
                                                 mean(new_vaccinations_per_hundred)*100,
                                               Min_new=min(new_vaccinations_per_hundred),
                                               Max_new=max(new_vaccinations_per_hundred))

Vacunas_DfUS23 <- DfUS23 %>% summarise(Promedio_total=mean(total_vaccinations),
                                       D.Est_total=sd(total_vaccinations),
                                       CV_total=sd(total_vaccinations)/mean(total_vaccinations)*100,
                                       Min_total=min(total_vaccinations),
                                       Max_total=max(total_vaccinations),
                                       Promedio_new=mean(new_vaccinations),
                                       D.Est_new=sd(new_vaccinations),
                                       CV_new=sd(new_vaccinations)/mean(new_vaccinations)*100,
                                       Min_new=min(new_vaccinations),
                                       Max_new=max(new_vaccinations))

Tasa_de_vacunas_DfUS23 <- DfUS23 %>% summarise(Promedio_total=mean(total_vaccinations_per_hundred),
                                               D.Est_total=sd(total_vaccinations_per_hundred),
                                               CV_total=
                                                 sd(total_vaccinations_per_hundred)/
                                                 mean(total_vaccinations_per_hundred)*100,
                                               Min_total=min(total_vaccinations_per_hundred),
                                               Max_total=max(total_vaccinations_per_hundred),
                                               Promedio_new=mean(new_vaccinations_per_hundred),
                                               D.Est_new=sd(new_vaccinations_per_hundred),
                                               CV_new=
                                                 sd(new_vaccinations_per_hundred)/
                                                 mean(new_vaccinations_per_hundred)*100,
                                               Min_new=min(new_vaccinations_per_hundred),
                                               Max_new=max(new_vaccinations_per_hundred))
#Estados Unidos, Decesos

Decesos_DfUS20 <- DfUS20 %>% summarise(Promedio_total=mean(total_deaths),
                                       D.Est_total=sd(total_deaths),
                                       CV_total=sd(total_deaths)/mean(total_deaths)*100,
                                       Min_total=min(total_deaths),
                                       Max_total=max(total_deaths),
                                       Promedio_new=mean(new_deaths),
                                       D.Est_new=sd(new_deaths),
                                       CV_new=sd(new_deaths)/mean(new_deaths)*100,
                                       Min_new=min(new_deaths),
                                       Max_new=max(new_deaths))

Tasa_de_decesos_DfUS20 <- DfUS20 %>% summarise(Promedio_total=mean(total_deaths_per_million),
                                               D.Est_total=sd(total_deaths_per_million),
                                               CV_total=
                                                 sd(total_deaths_per_million)/mean(total_deaths_per_million)*100,
                                               Min_total=min(total_deaths_per_million),
                                               Max_total=max(total_deaths_per_million),
                                               Promedio_new=mean(new_deaths_per_million),
                                               D.Est_new=sd(new_deaths_per_million),
                                               CV_new=
                                                 sd(new_deaths_per_million)/mean(new_deaths_per_million)*100,
                                               Min_new=min(new_deaths_per_million),
                                               Max_new=max(new_deaths_per_million))

Decesos_DfUS21 <- DfUS21 %>% summarise(Promedio_total=mean(total_deaths),
                                       D.Est_total=sd(total_deaths),
                                       CV_total=sd(total_deaths)/mean(total_deaths)*100,
                                       Min_total=min(total_deaths),
                                       Max_total=max(total_deaths),
                                       Promedio_new=mean(new_deaths),
                                       D.Est_new=sd(new_deaths),
                                       CV_new=sd(new_deaths)/mean(new_deaths)*100,
                                       Min_new=min(new_deaths),
                                       Max_new=max(new_deaths))

Tasa_de_decesos_DfUS21 <- DfUS21 %>% summarise(Promedio_total=mean(total_deaths_per_million),
                                               D.Est_total=sd(total_deaths_per_million),
                                               CV_total=
                                                 sd(total_deaths_per_million)/mean(total_deaths_per_million)*100,
                                               Min_total=min(total_deaths_per_million),
                                               Max_total=max(total_deaths_per_million),
                                               Promedio_new=mean(new_deaths_per_million),
                                               D.Est_new=sd(new_deaths_per_million),
                                               CV_new=
                                                 sd(new_deaths_per_million)/mean(new_deaths_per_million)*100,
                                               Min_new=min(new_deaths_per_million),
                                               Max_new=max(new_deaths_per_million))

Decesos_DfUS22 <- DfUS22 %>% summarise(Promedio_total=mean(total_deaths),
                                       D.Est_total=sd(total_deaths),
                                       CV_total=sd(total_deaths)/mean(total_deaths)*100,
                                       Min_total=min(total_deaths),
                                       Max_total=max(total_deaths),
                                       Promedio_new=mean(new_deaths),
                                       D.Est_new=sd(new_deaths),
                                       CV_new=sd(new_deaths)/mean(new_deaths)*100,
                                       Min_new=min(new_deaths),
                                       Max_new=max(new_deaths))

Tasa_de_decesos_DfUS22 <- DfUS22 %>% summarise(Promedio_total=mean(total_deaths_per_million),
                                               D.Est_total=sd(total_deaths_per_million),
                                               CV_total=
                                                 sd(total_deaths_per_million)/mean(total_deaths_per_million)*100,
                                               Min_total=min(total_deaths_per_million),
                                               Max_total=max(total_deaths_per_million),
                                               Promedio_new=mean(new_deaths_per_million),
                                               D.Est_new=sd(new_deaths_per_million),
                                               CV_new=
                                                 sd(new_deaths_per_million)/mean(new_deaths_per_million)*100,
                                               Min_new=min(new_deaths_per_million),
                                               Max_new=max(new_deaths_per_million))

Decesos_DfUS23 <- DfUS23 %>% summarise(Promedio_total=mean(total_deaths),
                                       D.Est_total=sd(total_deaths),
                                       CV_total=sd(total_deaths)/mean(total_deaths)*100,
                                       Min_total=min(total_deaths),
                                       Max_total=max(total_deaths),
                                       Promedio_new=mean(new_deaths),
                                       D.Est_new=sd(new_deaths),
                                       CV_new=sd(new_deaths)/mean(new_deaths)*100,
                                       Min_new=min(new_deaths),
                                       Max_new=max(new_deaths))

Tasa_de_decesos_DfUS23 <- DfUS23 %>% summarise(Promedio_total=mean(total_deaths_per_million),
                                               D.Est_total=sd(total_deaths_per_million),
                                               CV_total=
                                                 sd(total_deaths_per_million)/mean(total_deaths_per_million)*100,
                                               Min_total=min(total_deaths_per_million),
                                               Max_total=max(total_deaths_per_million),
                                               Promedio_new=mean(new_deaths_per_million),
                                               D.Est_new=sd(new_deaths_per_million),
                                               CV_new=
                                                 sd(new_deaths_per_million)/mean(new_deaths_per_million)*100,
                                               Min_new=min(new_deaths_per_million),
                                               Max_new=max(new_deaths_per_million))
#Alemania, Casos

Casos_DfDEU20 <- DfDEU20 %>% summarise(Promedio_total=mean(total_cases),
                                     D.Est_total=sd(total_cases),
                                     CV_total=sd(total_cases)/mean(total_cases)*100,
                                     Min_total=min(total_cases),
                                     Max_total=max(total_cases),
                                     Promedio_new=mean(new_cases),
                                     D.Est_new=sd(new_cases),
                                     CV_new=sd(new_cases)/mean(new_cases)*100,
                                     Min_new=min(new_cases),
                                     Max_new=max(new_cases))

Tasa_de_casos_DfDEU20 <- DfDEU20 %>% summarise(Promedio_total=mean(total_cases_per_million),
                                             D.Est_total=sd(total_cases_per_million),
                                             CV_total=
                                               sd(total_cases_per_million)/mean(total_cases_per_million)*100,
                                             Min_total=min(total_cases_per_million),
                                             Max_total=max(total_cases_per_million),
                                             Promedio_new=mean(new_cases_per_million),
                                             D.Est_new=sd(new_cases_per_million),
                                             CV_new=
                                               sd(new_cases_per_million)/mean(new_cases_per_million)*100,
                                             Min_new=min(new_cases_per_million),
                                             Max_new=max(new_cases_per_million))


Casos_DfDEU21 <- DfDEU21 %>% summarise(Promedio_total=mean(total_cases),
                                     D.Est_total=sd(total_cases),
                                     CV_total=sd(total_cases)/mean(total_cases)*100,
                                     Min_total=min(total_cases),
                                     Max_total=max(total_cases),
                                     Promedio_new=mean(new_cases),
                                     D.Est_new=sd(new_cases),
                                     CV_new=sd(new_cases)/mean(new_cases)*100,
                                     Min_new=min(new_cases),
                                     Max_new=max(new_cases))

Tasa_de_casos_DfDEU21 <- DfDEU21 %>% summarise(Promedio_total=mean(total_cases_per_million),
                                             D.Est_total=sd(total_cases_per_million),
                                             CV_total=
                                               sd(total_cases_per_million)/mean(total_cases_per_million)*100,
                                             Min_total=min(total_cases_per_million),
                                             Max_total=max(total_cases_per_million),
                                             Promedio_new=mean(new_cases_per_million),
                                             D.Est_new=sd(new_cases_per_million),
                                             CV_new=
                                               sd(new_cases_per_million)/mean(new_cases_per_million)*100,
                                             Min_new=min(new_cases_per_million),
                                             Max_new=max(new_cases_per_million))

Casos_DfDEU22 <- DfDEU22 %>% summarise(Promedio_total=mean(total_cases),
                                     D.Est_total=sd(total_cases),
                                     CV_total=sd(total_cases)/mean(total_cases)*100,
                                     Min_total=min(total_cases),
                                     Max_total=max(total_cases),
                                     Promedio_new=mean(new_cases),
                                     D.Est_new=sd(new_cases),
                                     CV_new=sd(new_cases)/mean(new_cases)*100,
                                     Min_new=min(new_cases),
                                     Max_new=max(new_cases))

Tasa_de_casos_DfDEU22 <- DfDEU22 %>% summarise(Promedio_total=mean(total_cases_per_million),
                                             D.Est_total=sd(total_cases_per_million),
                                             CV_total=
                                               sd(total_cases_per_million)/mean(total_cases_per_million)*100,
                                             Min_total=min(total_cases_per_million),
                                             Max_total=max(total_cases_per_million),
                                             Promedio_new=mean(new_cases_per_million),
                                             D.Est_new=sd(new_cases_per_million),
                                             CV_new=
                                               sd(new_cases_per_million)/mean(new_cases_per_million)*100,
                                             Min_new=min(new_cases_per_million),
                                             Max_new=max(new_cases_per_million))

Casos_DfDEU23 <- DfDEU23 %>% summarise(Promedio_total=mean(total_cases),
                                     D.Est_total=sd(total_cases),
                                     CV_total=sd(total_cases)/mean(total_cases)*100,
                                     Min_total=min(total_cases),
                                     Max_total=max(total_cases),
                                     Promedio_new=mean(new_cases),
                                     D.Est_new=sd(new_cases),
                                     CV_new=sd(new_cases)/mean(new_cases)*100,
                                     Min_new=min(new_cases),
                                     Max_new=max(new_cases))

Tasa_de_casos_DfDEU23 <- DfDEU23 %>% summarise(Promedio_total=mean(total_cases_per_million),
                                             D.Est_total=sd(total_cases_per_million),
                                             CV_total=
                                               sd(total_cases_per_million)/mean(total_cases_per_million)*100,
                                             Min_total=min(total_cases_per_million),
                                             Max_total=max(total_cases_per_million),
                                             Promedio_new=mean(new_cases_per_million),
                                             D.Est_new=sd(new_cases_per_million),
                                             CV_new=
                                               sd(new_cases_per_million)/mean(new_cases_per_million)*100,
                                             Min_new=min(new_cases_per_million),
                                             Max_new=max(new_cases_per_million))

#Vacunas en Alemania

Vacunas_DfDEU20 <- DfDEU20 %>% summarise(Promedio_total=mean(total_vaccinations),
                                       D.Est_total=sd(total_vaccinations),
                                       CV_total=sd(total_vaccinations)/mean(total_vaccinations)*100,
                                       Min_total=min(total_vaccinations),
                                       Max_total=max(total_vaccinations),
                                       Promedio_new=mean(new_vaccinations),
                                       D.Est_new=sd(new_vaccinations),
                                       CV_new=sd(new_vaccinations)/mean(new_vaccinations)*100,
                                       Min_new=min(new_vaccinations),
                                       Max_new=max(new_vaccinations))

Tasa_de_vacunas_DfDEU20 <- DfDEU20 %>% summarise(Promedio_total=mean(total_vaccinations_per_hundred),
                                               D.Est_total=sd(total_vaccinations_per_hundred),
                                               CV_total=
                                                 sd(total_vaccinations_per_hundred)/
                                                 mean(total_vaccinations_per_hundred)*100,
                                               Min_total=min(total_vaccinations_per_hundred),
                                               Max_total=max(total_vaccinations_per_hundred),
                                               Promedio_new=mean(new_vaccinations_per_hundred),
                                               D.Est_new=sd(new_vaccinations_per_hundred),
                                               CV_new=
                                                 sd(new_vaccinations_per_hundred)/
                                                 mean(new_vaccinations_per_hundred)*100,
                                               Min_new=min(new_vaccinations_per_hundred),
                                               Max_new=max(new_vaccinations_per_hundred))

Vacunas_DfDEU21 <- DfDEU21 %>% summarise(Promedio_total=mean(total_vaccinations),
                                       D.Est_total=sd(total_vaccinations),
                                       CV_total=sd(total_vaccinations)/mean(total_vaccinations)*100,
                                       Min_total=min(total_vaccinations),
                                       Max_total=max(total_vaccinations),
                                       Promedio_new=mean(new_vaccinations),
                                       D.Est_new=sd(new_vaccinations),
                                       CV_new=sd(new_vaccinations)/mean(new_vaccinations)*100,
                                       Min_new=min(new_vaccinations),
                                       Max_new=max(new_vaccinations))

Tasa_de_vacunas_DfDEU21 <- DfDEU21 %>% summarise(Promedio_total=mean(total_vaccinations_per_hundred),
                                               D.Est_total=sd(total_vaccinations_per_hundred),
                                               CV_total=
                                                 sd(total_vaccinations_per_hundred)/
                                                 mean(total_vaccinations_per_hundred)*100,
                                               Min_total=min(total_vaccinations_per_hundred),
                                               Max_total=max(total_vaccinations_per_hundred),
                                               Promedio_new=mean(new_vaccinations_per_hundred),
                                               D.Est_new=sd(new_vaccinations_per_hundred),
                                               CV_new=
                                                 sd(new_vaccinations_per_hundred)/
                                                 mean(new_vaccinations_per_hundred)*100,
                                               Min_new=min(new_vaccinations_per_hundred),
                                               Max_new=max(new_vaccinations_per_hundred))

Vacunas_DfDEU22 <- DfDEU22 %>% summarise(Promedio_total=mean(total_vaccinations),
                                       D.Est_total=sd(total_vaccinations),
                                       CV_total=sd(total_vaccinations)/mean(total_vaccinations)*100,
                                       Min_total=min(total_vaccinations),
                                       Max_total=max(total_vaccinations),
                                       Promedio_new=mean(new_vaccinations),
                                       D.Est_new=sd(new_vaccinations),
                                       CV_new=sd(new_vaccinations)/mean(new_vaccinations)*100,
                                       Min_new=min(new_vaccinations),
                                       Max_new=max(new_vaccinations))

Tasa_de_vacunas_DfDEU22 <- DfDEU22 %>% summarise(Promedio_total=mean(total_vaccinations_per_hundred),
                                               D.Est_total=sd(total_vaccinations_per_hundred),
                                               CV_total=
                                                 sd(total_vaccinations_per_hundred)/
                                                 mean(total_vaccinations_per_hundred)*100,
                                               Min_total=min(total_vaccinations_per_hundred),
                                               Max_total=max(total_vaccinations_per_hundred),
                                               Promedio_new=mean(new_vaccinations_per_hundred),
                                               D.Est_new=sd(new_vaccinations_per_hundred),
                                               CV_new=
                                                 sd(new_vaccinations_per_hundred)/
                                                 mean(new_vaccinations_per_hundred)*100,
                                               Min_new=min(new_vaccinations_per_hundred),
                                               Max_new=max(new_vaccinations_per_hundred))

Vacunas_DfDEU23 <- DfDEU23 %>% summarise(Promedio_total=mean(total_vaccinations),
                                       D.Est_total=sd(total_vaccinations),
                                       CV_total=sd(total_vaccinations)/mean(total_vaccinations)*100,
                                       Min_total=min(total_vaccinations),
                                       Max_total=max(total_vaccinations),
                                       Promedio_new=mean(new_vaccinations),
                                       D.Est_new=sd(new_vaccinations),
                                       CV_new=sd(new_vaccinations)/mean(new_vaccinations)*100,
                                       Min_new=min(new_vaccinations),
                                       Max_new=max(new_vaccinations))

Tasa_de_vacunas_DfDEU23 <- DfDEU23 %>% summarise(Promedio_total=mean(total_vaccinations_per_hundred),
                                               D.Est_total=sd(total_vaccinations_per_hundred),
                                               CV_total=
                                                 sd(total_vaccinations_per_hundred)/
                                                 mean(total_vaccinations_per_hundred)*100,
                                               Min_total=min(total_vaccinations_per_hundred),
                                               Max_total=max(total_vaccinations_per_hundred),
                                               Promedio_new=mean(new_vaccinations_per_hundred),
                                               D.Est_new=sd(new_vaccinations_per_hundred),
                                               CV_new=
                                                 sd(new_vaccinations_per_hundred)/
                                                 mean(new_vaccinations_per_hundred)*100,
                                               Min_new=min(new_vaccinations_per_hundred),
                                               Max_new=max(new_vaccinations_per_hundred))

#Alemania, Decesos

Decesos_DfDEU20 <- DfDEU20 %>% summarise(Promedio_total=mean(total_deaths),
                                       D.Est_total=sd(total_deaths),
                                       CV_total=sd(total_deaths)/mean(total_deaths)*100,
                                       Min_total=min(total_deaths),
                                       Max_total=max(total_deaths),
                                       Promedio_new=mean(new_deaths),
                                       D.Est_new=sd(new_deaths),
                                       CV_new=sd(new_deaths)/mean(new_deaths)*100,
                                       Min_new=min(new_deaths),
                                       Max_new=max(new_deaths))

Tasa_de_decesos_DfDEU20 <- DfDEU20 %>% summarise(Promedio_total=mean(total_deaths_per_million),
                                               D.Est_total=sd(total_deaths_per_million),
                                               CV_total=
                                                 sd(total_deaths_per_million)/mean(total_deaths_per_million)*100,
                                               Min_total=min(total_deaths_per_million),
                                               Max_total=max(total_deaths_per_million),
                                               Promedio_new=mean(new_deaths_per_million),
                                               D.Est_new=sd(new_deaths_per_million),
                                               CV_new=
                                                 sd(new_deaths_per_million)/mean(new_deaths_per_million)*100,
                                               Min_new=min(new_deaths_per_million),
                                               Max_new=max(new_deaths_per_million))

Decesos_DfDEU21 <- DfDEU21 %>% summarise(Promedio_total=mean(total_deaths),
                                       D.Est_total=sd(total_deaths),
                                       CV_total=sd(total_deaths)/mean(total_deaths)*100,
                                       Min_total=min(total_deaths),
                                       Max_total=max(total_deaths),
                                       Promedio_new=mean(new_deaths),
                                       D.Est_new=sd(new_deaths),
                                       CV_new=sd(new_deaths)/mean(new_deaths)*100,
                                       Min_new=min(new_deaths),
                                       Max_new=max(new_deaths))

Tasa_de_decesos_DfDEU21 <- DfDEU21 %>% summarise(Promedio_total=mean(total_deaths_per_million),
                                               D.Est_total=sd(total_deaths_per_million),
                                               CV_total=
                                                 sd(total_deaths_per_million)/mean(total_deaths_per_million)*100,
                                               Min_total=min(total_deaths_per_million),
                                               Max_total=max(total_deaths_per_million),
                                               Promedio_new=mean(new_deaths_per_million),
                                               D.Est_new=sd(new_deaths_per_million),
                                               CV_new=
                                                 sd(new_deaths_per_million)/mean(new_deaths_per_million)*100,
                                               Min_new=min(new_deaths_per_million),
                                               Max_new=max(new_deaths_per_million))

Decesos_DfDEU22 <- DfDEU22 %>% summarise(Promedio_total=mean(total_deaths),
                                       D.Est_total=sd(total_deaths),
                                       CV_total=sd(total_deaths)/mean(total_deaths)*100,
                                       Min_total=min(total_deaths),
                                       Max_total=max(total_deaths),
                                       Promedio_new=mean(new_deaths),
                                       D.Est_new=sd(new_deaths),
                                       CV_new=sd(new_deaths)/mean(new_deaths)*100,
                                       Min_new=min(new_deaths),
                                       Max_new=max(new_deaths))

Tasa_de_decesos_DfDEU22 <- DfDEU22 %>% summarise(Promedio_total=mean(total_deaths_per_million),
                                               D.Est_total=sd(total_deaths_per_million),
                                               CV_total=
                                                 sd(total_deaths_per_million)/mean(total_deaths_per_million)*100,
                                               Min_total=min(total_deaths_per_million),
                                               Max_total=max(total_deaths_per_million),
                                               Promedio_new=mean(new_deaths_per_million),
                                               D.Est_new=sd(new_deaths_per_million),
                                               CV_new=
                                                 sd(new_deaths_per_million)/mean(new_deaths_per_million)*100,
                                               Min_new=min(new_deaths_per_million),
                                               Max_new=max(new_deaths_per_million))

Decesos_DfDEU23 <- DfDEU23 %>% summarise(Promedio_total=mean(total_deaths),
                                       D.Est_total=sd(total_deaths),
                                       CV_total=sd(total_deaths)/mean(total_deaths)*100,
                                       Min_total=min(total_deaths),
                                       Max_total=max(total_deaths),
                                       Promedio_new=mean(new_deaths),
                                       D.Est_new=sd(new_deaths),
                                       CV_new=sd(new_deaths)/mean(new_deaths)*100,
                                       Min_new=min(new_deaths),
                                       Max_new=max(new_deaths))

Tasa_de_decesos_DfDEU23 <- DfDEU23 %>% summarise(Promedio_total=mean(total_deaths_per_million),
                                               D.Est_total=sd(total_deaths_per_million),
                                               CV_total=
                                                 sd(total_deaths_per_million)/mean(total_deaths_per_million)*100,
                                               Min_total=min(total_deaths_per_million),
                                               Max_total=max(total_deaths_per_million),
                                               Promedio_new=mean(new_deaths_per_million),
                                               D.Est_new=sd(new_deaths_per_million),
                                               CV_new=
                                                 sd(new_deaths_per_million)/mean(new_deaths_per_million)*100,
                                               Min_new=min(new_deaths_per_million),
                                               Max_new=max(new_deaths_per_million))

#Turquia, Casos
Casos_DfTUR20 <- DfTUR20 %>% summarise(Promedio_total=mean(total_cases),
                                       D.Est_total=sd(total_cases),
                                       CV_total=sd(total_cases)/mean(total_cases)*100,
                                       Min_total=min(total_cases),
                                       Max_total=max(total_cases),
                                       Promedio_new=mean(new_cases),
                                       D.Est_new=sd(new_cases),
                                       CV_new=sd(new_cases)/mean(new_cases)*100,
                                       Min_new=min(new_cases),
                                       Max_new=max(new_cases))

Tasa_de_casos_DfTUR20 <- DfTUR20 %>% summarise(Promedio_total=mean(total_cases_per_million),
                                               D.Est_total=sd(total_cases_per_million),
                                               CV_total=
                                                 sd(total_cases_per_million)/mean(total_cases_per_million)*100,
                                               Min_total=min(total_cases_per_million),
                                               Max_total=max(total_cases_per_million),
                                               Promedio_new=mean(new_cases_per_million),
                                               D.Est_new=sd(new_cases_per_million),
                                               CV_new=
                                                 sd(new_cases_per_million)/mean(new_cases_per_million)*100,
                                               Min_new=min(new_cases_per_million),
                                               Max_new=max(new_cases_per_million))


Casos_DfTUR21 <- DfTUR21 %>% summarise(Promedio_total=mean(total_cases),
                                       D.Est_total=sd(total_cases),
                                       CV_total=sd(total_cases)/mean(total_cases)*100,
                                       Min_total=min(total_cases),
                                       Max_total=max(total_cases),
                                       Promedio_new=mean(new_cases),
                                       D.Est_new=sd(new_cases),
                                       CV_new=sd(new_cases)/mean(new_cases)*100,
                                       Min_new=min(new_cases),
                                       Max_new=max(new_cases))

Tasa_de_casos_DfTUR21 <- DfTUR21 %>% summarise(Promedio_total=mean(total_cases_per_million),
                                               D.Est_total=sd(total_cases_per_million),
                                               CV_total=
                                                 sd(total_cases_per_million)/mean(total_cases_per_million)*100,
                                               Min_total=min(total_cases_per_million),
                                               Max_total=max(total_cases_per_million),
                                               Promedio_new=mean(new_cases_per_million),
                                               D.Est_new=sd(new_cases_per_million),
                                               CV_new=
                                                 sd(new_cases_per_million)/mean(new_cases_per_million)*100,
                                               Min_new=min(new_cases_per_million),
                                               Max_new=max(new_cases_per_million))

Casos_DfTUR22 <- DfTUR22 %>% summarise(Promedio_total=mean(total_cases),
                                       D.Est_total=sd(total_cases),
                                       CV_total=sd(total_cases)/mean(total_cases)*100,
                                       Min_total=min(total_cases),
                                       Max_total=max(total_cases),
                                       Promedio_new=mean(new_cases),
                                       D.Est_new=sd(new_cases),
                                       CV_new=sd(new_cases)/mean(new_cases)*100,
                                       Min_new=min(new_cases),
                                       Max_new=max(new_cases))

Tasa_de_casos_DfTUR22 <- DfTUR22 %>% summarise(Promedio_total=mean(total_cases_per_million),
                                               D.Est_total=sd(total_cases_per_million),
                                               CV_total=
                                                 sd(total_cases_per_million)/mean(total_cases_per_million)*100,
                                               Min_total=min(total_cases_per_million),
                                               Max_total=max(total_cases_per_million),
                                               Promedio_new=mean(new_cases_per_million),
                                               D.Est_new=sd(new_cases_per_million),
                                               CV_new=
                                                 sd(new_cases_per_million)/mean(new_cases_per_million)*100,
                                               Min_new=min(new_cases_per_million),
                                               Max_new=max(new_cases_per_million))

Casos_DfTUR23 <- DfTUR23 %>% summarise(Promedio_total=mean(total_cases),
                                       D.Est_total=sd(total_cases),
                                       CV_total=sd(total_cases)/mean(total_cases)*100,
                                       Min_total=min(total_cases),
                                       Max_total=max(total_cases),
                                       Promedio_new=mean(new_cases),
                                       D.Est_new=sd(new_cases),
                                       CV_new=sd(new_cases)/mean(new_cases)*100,
                                       Min_new=min(new_cases),
                                       Max_new=max(new_cases))

Tasa_de_casos_DfTUR23 <- DfTUR23 %>% summarise(Promedio_total=mean(total_cases_per_million),
                                               D.Est_total=sd(total_cases_per_million),
                                               CV_total=
                                                 sd(total_cases_per_million)/mean(total_cases_per_million)*100,
                                               Min_total=min(total_cases_per_million),
                                               Max_total=max(total_cases_per_million),
                                               Promedio_new=mean(new_cases_per_million),
                                               D.Est_new=sd(new_cases_per_million),
                                               CV_new=
                                                 sd(new_cases_per_million)/mean(new_cases_per_million)*100,
                                               Min_new=min(new_cases_per_million),
                                               Max_new=max(new_cases_per_million))

#Vacunas en Turquia

Vacunas_DfTUR20 <- DfTUR20 %>% summarise(Promedio_total=mean(total_vaccinations),
                                         D.Est_total=sd(total_vaccinations),
                                         CV_total=sd(total_vaccinations)/mean(total_vaccinations)*100,
                                         Min_total=min(total_vaccinations),
                                         Max_total=max(total_vaccinations),
                                         Promedio_new=mean(new_vaccinations),
                                         D.Est_new=sd(new_vaccinations),
                                         CV_new=sd(new_vaccinations)/mean(new_vaccinations)*100,
                                         Min_new=min(new_vaccinations),
                                         Max_new=max(new_vaccinations))

Tasa_de_vacunas_DfTUR20 <- DfTUR20 %>% summarise(Promedio_total=mean(total_vaccinations_per_hundred),
                                                 D.Est_total=sd(total_vaccinations_per_hundred),
                                                 CV_total=
                                                   sd(total_vaccinations_per_hundred)/
                                                   mean(total_vaccinations_per_hundred)*100,
                                                 Min_total=min(total_vaccinations_per_hundred),
                                                 Max_total=max(total_vaccinations_per_hundred),
                                                 Promedio_new=mean(new_vaccinations_per_hundred),
                                                 D.Est_new=sd(new_vaccinations_per_hundred),
                                                 CV_new=
                                                   sd(new_vaccinations_per_hundred)/
                                                   mean(new_vaccinations_per_hundred)*100,
                                                 Min_new=min(new_vaccinations_per_hundred),
                                                 Max_new=max(new_vaccinations_per_hundred))

Vacunas_DfTUR21 <- DfTUR21 %>% summarise(Promedio_total=mean(total_vaccinations),
                                         D.Est_total=sd(total_vaccinations),
                                         CV_total=sd(total_vaccinations)/mean(total_vaccinations)*100,
                                         Min_total=min(total_vaccinations),
                                         Max_total=max(total_vaccinations),
                                         Promedio_new=mean(new_vaccinations),
                                         D.Est_new=sd(new_vaccinations),
                                         CV_new=sd(new_vaccinations)/mean(new_vaccinations)*100,
                                         Min_new=min(new_vaccinations),
                                         Max_new=max(new_vaccinations))

Tasa_de_vacunas_DfTUR21 <- DfTUR21 %>% summarise(Promedio_total=mean(total_vaccinations_per_hundred),
                                                 D.Est_total=sd(total_vaccinations_per_hundred),
                                                 CV_total=
                                                   sd(total_vaccinations_per_hundred)/
                                                   mean(total_vaccinations_per_hundred)*100,
                                                 Min_total=min(total_vaccinations_per_hundred),
                                                 Max_total=max(total_vaccinations_per_hundred),
                                                 Promedio_new=mean(new_vaccinations_per_hundred),
                                                 D.Est_new=sd(new_vaccinations_per_hundred),
                                                 CV_new=
                                                   sd(new_vaccinations_per_hundred)/
                                                   mean(new_vaccinations_per_hundred)*100,
                                                 Min_new=min(new_vaccinations_per_hundred),
                                                 Max_new=max(new_vaccinations_per_hundred))

Vacunas_DfTUR22 <- DfTUR22 %>% summarise(Promedio_total=mean(total_vaccinations),
                                         D.Est_total=sd(total_vaccinations),
                                         CV_total=sd(total_vaccinations)/mean(total_vaccinations)*100,
                                         Min_total=min(total_vaccinations),
                                         Max_total=max(total_vaccinations),
                                         Promedio_new=mean(new_vaccinations),
                                         D.Est_new=sd(new_vaccinations),
                                         CV_new=sd(new_vaccinations)/mean(new_vaccinations)*100,
                                         Min_new=min(new_vaccinations),
                                         Max_new=max(new_vaccinations))

Tasa_de_vacunas_DfTUR22 <- DfTUR22 %>% summarise(Promedio_total=mean(total_vaccinations_per_hundred),
                                                 D.Est_total=sd(total_vaccinations_per_hundred),
                                                 CV_total=
                                                   sd(total_vaccinations_per_hundred)/
                                                   mean(total_vaccinations_per_hundred)*100,
                                                 Min_total=min(total_vaccinations_per_hundred),
                                                 Max_total=max(total_vaccinations_per_hundred),
                                                 Promedio_new=mean(new_vaccinations_per_hundred),
                                                 D.Est_new=sd(new_vaccinations_per_hundred),
                                                 CV_new=
                                                   sd(new_vaccinations_per_hundred)/
                                                   mean(new_vaccinations_per_hundred)*100,
                                                 Min_new=min(new_vaccinations_per_hundred),
                                                 Max_new=max(new_vaccinations_per_hundred))

Vacunas_DfTUR23 <- DfTUR23 %>% summarise(Promedio_total=mean(total_vaccinations),
                                         D.Est_total=sd(total_vaccinations),
                                         CV_total=sd(total_vaccinations)/mean(total_vaccinations)*100,
                                         Min_total=min(total_vaccinations),
                                         Max_total=max(total_vaccinations),
                                         Promedio_new=mean(new_vaccinations),
                                         D.Est_new=sd(new_vaccinations),
                                         CV_new=sd(new_vaccinations)/mean(new_vaccinations)*100,
                                         Min_new=min(new_vaccinations),
                                         Max_new=max(new_vaccinations))

Tasa_de_vacunas_DfTUR23 <- DfTUR23 %>% summarise(Promedio_total=mean(total_vaccinations_per_hundred),
                                                 D.Est_total=sd(total_vaccinations_per_hundred),
                                                 CV_total=
                                                   sd(total_vaccinations_per_hundred)/
                                                   mean(total_vaccinations_per_hundred)*100,
                                                 Min_total=min(total_vaccinations_per_hundred),
                                                 Max_total=max(total_vaccinations_per_hundred),
                                                 Promedio_new=mean(new_vaccinations_per_hundred),
                                                 D.Est_new=sd(new_vaccinations_per_hundred),
                                                 CV_new=
                                                   sd(new_vaccinations_per_hundred)/
                                                   mean(new_vaccinations_per_hundred)*100,
                                                 Min_new=min(new_vaccinations_per_hundred),
                                                 Max_new=max(new_vaccinations_per_hundred))

#Turquia, Decesos

Decesos_DfTUR20 <- DfTUR20 %>% summarise(Promedio_total=mean(total_deaths),
                                         D.Est_total=sd(total_deaths),
                                         CV_total=sd(total_deaths)/mean(total_deaths)*100,
                                         Min_total=min(total_deaths),
                                         Max_total=max(total_deaths),
                                         Promedio_new=mean(new_deaths),
                                         D.Est_new=sd(new_deaths),
                                         CV_new=sd(new_deaths)/mean(new_deaths)*100,
                                         Min_new=min(new_deaths),
                                         Max_new=max(new_deaths))

Tasa_de_decesos_DfTUR20 <- DfTUR20 %>% summarise(Promedio_total=mean(total_deaths_per_million),
                                                 D.Est_total=sd(total_deaths_per_million),
                                                 CV_total=
                                                   sd(total_deaths_per_million)/mean(total_deaths_per_million)*100,
                                                 Min_total=min(total_deaths_per_million),
                                                 Max_total=max(total_deaths_per_million),
                                                 Promedio_new=mean(new_deaths_per_million),
                                                 D.Est_new=sd(new_deaths_per_million),
                                                 CV_new=
                                                   sd(new_deaths_per_million)/mean(new_deaths_per_million)*100,
                                                 Min_new=min(new_deaths_per_million),
                                                 Max_new=max(new_deaths_per_million))

Decesos_DfTUR21 <- DfTUR21 %>% summarise(Promedio_total=mean(total_deaths),
                                         D.Est_total=sd(total_deaths),
                                         CV_total=sd(total_deaths)/mean(total_deaths)*100,
                                         Min_total=min(total_deaths),
                                         Max_total=max(total_deaths),
                                         Promedio_new=mean(new_deaths),
                                         D.Est_new=sd(new_deaths),
                                         CV_new=sd(new_deaths)/mean(new_deaths)*100,
                                         Min_new=min(new_deaths),
                                         Max_new=max(new_deaths))

Tasa_de_decesos_DfTUR21 <- DfTUR21 %>% summarise(Promedio_total=mean(total_deaths_per_million),
                                                 D.Est_total=sd(total_deaths_per_million),
                                                 CV_total=
                                                   sd(total_deaths_per_million)/mean(total_deaths_per_million)*100,
                                                 Min_total=min(total_deaths_per_million),
                                                 Max_total=max(total_deaths_per_million),
                                                 Promedio_new=mean(new_deaths_per_million),
                                                 D.Est_new=sd(new_deaths_per_million),
                                                 CV_new=
                                                   sd(new_deaths_per_million)/mean(new_deaths_per_million)*100,
                                                 Min_new=min(new_deaths_per_million),
                                                 Max_new=max(new_deaths_per_million))

Decesos_DfTUR22 <- DfTUR22 %>% summarise(Promedio_total=mean(total_deaths),
                                         D.Est_total=sd(total_deaths),
                                         CV_total=sd(total_deaths)/mean(total_deaths)*100,
                                         Min_total=min(total_deaths),
                                         Max_total=max(total_deaths),
                                         Promedio_new=mean(new_deaths),
                                         D.Est_new=sd(new_deaths),
                                         CV_new=sd(new_deaths)/mean(new_deaths)*100,
                                         Min_new=min(new_deaths),
                                         Max_new=max(new_deaths))

Tasa_de_decesos_DfTUR22 <- DfTUR22 %>% summarise(Promedio_total=mean(total_deaths_per_million),
                                                 D.Est_total=sd(total_deaths_per_million),
                                                 CV_total=
                                                   sd(total_deaths_per_million)/mean(total_deaths_per_million)*100,
                                                 Min_total=min(total_deaths_per_million),
                                                 Max_total=max(total_deaths_per_million),
                                                 Promedio_new=mean(new_deaths_per_million),
                                                 D.Est_new=sd(new_deaths_per_million),
                                                 CV_new=
                                                   sd(new_deaths_per_million)/mean(new_deaths_per_million)*100,
                                                 Min_new=min(new_deaths_per_million),
                                                 Max_new=max(new_deaths_per_million))

Decesos_DfTUR23 <- DfTUR23 %>% summarise(Promedio_total=mean(total_deaths),
                                         D.Est_total=sd(total_deaths),
                                         CV_total=sd(total_deaths)/mean(total_deaths)*100,
                                         Min_total=min(total_deaths),
                                         Max_total=max(total_deaths),
                                         Promedio_new=mean(new_deaths),
                                         D.Est_new=sd(new_deaths),
                                         CV_new=sd(new_deaths)/mean(new_deaths)*100,
                                         Min_new=min(new_deaths),
                                         Max_new=max(new_deaths))

Tasa_de_decesos_DfTUR23 <- DfTUR23 %>% summarise(Promedio_total=mean(total_deaths_per_million),
                                                 D.Est_total=sd(total_deaths_per_million),
                                                 CV_total=
                                                   sd(total_deaths_per_million)/mean(total_deaths_per_million)*100,
                                                 Min_total=min(total_deaths_per_million),
                                                 Max_total=max(total_deaths_per_million),
                                                 Promedio_new=mean(new_deaths_per_million),
                                                 D.Est_new=sd(new_deaths_per_million),
                                                 CV_new=
                                                   sd(new_deaths_per_million)/mean(new_deaths_per_million)*100,
                                                 Min_new=min(new_deaths_per_million),
                                                 Max_new=max(new_deaths_per_million))



#Graficos correspondientes a DEU, TUR y US


#Estudio del pais España en los cuatro años de pandemia

#Uso de summary para ver la informacion estadsitica de españa
summary(DfESP)
#Vemos la cantidad de Na, para empezar a trabajar separamos por cada año de pandemia
ESP21 <- data_frame (DfESP [365:729,])
ESP22 <-data.frame (DfESP [730:1094,])
ESP23 <- data.frame(DfESP [DfESP$date>"2022-12-31",])
ESP20 <- data.frame(DfESP [DfESP$date<"2021-01-01",])

#Luego podemos ver mejor la cantidad Na que tenemos por pais
glimpse(ESP20)
glimpse(ESP21)
glimpse(ESP22)

#España en el año 2020, no hay registro en las variables relacionadas con las
#vacunas, ya que estas mismas llegaron el 27 de dicembre del mismo año
#Los valores NA que contiene el año 2020, son 0, contando tambien principios de años que no se registro casos
#Rellenamos los valores NA de España

is.na(ESP20)
ESP20[is.na(ESP20)] <- 0

#En el año 21 ya comienzan a ser registradas las personas que son vacunadas, los datos Na de este año
#son 0 y/o Dias que no hubo nuevos vacunados

#para rellenar los valores na del año 2021 tendremos que rellenarlos por segmento y por dia
is.na(ESP21)
#Dia 1 al 3, Los 3 primeros dias del año no tenemos registro alguno sobre las vacunas
ESP21 [1:3,13] <- 0
ESP21 [1:3,14] <- 0
ESP21 [1:3,15] <- 0
ESP21 [1:3,16] <- 0
ESP21 [1:3,17] <- 0
#Dia 4, comenzamos a tener vacunas
ESP21 [4,14] <- 82834
#Dia 6
ESP21 [6,13] <- 139339
ESP21 [6,14] <- 0
ESP21 [6,15] <- 0.29
ESP21 [6,16] <- 139339
ESP21 [6,17] <- 0.29
#Dia 7
ESP21 [7,14] <- 67984
#Dias 9 y 10
ESP21 [9:10,13] <- 277976
ESP21 [9:10,14] <- 0
ESP21 [9:10,15] <- 0.58
ESP21 [9:10,16] <- 277976
ESP21 [9:10,17] <- 0.58
#Dia 11
ESP21 [11,14] <- (406091-277976)
#Dia 16 al 17
ESP21 [16:17,13] <- 768950
ESP21 [16:17,14] <- 0
ESP21 [16:17,15] <- 1.62
ESP21 [16:17,16] <- 768950
ESP21 [16:17,17] <- 1.62
#Dia 18
ESP21 [18,14] <- (897942-768950)
#Dia 23
ESP21 [23,13] <- 1165825
ESP21 [23,14] <- 0
ESP21 [23,15] <- 2.45 
ESP21 [23,16] <- 1097369
ESP21 [23,17] <- 2.31
#Dia 24
ESP21 [24,14] <- (1237593-1165825)
#Dia 29 al 30
ESP21 [29:30,13] <- 1474189
ESP21 [29:30,14] <- 0
ESP21 [29:30,15] <- 3.10
ESP21 [29:30,16] <- 1222323
ESP21 [29:30,17] <- 2.59
#Dia 31
ESP21 [31,14] <- (1609261-1474189)
#Dia 36 al 37
ESP21 [36:37,13] <- 1988160
ESP21 [36:37,14] <- 0
ESP21 [36:37,15] <- 4.18
ESP21 [36:37,16] <- 1305251
ESP21 [36:37,17] <- 2.74
#Dia 38
ESP21 [38,14] <- (2105033-1988160)
#Dia 43 al 44
ESP21 [43:44,13] <- 2423045
ESP21 [43:44,14] <- 0
ESP21 [43:44,15] <- 5.09
ESP21 [43:44,16] <- 1422560
ESP21 [43:44,17] <- 2.99
#Dia 45
ESP21 [45,14] <- (2561608-2423045)
#Dia 50 al 51
ESP21 [50:51,13] <- 2936011
ESP21 [50:51,14] <- 0
ESP21 [50:51,15] <- 6.17
ESP21 [50:51,16] <- 1764985
ESP21 [50:51,17] <- 3.71
#Dia 52
ESP21 [52,14] <- (3090351-2936011)
#Dia 57 al 58
ESP21 [57:58,13] <- 3605635
ESP21 [57:58,14] <- 0
ESP21 [57:58,15] <- 7.58
ESP21 [57:58,16] <- 2361852
ESP21 [57:58,17] <- 4.97
#Dia 59
ESP21 [59,14] <- (3829465-3605635)
#Dia 64 al 65
ESP21 [64:65,13] <- 4471577
ESP21 [64:65,14] <- 0
ESP21 [64:65,15] <- 9.40
ESP21 [64:65,16] <- 3129092
ESP21 [64:65,17] <- 6.58
#Dia 66
ESP21 [66,14] <- (4712191-4471577)
#Dia 71 al 73
ESP21 [71:73,13] <- 5352767
ESP21 [71:73,14] <- 0
ESP21 [71:73,15] <- 11.26
ESP21 [71:73,16] <- 3769523
ESP21 [71:73,17] <- 7.93
#Dia 74
ESP21 [74,14] <- (5644895-5352767)
#Dia 78 al 80
ESP21 [78:80,13] <- 5993363
ESP21 [78:80,14] <- 0
ESP21 [78:80,15] <- 12.60
ESP21 [78:80,16] <- 4106550
ESP21 [78:80,17] <- 8.63
#Dia 81
ESP21 [81,14] <- (6321908-5993363)
#Dia 86 al 87
ESP21 [86:87,13] <- 7067371
ESP21 [86:87,14] <- 0
ESP21 [86:87,15] <- 14.86
ESP21 [86:87,16] <- 4561529
ESP21 [86:87,17] <- 9.59
#Dia 88
ESP21 [88,14] <- (7571439-7067371)
#Dia 92
ESP21 [92,13] <- 8342155
ESP21 [92,14] <- 0
ESP21 [92,15] <- 17.54
ESP21 [92,16] <- 5545140
ESP21 [92,17] <- 11.66
#Dia 93
ESP21 [93,14] <- (8548598-8342155)
#Dia 94
ESP21 [94,13] <- 8548598
ESP21 [94,14] <- 0
ESP21 [94,15] <- 17.97
ESP21 [94,16] <- 5707450
ESP21 [94,17] <- 12.00
#Dia 95
ESP21 [95, 14] <- (8748694-8548598)
#Dia 101 al 102
ESP21 [100:102,13] <- 10231825
ESP21 [100:102,14] <- 0
ESP21 [100:102,15] <- 21.51
ESP21 [100:102,16] <- 7159716
ESP21 [100:102,17] <- 15.05
#Dia 103
ESP21 [103,14] <- (10784997-10231825)
#Desde el dia 107 al dia 108
ESP21 [107:108,13] <- 12330755
ESP21 [107:108,14] <- 0
ESP21 [107:108,15] <- 25.93
ESP21 [107:108,16] <- 9002054
ESP21 [107:108,17] <- 18.93
#Dia 109
ESP21 [109,14] <- (12853599-12330755)
#Dia 114
ESP21 [114,13] <- 14266251
ESP21 [114,14] <- 0
ESP21 [114,15] <- 30.00
ESP21 [114,16] <- 10405863
ESP21 [114,17] <- 21.88
#Dia 115
ESP21 [115,14] <- (14715931-14266251)
#Dia 120 al 121
ESP21 [120:121,13] <- 16364595
ESP21 [120:121,14] <- 0
ESP21 [120:121,15] <- 34.41
ESP21 [120:121,16] <- 11763360
ESP21 [120:121,17] <- 24.73
#Dia 122
ESP21 [122,14] <- (17178566-16364595)
#Dia 127 al 128
ESP21 [127:128,13] <- 19048132
ESP21 [127:128,14] <- 0
ESP21 [127:128,15] <- 40.05
ESP21 [127:128,16] <- 13271511
ESP21 [127:128,17] <- 27.91
#Dia 129
ESP21 [129,14] <- (19830624-19048132)
#Dia 134 al 135
ESP21 [134:135,13] <- 21683707
ESP21 [134:135,14] <- 0
ESP21 [134:135,15] <- 45.59
ESP21 [134:135,16] <- 14911306
ESP21 [134:135,17] <- 31.35
#Dia 136
ESP21 [136,14] <- (22313088-21683707)
#Dia 141 al 142
ESP21 [141:142,13] <- 23962365
ESP21 [141:142,14] <- 0
ESP21 [141:142,15] <- 50.38
ESP21 [141:142,16] <- 16347683
ESP21 [141:142,17] <- 34.37
#Dia 143
ESP21 [143,14] <- (24534056-23962365)
#Dia 148 al 149
ESP21 [148:149,13] <- 26133689
ESP21 [148:149,14] <- 0
ESP21 [148:149,15] <- 54.95
ESP21 [148:149,16] <- 17707320
ESP21 [148:149,17] <- 37.23
#Dia 150
ESP21 [150,14] <- (26827637-26133689)
#Dia 155 al 156
ESP21 [155:156,13] <- 28752570
ESP21 [155:156,14] <- 0
ESP21 [155:156,15] <- 60.46
ESP21 [155:156,16] <- 19038135
ESP21 [155:156,17] <- 40.03
#Dia 157
ESP21 [157,14] <- (30007806-28752570)
#Dia 162 al 163
ESP21 [162:163,13] <- 32282553
ESP21 [162:163,14] <- 0
ESP21 [162:163,15] <- 67.88
ESP21 [162:163,16] <- 20963619
ESP21 [162:163,17] <- 44.08
#Dia 164 
ESP21 [164,14] <- (33242768-32282533)
#Dia 169 al 170
ESP21 [169:170,13] <- 35412140
ESP21 [169:170,14] <- 0
ESP21 [169:170,15] <- 74.46
ESP21 [169:170,16] <- 22684911
ESP21 [169:170,17] <- 47.70
#Dia 171
ESP21 [171,14] <- (36357745-35412140)
#Dia 176 al 177
ESP21 [176:177,13] <- 38808490
ESP21 [176:177,14] <- 0
ESP21 [176:177,15] <- 81.60
ESP21 [176:177,16] <- 24325902
ESP21 [176:177,17] <- 51.15
#Dia 178
ESP21 [178,14] <- (39914204-38808490)
#Dia 183 al 184
ESP21 [183:184,13] <- 42792983
ESP21 [183:184,14] <- 0
ESP21 [183:184,15] <- 89.98
ESP21 [183:184,16] <- 25948237
ESP21 [183:184,17] <- 54.56
#Dia 185
ESP21 [185,14] <- (43959621-42792983)
#Dia 190 al 191
ESP21 [190:191,13] <- 46612489
ESP21 [190:191,14] <- 0
ESP21 [190:191,15] <- 98.01
ESP21 [190:191,16] <- 27534723
ESP21 [190:191,17] <- 57.90
#Dia 192
ESP21 [192,14] <- (47777316-46612489)
#Dia 197 al 198
ESP21 [197:198,13] <- 50239048
ESP21 [197:198,14] <- 0
ESP21 [197:198,15] <- 105.64
ESP21 [197:198,16] <- 29033376
ESP21 [197:198,17] <- 61.05
#Dia 199
ESP21 [199,14] <- (51229989-50239048)
#Dia 204 al 205
ESP21 [204:205,13] <- 53449255
ESP21 [204:205,14] <- 0
ESP21 [204:205,15] <- 112.39
ESP21 [204:205,16] <- 30500003
ESP21 [204:205,17] <- 64.13
#Dia 206
ESP21 [206,14] <- (54259325-53449255)
#Dia 211 al 212
ESP21 [211:212,13] <- 56186601
ESP21 [211:212,14] <- 0
ESP21 [211:212,15] <- 118.14
ESP21 [211:212,16] <- 31851618
ESP21 [211:212,17] <- 66.97
#Dia 213
ESP21 [213,14] <- (56795869-56186601)
#Dia 218 al 219
ESP21 [218:219,13] <- 58542495
ESP21 [218:219,14] <- 0
ESP21 [218:219,15] <- 123.10
ESP21 [218:219,16] <- 33141450
ESP21 [218:219,17] <- 69.69
#Dia 220
ESP21 [220,14] <- (59108679-58542495)
#Dia 225 al 226
ESP21 [225:226,13] <- 60919867
ESP21 [225:226,14] <- 0
ESP21 [225:226,15] <- 128.09
ESP21 [225:226,16] <- 34455343
ESP21 [225:226,17] <- 72.45
#Dia 227
ESP21 [227,14] <- (61418303-60919867)
#Dia 232 al 233
ESP21 [232:233,13] <- 63090941
ESP21 [232:233,14] <- 0
ESP21 [232:233,15] <- 132.66
ESP21 [232:233,16] <- 35442173
ESP21 [232:233,17] <- 74.52
#Dia 234 
ESP21 [234,14] <- (63590985-63090941)
#Dia 239 al 240
ESP21 [239:240,13] <- 63035990
ESP21 [239:240,14] <- 0
ESP21 [239:240,15] <- 136.75
ESP21 [239:240,16] <- 36237517
ESP21 [239:240,17] <- 76.20
#Dia 241
ESP21 [241,14] <- (65444891-63035990)
#Dia 246 al 247
ESP21 [246:247,13] <- 66788317
ESP21 [246:247,14] <- 0
ESP21 [246:247,15] <- 140.43
ESP21 [246:247,16] <- 36696877
ESP21 [246:247,17] <- 77.16
#Dia 248
ESP21 [248,14] <- (67190707-66788317)
#Dia 253 al 254
ESP21 [253:254,13] <-68205694
ESP21 [253:254,14] <- 0
ESP21 [253:254,15] <- 143.41
ESP21 [253:254,16] <- 37126744
ESP21 [253:254,17] <- 78.07
#Dia 255
ESP21 [255,14] <- (68486683-68205694)
#Dia 260 al 261
ESP21 [260:261,13] <- 69094664
ESP21 [260:261,14] <- 0
ESP21 [260:261,15] <- 145.28
ESP21 [260:261,16] <- 37385758
ESP21 [260:261,17] <- 78.61
#Dia 262
ESP21 [262,14] <- (69427047-69094664)
#Dia 263
ESP21 [263,13] <- 69427047
ESP21 [263,14] <- 0
ESP21 [263,15] <- 145.98
ESP21 [263,16] <- 37472155
ESP21 [263,17] <- 78.79
#Dia 264
ESP21 [264,14] <- (69549078-69427047)
#Dias 267 al 268
ESP21 [267:268,13] <- 69740837
ESP21 [267:268,14] <- 0
ESP21 [267:268,15] <- 146.64
ESP21 [267:268,16] <- 37571186
ESP21 [267:268,17] <- 79.00
#Dia 269
ESP21  [269,14] <- (69867532-69740837)
#Dias 274 al 275 
ESP21 [274:275,13] <- 70153507
ESP21 [274:275,14] <- 0
ESP21 [274:275,15] <- 147.51
ESP21 [274:275,16] <- 37702384
ESP21 [274:275,17] <- 79.28
#Dia 276
ESP21 [276,14] <- (70243260-70153507)
#Dias 281 al 282
ESP21 [281:282,13] <- 70699744
ESP21 [281:282,14] <- 0
ESP21 [281:282,15] <- 148.66       
ESP21 [281:282,16] <- 37788299
ESP21 [281:282,17] <- 79.46
#Dia 283
ESP21 [283,14] <- (70822813-70699744)
#Dia 284 al 285
ESP21 [284:285,13] <- 70822813
ESP21 [284:285,14] <- 0
ESP21 [284:285,15] <- 148.92
ESP21 [284:285,16] <- 37816635
ESP21 [284:285,17] <- 79.52
#Dias 288 al 289
ESP21 [288:289,13] <- 70982052
ESP21 [288:289,14] <- 0
ESP21 [288:289,15] <- 149.25
ESP21 [288:289,16] <- 37868453
ESP21 [288:289,17] <- 79.62
#Dia 290
ESP21 [290,14] <- (71048577-70982052)
#Dias 295 al 296
ESP21 [295:296,13] <- 71275614
ESP21 [285:296,14] <- 0
ESP21 [295:296,15] <- 149.87
ESP21 [295:296,16] <- 37954027
ESP21 [295:296,17] <- 79.80
#Dia 297
ESP21 [297,14] <- (71344477-71275614)
#Dia 302
ESP21 [302,13] <- 71943760
ESP21 [302,14] <- 0
ESP21 [302,15] <- 151.27
ESP21 [302,16] <- 38036436
ESP21 [302,17] <- 79.98
#Dia 303
ESP21 [303,14] <- (72058020-71943760)
#Dia 304 al 305
ESP21 [304:305,13] <- 72058020
ESP21 [304:305,14] <- 0
ESP21 [304:305,15] <- 151.51
ESP21 [304:305,16] <- 38037531
ESP21 [304:305,17] <- 79.98
#Dia 306
ESP21 [306,14] <- (72233182-72058020)
#Dias 309 al 310
ESP21 [309:310,13] <- 72594573
ESP21 [309:310,14] <- 0
ESP21 [309:310,15] <- 152.64
ESP21 [309:310,16] <- 38083061
ESP21 [309:310,17] <- 80.08
#Dia 311
ESP21 [311,14] <- (72805128-72594573)
#Dia 312
ESP21 [312,13] <- 72805128
ESP21 [312,14] <- 0
ESP21 [312,15] <- 153.08
ESP21 [312,16] <- 38100169
ESP21 [312,17] <- 80.11 
#Dia 313
ESP21 [313,14] <- (72149785-72805128)
#Dias 316 al 317 
ESP21 [316:317,13] <- 73623626
ESP21 [316:317,14] <- 0
ESP21 [316:317,15] <- 154.81
ESP21 [316:317,16] <- 38144625
ESP21 [316:317,17] <- 80.21
#Dia 318
ESP21 [318,14] <- (73886764-73623626)
#Dias 323 al 324 
ESP21 [323:324,13] <- 74683916
ESP21 [323:324,14] <- 0
ESP21 [323:324,15] <- 157.04
ESP21 [323:324,16] <- 38209702
ESP21 [323:324,17] <- 80.34
#Dia 325 
ESP21 [325,14] <- (74988489-74683916)
#Dias 330 al 331
ESP21 [330:331,13] <- 76026523
ESP21 [330:331,14] <- 0
ESP21 [330:331,15] <- 159.86
ESP21 [330:331,16] <- 38288327
ESP21 [330:331,17] <- 80.51
#Dia 332 
ESP21 [332,13] <- 77157350
ESP21 [332,14] <- (77157350-76026523)
#Dias 337 al 339
ESP21 [337:339,13] <- 77157350
ESP21 [337:339,14] <- 0
ESP21 [337:339,15] <- 162.24
ESP21 [337:339,16] <- 38409145
ESP21 [337:339,17] <- 80.76
#Dia 340
ESP21 [340,14] <- (77541843-77157350)
#Dia 341
ESP21 [341,13] <- 77541843
ESP21 [341,14] <- 0
ESP21 [341,15] <- 163.04
ESP21 [341,16] <- 38447301
ESP21 [341,17] <- 80.84
#Dia 342
ESP21 [342,14] <- (77653701-77541843)  
#Dias 344 al 345
ESP21 [344:345,13] <- 77827910
ESP21 [344:345,14] <- 0
ESP21 [344:345,15] <- 163.65
ESP21 [344:345,16] <- 38488342
ESP21 [344:345,17] <- 80.93
#Dia 346
ESP21 [346,14] <- (80647211-77827910)  
#Dias 351 al 352
ESP21 [351:352,13] <- 82518671
ESP21 [351:352,14] <- 0
ESP21 [351:352,15] <- 173.51
ESP21 [351:352,16] <- 38607797
ESP21 [351:352,17] <- 81.18
#Dia 353
ESP21 [353,14] <- (83502561-82518671)
#Dias 354 al 356
ESP21 [354:356,13] <- 83502561
ESP21 [354:356,14] <- 0
ESP21 [354:356,15] <- 175.58
#Dias 357 al 359
ESP21 [357:359,13] <- 83502561
ESP21 [357:359,14] <- 0
ESP21 [357:359,15] <- 175.58
ESP21 [357:359,16] <- 39405157
ESP21 [357:359,17] <- 82.86
#Dias 360 al 362
ESP21 [360:362,13] <- 83502561
ESP21 [360:362,14] <- 0
ESP21 [360:362,15] <-175.58
#Dias 363 al 365
ESP21 [363:365,13] <- 83529796
ESP21 [363:365,14] <- 0
ESP21 [363:365,15] <- 175.64
ESP21 [363:365,16] <- 39664717
ESP21 [363:365,17] <- 83.40

#Verificamos que no halla ningun dato Na
sum(is.na(ESP21))

#verificamos los datos NA´s que contiene el año 2022
sum(is.na(ESP22))

#Comenzamos a rellenar los datos NA´s pertenencientes a las variables relacionadas con las vacunaciones
#Dia 1
ESP22 [01,13] <- 83529796
ESP22 [01,14] <- 0
ESP22 [01,15] <- 175.64
ESP22 [01,16] <- 39664717
ESP22 [01,17] <- 83.40
#Dia 2
ESP22 [02,14] <- (84250146-83529796)
#Dia 5
ESP22 [05,13] <- 84790916
ESP22 [05,14] <- 0
ESP22 [05,15] <- 178.29
ESP22 [05,16] <- 39931922
ESP22 [05,17] <- 83.96
#Dia 6
ESP22 [06,14] <- (84998186-84790916)
#Dias 7 al 8
ESP22 [07:08,13] <- 84998186
ESP22 [07:08,14] <- 0
ESP22 [07:08,15] <- 178.72
ESP22 [07:08,16] <- 39965876
ESP22 [07:08,17] <- 84.03
#Dia 9
ESP22 [09,14] <- (85480089-84998186)
#Dias 14 al 15
ESP22 [14:15,13] <- 86659108
ESP22 [14:15,14] <- 0
ESP22 [14:15,15] <- 182.22
ESP22 [14:15,16] <- 40317861
ESP22 [14:15,17] <- 84.78
#Dia 16
ESP22 [16,14] <- (87153288-86659108)
#Dias 21 al 22 
ESP22 [21:22,13] <- 88348566
ESP22 [21:22,14] <- 0
ESP22 [21:22,15] <- 185.77
ESP22 [21:22,16] <- 40884275
ESP22 [21:22,17] <- 85.68
#Dia 23
ESP22 [23,14] <- (88759314-88348566)
#Dias 28 al 29
ESP22 [28:29,13] <- 89717860
ESP22 [28:29,14] <- 0
ESP22 [28:29,15] <- 188.65
ESP22 [28:29,16] <- 40884275
ESP22 [28:29,17] <- 85.97
#Dia 30
ESP22 [30,14] <- (90135346-89717860)
#Dia 35 al 36
ESP22 [35:36,13] <- 90715372
ESP22 [35:36,14] <- 0
ESP22 [35:36,15] <- 190.74
ESP22 [35:36,16] <- 40942994
ESP22 [35:36,17] <- 86.09
#Dia 37
ESP22 [37,14] <- (90943374-90715372)
#Dias 42 al 43
ESP22 [42:43,13] <- 91411886
ESP22 [42:43,14] <- 0
ESP22 [42:43,15] <- 192.21
ESP22 [42:43,16] <- 40986943
ESP22 [42:43,17] <- 86.18
#Dia 44
ESP22 [44,14] <- (91571654-91411886)
#Dia 48
ESP22 [48,16] <- 41024149
ESP22 [48,17] <- 86.26
#Dia 49 al 50
ESP22 [49:50,13] <- 92121948
ESP22 [49:50,14] <- 0
ESP22 [49:50,15] <- 193.70
ESP22 [49:50,16] <- 41024149
ESP22 [49:50,17] <- 86.26
#Dia 51
ESP22 [51,14] <- (92210830-92121948)
ESP22 [51,16] <- 41024149
ESP22 [51,17] <- 86.26
#Dias 56 al 57
ESP22 [56:57,13] <- 92491318
ESP22 [56:57,14] <- 0
ESP22 [56:57,15] <- 194.48
ESP22 [56:57,16] <- 41059263
ESP22 [56:57,17] <- 86.33
#Dia 58
ESP22 [58,14] <- (92591078-92491318)
#Dias 63 al 68
ESP22 [63:68,13] <- 92774397
ESP22 [63:68,14] <- 0
ESP22 [63:68,15] <- 195.07
ESP22 [63:68,16] <- 41083791
ESP22 [63:68,17] <- 86.39
#Dia 69
ESP22 [69,14] <- (93346088-92774397)
#Dias 70 al 74
ESP22 [70:74,13] <- 93346088
ESP22 [70:74,14] <- 0
ESP22 [70:74,15] <- 196.28
ESP22 [70:74,16] <- 41089269
ESP22 [70:74,17] <- 86.40
#Dia 75
ESP22 [75,14] <- (93571317-93346088)
#Dia 76 al 81
ESP22 [76:81,13] <- 93571317
ESP22 [76:81,14] <- 0
ESP22 [76:81,15] <- 196.75
ESP22 [76:81,16] <- 41112436
ESP22 [76:81,17] <- 86.45
#Dia 82
ESP22 [82,14] <- (93807121-93571317)
#Dias 83 al 88
ESP22 [83:88,13] <- 93807121
ESP22 [83:88,14] <- 0
ESP22 [83:88,15] <- 197.25
ESP22 [83:88,16] <- 41134037
ESP22 [83:88,17] <- 86.49
#Dia 89
ESP22 [89,14] <- (94004482-93807121)
#Dias 90 al 95
ESP22 [90:95,13] <- 94004482
ESP22 [90:95,14] <- 0
ESP22 [90:95,15] <- 197.66
ESP22 [90:95,16] <- 41153657
ESP22 [90:95,17] <- 86.53
#Dia 96
ESP22 [96,14] <- (94130810-94004482)
#Dias 97 al 101
ESP22 [97:101,13] <- 94130810
ESP22 [97:101,14] <- 0
ESP22 [97:101,15] <- 197.93
ESP22 [97:101,16] <- 41167422
ESP22 [97:101,17] <- 86.56
#Dia 102
ESP22 [102,14] <- (94223806-94130890)
#Dias 103 al 110
ESP22 [103:110,13] <- 94223806
ESP22 [103:110,14] <- 0
ESP22 [103:110,15] <- 198.13
ESP22 [103:110,16] <- 41180591
ESP22 [103:110,17] <- 86.59
#Dia 111
ESP22 [111,14] <- (94295766-94223806)
#Dias 112 al 116
ESP22 [112:116,13] <- 94295766
ESP22 [112:116,14] <- 0
ESP22 [112:116,15] <- 198.27
ESP22 [112:116,16] <- 41188075
ESP22 [112:116,17] <- 86.60
#Dia 117
ESP22 [117,14] <- (94404832-94295766)
#Dias 118 al 123
ESP22 [118:123,13] <- 94404832
ESP22 [118:123,14] <- 0
ESP22 [118:123,15] <- 198.50
ESP22 [118:123,16] <- 41200263
ESP22 [118:123,17] <- 86.63
#Dia 124
ESP22 [124,14] <- (94485122-94404832)
#Dias 125 al 130
ESP22 [125:130,13] <- 94485122
ESP22 [125:130,14] <- 0
ESP22 [125:130,15] <- 198.67
ESP22 [125:130,16] <- 41209244
ESP22 [125:130,17] <- 86.65
#Dia 131
ESP22 [131,14] <- (94567014-94485122)
#Dias 132 al 137
ESP22 [132:137,13] <- 94567014
ESP22 [132:137,14] <- 0
ESP22 [132:137,15] <- 198.84
ESP22 [132:137,16] <- 41216883
ESP22 [132:137,17] <- 86.67
#Dia 138
ESP22 [138,14] <- (94653236-94567014)
#Dias 139 al 144
ESP22 [139:144,13] <- 94653236
ESP22 [139:144,14] <- 0
ESP22 [139:144,15] <- 199.02
ESP22 [139:144,16] <- 4122450
ESP22 [139:144,17] <- 86.68
#Dia 145
ESP22 [145,14] <- (94734966-94653236)
#Dias 146 151
ESP22 [146:151,13] <- 94734966
ESP22 [146:151,14] <- 0
ESP22 [146:151,15] <- 199.20
ESP22 [146:151,16] <- 41231916
ESP22 [146:151,17] <- 86.70
#Dia 152
ESP22 [152,14] <- (94815730-94734966)
#Dias 153 al 158
ESP22 [153:158,13] <- 94815730
ESP22 [153:158,14] <- 0
ESP22 [153:158,15] <- 199.37
ESP22 [153:158,16] <- 41238389
ESP22 [153:158,17] <- 86.71
#Dia 159
ESP22 [159,14] <- (94901362-94815730)
#Dias 160 al 165
ESP22 [160:165,13] <- 94901362
ESP22 [160:165,14] <- 0
ESP22 [160:165,15] <- 199.55
ESP22 [160:165,16] <- 41244442
ESP22 [160:165,17] <- 86.72
#Dia 166
ESP22 [166,14] <- (94987684-94901362)
#Dias 167 al 172
ESP22 [167:172,13] <- 94987684
ESP22 [167:172,14] <- 0
ESP22 [167:172,15] <- 199.73
ESP22 [167:172,16] <- 41249657
ESP22 [167:172,17] <- 86.73
#Dia 173
ESP22 [173,14] <- (95071546-94987684)
#Dias 174 al 179
ESP22 [174:179,13] <- 95071546
ESP22 [174:179,14] <- 0
ESP22 [174:179,15] <- 199.90
ESP22 [174:179,16] <- 41254310
ESP22 [174:179,17] <- 86.74
#Dia 180
ESP22 [180,14] <- (95153556-95071546)
#Dias 181 al 186
ESP22 [181:186,13] <- 95153556
ESP22 [181:186,14] <- 0
ESP22 [181:186,15] <- 200.08
ESP22 [181:186,16] <- 41264287
ESP22 [181:186,17] <- 86.75
#Dia 187
ESP22 [187,14] <- (95245599-95153556)
#Dias 188 al 193
ESP22 [188:193,13] <- 95245599
ESP22 [188:193,14] <- 0
ESP22 [188:193,15] <- 200.27
ESP22 [188:193,16] <- 41264287
ESP22 [188:193,17] <- 87.77
#Dia 194
ESP22 [194,14] <- (95340742-95245599)
#Dias 195 al 200
ESP22 [195:200,13] <- 95340742
ESP22 [195:200,14] <- 0
ESP22 [195:200,15] <- 200.47
ESP22 [195:200,16] <- 41269517
ESP22 [195:200,17] <- 86.78
#Dia 201
ESP22 [201,14] <- (95425998-95340742)
#Dias 202 al 207
ESP22 [202:207,13] <- 95425998
ESP22 [202:207,14] <- 0
ESP22 [202:207,15] <- 200.65
ESP22 [202:207,16] <- 41274079
ESP22 [202:207,17] <- 86.79
#Dia 208
ESP22 [208,14] <- (95491074-95425998)
#Dias 209 al 214
ESP22 [209:214,13] <- 95491074
ESP22 [209:214,14] <- 0
ESP22 [209:214,15] <- 200.79
ESP22 [209:214,16] <- 41277964
ESP22 [209:214,17] <- 86.79
#Dia 215
ESP22 [215,14] <- (95548924-95491074)
#Dias 216 al 221
ESP22 [216:221,13] <- 95548924
ESP22 [216:221,14] <- 0
ESP22 [216:221,15] <- 200.91
ESP22 [216:221,16] <- 41281380
ESP22 [216:221,17] <- 86.80
#Dia 222
ESP22 [222,14] <- (95584445-95548924)
#Dias 223 al 228
ESP22 [223:228,13] <- 95584445
ESP22 [223:228,14] <- 0
ESP22 [223:228,15] <- 200.98
ESP22 [223:228,16] <- 41284188
ESP22 [223:228,17] <- 86.81
#Dia 229
ESP22 [229,14] <- (95612718-95584445)
#Dias 230 al 235
ESP22 [230:235,13] <- 95612718
ESP22 [230:235,14] <- 0
ESP22 [230:235,15] <- 201.04
ESP22 [230:235,16] <- 41286450
ESP22 [230:235,17] <- 86.81
#Dia 236
ESP22 [236,14] <- (95643377-95612718)
#Dias 237 al 242
ESP22 [237:242,13] <- 95643377
ESP22 [237:242,14] <- 0
ESP22 [237:242,15] <- 201.11
ESP22 [237:242,16] <- 41289087
ESP22 [237:242,17] <- 86.82
#Dia 243
ESP22 [243,14] <- (95676798-95643377)
#Dias 244 al 249
ESP22 [244:249,13] <- 95676798
ESP22 [244:249,14] <- 0
ESP22 [244:249,15] <- 201.18
ESP22 [244:249,16] <- 41292888
ESP22 [244:249,17] <- 86.83
#Dia 250
ESP22 [250,14] <- (95704322-95676798)
#Dias 251 al 256
ESP22 [251:256,13] <- 95704322
ESP22 [251:256,14] <- 0
ESP22 [251:256,15] <- 201.23
ESP22 [251:256,16] <- 41296128
ESP22 [251:256,17] <- 86.83
#Dia 257
ESP22 [257,14] <- (95739561-95704322)
#Dias 258 al 263
ESP22 [258:263,13] <- 95739561
ESP22 [258:263,14] <- 0
ESP22 [258:263,15] <- 201.36
ESP22 [258:263,16] <- 41304755
ESP22 [258:263,17] <- 86.85
#Dia 264
ESP22 [264,14] <- (95763061-95739561)
#Dias 265 al 270
ESP22 [265:270,13] <- 95763061
ESP22 [265:270,14] <- 0
ESP22 [265:270,15] <- 201.40
ESP22 [265:270,16] <- 41307428
ESP22 [265:270,17] <- 86.86
#Dia 271
ESP22 [271,14] <- (95783126-95763061)
#Dias 272 al 277
ESP22 [272:277,13] <- 95783126
ESP22 [272:277,14] <- 0
ESP22 [272:277,15] <- 201.40
ESP22 [272:277,16] <- 41307428
ESP22 [272:277,17] <- 86.86
#Dia 278
ESP22 [278,14] <- (95796340-95783126)
#Dias 279 al 285
ESP22 [279:285,13] <- 95796340
ESP22 [279:285,14] <- 0
ESP22 [279:285,15] <- 201.43
ESP22 [279:285,16] <- 41309336
ESP22 [279:285,17] <- 86.86
#Dia 286
ESP22 [286,14] <- (95804542-95796340)
#Dias 287 al 291
ESP22 [287:291,13] <- 95804542
ESP22 [287:291,14] <- 0
ESP22 [287:291,15] <- 201.46
ESP22 [287:291,16] <- 41310693
ESP22 [287:291,17] <- 86.86
#Dia 292
ESP22 [292,14] <- (97667329-95804542)
#Dias 293 al 299
ESP22 [293:299,13] <- 97667329
ESP22 [293:299,14] <- 0
ESP22 [293:299,15] <- 205.36
ESP22 [293:299,16] <- 41312329
ESP22 [293:299,17] <- 86.87
#Dia 300
ESP22 [300,14] <- (98948747-97667329)
#Dias 301 al 305
ESP22 [301:305,13] <- 98948747
ESP22 [301:305,14] <- 0
ESP22 [301:305,15] <- 208.06
ESP22 [301:305,16] <- 41314306
ESP22 [301:305,17] <- 86.86
#Dia 306
ESP22 [306,14] <- (99955993-98948747)
#Dias 307 al 319
ESP22 [307:319,13] <- 99955993
ESP22 [307:319,14] <- 0
ESP22 [307:319,15] <- 210.17
ESP22 [307:319,16] <- 41316070
ESP22 [307:319,17] <- 86.87
#Dia 320
ESP22 [320,14] <- (102011601-99955993)
#Dias 321 al 329
ESP22 [321:329,13] <- 102011601
ESP22 [321:329,14] <- 0
ESP22 [321:329,15] <- 214.50
ESP22 [321:329,16] <- 41319243
ESP22 [321:329,17] <- 86.88
#Dia 330
ESP22 [330,14] <- (102574756-102011601)
#Dias 330 al 335
ESP22 [330:335,13] <- 102574756
ESP22 [330:335,14] <- 0
ESP22 [330:335,15] <- 215.68
ESP22 [330:335,16] <- 41324255
ESP22 [330:335,17] <- 86.89
#Dia 335
ESP22 [335,14] <- (103005173-102574756)
#Dias 336 al 339
ESP22 [336:339,13] <- 103005173
ESP22 [336:339,14] <- 0
ESP22 [336:339,15] <- 216.59
ESP22 [336:339,16] <- 41326081
ESP22 [336:339,17] <- 86.90
#Dia 340
ESP22 [340,14] <- (103212818-103005173)
#Dias 341 al 347
ESP22 [341:347,13] <- 103212818
ESP22 [341:347,14] <- 0
ESP22 [341:347,15] <- 217.02
ESP22 [341:347,16] <- 41326755
ESP22 [341:347,17] <- 86.90
#Dia 348
ESP22 [348,14] <- (103756296-103212818)
#Dias 349 al 357
ESP22 [349:357,13] <- 103756296
ESP22 [349:357,14] <- 0
ESP22 [349:357,15] <- 218.17
ESP22 [349:357,16] <- 41330488
ESP22 [349:357,17] <- 86.90
#Dia 358
ESP22 [358,14] <- (104045018-103756296)
#Dias 358 al 361
ESP22 [358:361,13] <- 104045018
ESP22 [358:361,14] <- 0
ESP22 [358:361,15] <- 217.02
ESP22 [358:361,16] <- 41332279
ESP22 [358:361,17] <- 86.91
#Dia 362
ESP22 [362,14] <- (104186558-104045018)
#Dias 363 al 365
ESP22 [363:365,13] <- 104186558
ESP22 [363:365,14] <- 0
ESP22 [363:365,15] <- 219.07
ESP22 [363:365,16] <- 41333243
ESP22 [363:365,17] <- 86.90

#Verificamos que no quede ningun Valor NA´s restante
sum(is.na(ESP22))

#2023
#Completamos los Valores NA´s
#Dias 01 al 02
ESP23 [01:02,13] <- 104186558
ESP23 [01:02,14] <- 0
ESP23 [01:02,15] <- 219.07
ESP23 [01:02,16] <- 41333243
ESP23 [01:02,17] <- 86.90
#Dia 03
ESP23 [03,14] <- (104301730-104186558)
#Dias 04 al 10
ESP23 [04:10,13] <- 104301730
ESP23 [04:10,14] <- 0
ESP23 [04:10,15] <- 219.31
ESP23 [04:10,16] <- 41334113
ESP23 [04:10,17] <- 86.91
#Dia 11
ESP23 [11,14] <- (104536856-104301730)
#Dias 12 al 17
ESP23 [12:17,13] <- 104536856
ESP23 [12:17,14] <- 0
ESP23 [12:17,15] <- 219.81
ESP23 [12:17,16] <- 41335276
ESP23 [12:17,17] <- 86.91
#Dia 18
ESP23 [18,14] <- (104863700-104536856)
#Dias 19 al 24
ESP23 [19:24,13] <- 104863700
ESP23 [19:24,14] <- 0
ESP23 [19:24,15] <- 220.49
ESP23 [19:24,16] <- 41337463
ESP23 [19:24,17] <- 86.92
#Dia 25
ESP23 [25,14] <- (105094800-104863700)
#Dias 26 al 31
ESP23 [26:31,13] <- 105094800
ESP23 [26:31,14] <- 0
ESP23 [26:31,15] <- 219.98
ESP23 [26:31,16] <- 41338847
ESP23 [26:31,17] <- 86.92
#Dia 32
ESP23 [32,14] <- (105253468-105094800)
#Dias 33 al 38
ESP23 [33:38,13] <- 105253468
ESP23 [33:38,14] <- 0
ESP23 [33:38,15] <- 221.31
ESP23 [33:38,16] <- 41340052
ESP23 [33:38,17] <- 86.92
#Dia 39
ESP23 [39,14] <- (105373684-105253468)
#Dias 40 al 47
ESP23 [40:47,13] <- 105373684
ESP23 [40:47,14] <- 0
ESP23 [40:47,15] <- 221.57
ESP23 [40:47,16] <- 41341288
ESP23 [40:47,17] <- 86.92
#Dia 48
ESP23 [48,14] <- (105373684-105094800)
#Dias 48 al 52
ESP23 [48:52,13] <- 105459628
ESP23 [48:52,14] <- 0
ESP23 [48:52,15] <- 221.75
ESP23 [48:52,16] <- 41342436
ESP23 [48:52,17] <- 86.93
#Dia 53
ESP23 [53,14] <- (105522606-105459628)
#Dias 54 al 82
ESP23 [54:82,13] <- 105522606
ESP23 [54:82,14] <- 0
ESP23 [54:82,15] <- 221.88
ESP23 [54:82,16] <- 41343396
ESP23 [54:82,17] <- 86.93
#Dia 84
ESP23 [84,14] <- (105733320-105522606)
#Dias 84 al 115
ESP23 [85:115,13] <- 105733320
ESP23 [85:115,14] <- 0
ESP23 [85:115,15] <- 222.32
ESP23 [85:115,16] <- 41347499
ESP23 [85:115,17] <- 86.94
#Dia 116
ESP23 [116,14] <- (105799888-105733320)
#Dias 117 al 143
ESP23 [117:143,13] <- 105799888
ESP23 [117:143,14] <- 0
ESP23 [117:143,15] <- 222.45
ESP23 [117:143,16] <- 41349325
ESP23 [117:143,17] <- 86.94
#Dia 144
ESP23 [144,14] <- (105840320-105799888)
#Dias 145 al 172
ESP23 [145:172,13] <- 105840320
ESP23 [145:172,14] <- 0
ESP23 [145:172,15] <- 222.55
ESP23 [145:172,16] <- 41351234
ESP23 [145:172,17] <- 86.95
#Datos NA´s de los Dias 169 al 172 del año 2023, de las variables nuevos caso, nuevos casos por millon
#Nuevas muertes y nuevas muertes por millon
#Dias 169 al 172
ESP23 [169:172,06] <- 0
ESP23 [169:172,07] <- 0.000
ESP23 [169:172,10] <- 0
ESP23 [169:172,11] <- 0.000

#En este año podemos notar el descenso en la variables relacionadas con las vacunaciones
#esto ocurre ya que en el 2023 acaba la pandemia
#Verificamos que no quede ningun Valor NA´s restante
sum(is.na(ESP23))     

#Creacion de la variable new_vaccinations_per_hundred para cada pais

ESP20 <- mutate(ESP20, new_vaccinations_per_hundred=new_vaccinations/population*100)
ESP21 <- mutate(ESP21, new_vaccinations_per_hundred=new_vaccinations/population*100)
ESP22 <- mutate(ESP22, new_vaccinations_per_hundred=new_vaccinations/population*100)
ESP23 <- mutate(ESP23, new_vaccinations_per_hundred=new_vaccinations/population*100)

#Estadisticos por año 
#España Casos
Casos_ESP20 <- ESP20 %>% summarise(Promedio_total=mean(total_cases),
                                     D.Est_total=sd(total_cases),
                                     CV_total=sd(total_cases)/mean(total_cases)*100,
                                     Min_total=min(total_cases),
                                     Max_total=max(total_cases),
                                     Promedio_new=mean(new_cases),
                                     D.Est_new=sd(new_cases),
                                     CV_new=sd(new_cases)/mean(new_cases)*100,
                                     Min_new=min(new_cases),
                                     Max_new=max(new_cases))

Tasa_de_casos_ESP20 <- ESP20 %>% summarise(Promedio_total=mean(total_cases_per_million),
                                             D.Est_total=sd(total_cases_per_million),
                                             CV_total=
                                               sd(total_cases_per_million)/mean(total_cases_per_million)*100,
                                             Min_total=min(total_cases_per_million),
                                             Max_total=max(total_cases_per_million),
                                             Promedio_new=mean(new_cases_per_million),
                                             D.Est_new=sd(new_cases_per_million),
                                             CV_new=
                                               sd(new_cases_per_million)/mean(new_cases_per_million)*100,
                                             Min_new=min(new_cases_per_million),
                                             Max_new=max(new_cases_per_million))


Casos_ESP21 <- ESP21 %>% summarise(Promedio_total=mean(total_cases),
                                     D.Est_total=sd(total_cases),
                                     CV_total=sd(total_cases)/mean(total_cases)*100,
                                     Min_total=min(total_cases),
                                     Max_total=max(total_cases),
                                     Promedio_new=mean(new_cases),
                                     D.Est_new=sd(new_cases),
                                     CV_new=sd(new_cases)/mean(new_cases)*100,
                                     Min_new=min(new_cases),
                                     Max_new=max(new_cases))

Tasa_de_casos_ESP21 <- ESP21 %>% summarise(Promedio_total=mean(total_cases_per_million),
                                             D.Est_total=sd(total_cases_per_million),
                                             CV_total=
                                               sd(total_cases_per_million)/mean(total_cases_per_million)*100,
                                             Min_total=min(total_cases_per_million),
                                             Max_total=max(total_cases_per_million),
                                             Promedio_new=mean(new_cases_per_million),
                                             D.Est_new=sd(new_cases_per_million),
                                             CV_new=
                                               sd(new_cases_per_million)/mean(new_cases_per_million)*100,
                                             Min_new=min(new_cases_per_million),
                                             Max_new=max(new_cases_per_million))

Casos_ESP22 <- ESP22 %>% summarise(Promedio_total=mean(total_cases),
                                     D.Est_total=sd(total_cases),
                                     CV_total=sd(total_cases)/mean(total_cases)*100,
                                     Min_total=min(total_cases),
                                     Max_total=max(total_cases),
                                     Promedio_new=mean(new_cases),
                                     D.Est_new=sd(new_cases),
                                     CV_new=sd(new_cases)/mean(new_cases)*100,
                                     Min_new=min(new_cases),
                                     Max_new=max(new_cases))

Tasa_de_casos_ESP22 <- ESP22 %>% summarise(Promedio_total=mean(total_cases_per_million),
                                             D.Est_total=sd(total_cases_per_million),
                                             CV_total=
                                               sd(total_cases_per_million)/mean(total_cases_per_million)*100,
                                             Min_total=min(total_cases_per_million),
                                             Max_total=max(total_cases_per_million),
                                             Promedio_new=mean(new_cases_per_million),
                                             D.Est_new=sd(new_cases_per_million),
                                             CV_new=
                                               sd(new_cases_per_million)/mean(new_cases_per_million)*100,
                                             Min_new=min(new_cases_per_million),
                                             Max_new=max(new_cases_per_million))

Casos_ESP23 <- ESP23 %>% summarise(Promedio_total=mean(total_cases),
                                     D.Est_total=sd(total_cases),
                                     CV_total=sd(total_cases)/mean(total_cases)*100,
                                     Min_total=min(total_cases),
                                     Max_total=max(total_cases),
                                     Promedio_new=mean(new_cases),
                                     D.Est_new=sd(new_cases),
                                     CV_new=sd(new_cases)/mean(new_cases)*100,
                                     Min_new=min(new_cases),
                                     Max_new=max(new_cases))

Tasa_de_casos_ESP23 <- ESP23 %>% summarise(Promedio_total=mean(total_cases_per_million),
                                             D.Est_total=sd(total_cases_per_million),
                                             CV_total=
                                               sd(total_cases_per_million)/mean(total_cases_per_million)*100,
                                             Min_total=min(total_cases_per_million),
                                             Max_total=max(total_cases_per_million),
                                             Promedio_new=mean(new_cases_per_million),
                                             D.Est_new=sd(new_cases_per_million),
                                             CV_new=
                                               sd(new_cases_per_million)/mean(new_cases_per_million)*100,
                                             Min_new=min(new_cases_per_million),
                                             Max_new=max(new_cases_per_million))
#ESP Vacunaciones 
Vacunas_ESP20 <- ESP20 %>% summarise(Promedio_total=mean(total_vaccinations),
                                       D.Est_total=sd(total_vaccinations),
                                       CV_total=sd(total_vaccinations)/mean(total_vaccinations)*100,
                                       Min_total=min(total_vaccinations),
                                       Max_total=max(total_vaccinations),
                                       Promedio_new=mean(new_vaccinations),
                                       D.Est_new=sd(new_vaccinations),
                                       CV_new=sd(new_vaccinations)/mean(new_vaccinations)*100,
                                       Min_new=min(new_vaccinations),
                                       Max_new=max(new_vaccinations))

Tasa_de_vacunas_ESP20 <- ESP20 %>% summarise(Promedio_total=mean(total_vaccinations_per_hundred),
                                               D.Est_total=sd(total_vaccinations_per_hundred),
                                               CV_total=
                                                 sd(total_vaccinations_per_hundred)/
                                                 mean(total_vaccinations_per_hundred)*100,
                                               Min_total=min(total_vaccinations_per_hundred),
                                               Max_total=max(total_vaccinations_per_hundred),
                                               Promedio_new=mean(new_vaccinations_per_hundred),
                                               D.Est_new=sd(new_vaccinations_per_hundred),
                                               CV_new=
                                                 sd(new_vaccinations_per_hundred)/
                                                 mean(new_vaccinations_per_hundred)*100,
                                               Min_new=min(new_vaccinations_per_hundred),
                                               Max_new=max(new_vaccinations_per_hundred))

Vacunas_ESP21 <- ESP21 %>% summarise(Promedio_total=mean(total_vaccinations),
                                       D.Est_total=sd(total_vaccinations),
                                       CV_total=sd(total_vaccinations)/mean(total_vaccinations)*100,
                                       Min_total=min(total_vaccinations),
                                       Max_total=max(total_vaccinations),
                                       Promedio_new=mean(new_vaccinations),
                                       D.Est_new=sd(new_vaccinations),
                                       CV_new=sd(new_vaccinations)/mean(new_vaccinations)*100,
                                       Min_new=min(new_vaccinations),
                                       Max_new=max(new_vaccinations))

Tasa_de_vacunas_ESP21 <- ESP21 %>% summarise(Promedio_total=mean(total_vaccinations_per_hundred),
                                               D.Est_total=sd(total_vaccinations_per_hundred),
                                               CV_total=
                                                 sd(total_vaccinations_per_hundred)/
                                                 mean(total_vaccinations_per_hundred)*100,
                                               Min_total=min(total_vaccinations_per_hundred),
                                               Max_total=max(total_vaccinations_per_hundred),
                                               Promedio_new=mean(new_vaccinations_per_hundred),
                                               D.Est_new=sd(new_vaccinations_per_hundred),
                                               CV_new=
                                                 sd(new_vaccinations_per_hundred)/
                                                 mean(new_vaccinations_per_hundred)*100,
                                               Min_new=min(new_vaccinations_per_hundred),
                                               Max_new=max(new_vaccinations_per_hundred))

Vacunas_ESP22 <- ESP22 %>% summarise(Promedio_total=mean(total_vaccinations),
                                       D.Est_total=sd(total_vaccinations),
                                       CV_total=sd(total_vaccinations)/mean(total_vaccinations)*100,
                                       Min_total=min(total_vaccinations),
                                       Max_total=max(total_vaccinations),
                                       Promedio_new=mean(new_vaccinations),
                                       D.Est_new=sd(new_vaccinations),
                                       CV_new=sd(new_vaccinations)/mean(new_vaccinations)*100,
                                       Min_new=min(new_vaccinations),
                                       Max_new=max(new_vaccinations))

Tasa_de_vacunas_ESP22 <- ESP22 %>% summarise(Promedio_total=mean(total_vaccinations_per_hundred),
                                               D.Est_total=sd(total_vaccinations_per_hundred),
                                               CV_total=
                                                 sd(total_vaccinations_per_hundred)/
                                                 mean(total_vaccinations_per_hundred)*100,
                                               Min_total=min(total_vaccinations_per_hundred),
                                               Max_total=max(total_vaccinations_per_hundred),
                                               Promedio_new=mean(new_vaccinations_per_hundred),
                                               D.Est_new=sd(new_vaccinations_per_hundred),
                                               CV_new=
                                                 sd(new_vaccinations_per_hundred)/
                                                 mean(new_vaccinations_per_hundred)*100,
                                               Min_new=min(new_vaccinations_per_hundred),
                                               Max_new=max(new_vaccinations_per_hundred))

Vacunas_ESP23 <- ESP23 %>% summarise(Promedio_total=mean(total_vaccinations),
                                       D.Est_total=sd(total_vaccinations),
                                       CV_total=sd(total_vaccinations)/mean(total_vaccinations)*100,
                                       Min_total=min(total_vaccinations),
                                       Max_total=max(total_vaccinations),
                                       Promedio_new=mean(new_vaccinations),
                                       D.Est_new=sd(new_vaccinations),
                                       CV_new=sd(new_vaccinations)/mean(new_vaccinations)*100,
                                       Min_new=min(new_vaccinations),
                                       Max_new=max(new_vaccinations))

Tasa_de_vacunas_ESP23 <- ESP23 %>% summarise(Promedio_total=mean(total_vaccinations_per_hundred),
                                               D.Est_total=sd(total_vaccinations_per_hundred),
                                               CV_total=
                                                 sd(total_vaccinations_per_hundred)/
                                                 mean(total_vaccinations_per_hundred)*100,
                                               Min_total=min(total_vaccinations_per_hundred),
                                               Max_total=max(total_vaccinations_per_hundred),
                                               Promedio_new=mean(new_vaccinations_per_hundred),
                                               D.Est_new=sd(new_vaccinations_per_hundred),
                                               CV_new=
                                                 sd(new_vaccinations_per_hundred)/
                                                 mean(new_vaccinations_per_hundred)*100,
                                               Min_new=min(new_vaccinations_per_hundred),
                                               Max_new=max(new_vaccinations_per_hundred))
#ESP, Decesos

Decesos_ESP20 <- ESP20 %>% summarise(Promedio_total=mean(total_deaths),
                                       D.Est_total=sd(total_deaths),
                                       CV_total=sd(total_deaths)/mean(total_deaths)*100,
                                       Min_total=min(total_deaths),
                                       Max_total=max(total_deaths),
                                       Promedio_new=mean(new_deaths),
                                       D.Est_new=sd(new_deaths),
                                       CV_new=sd(new_deaths)/mean(new_deaths)*100,
                                       Min_new=min(new_deaths),
                                       Max_new=max(new_deaths))

Tasa_de_decesos_ESP20 <- ESP20 %>% summarise(Promedio_total=mean(total_deaths_per_million),
                                               D.Est_total=sd(total_deaths_per_million),
                                               CV_total=
                                                 sd(total_deaths_per_million)/mean(total_deaths_per_million)*100,
                                               Min_total=min(total_deaths_per_million),
                                               Max_total=max(total_deaths_per_million),
                                               Promedio_new=mean(new_deaths_per_million),
                                               D.Est_new=sd(new_deaths_per_million),
                                               CV_new=
                                                 sd(new_deaths_per_million)/mean(new_deaths_per_million)*100,
                                               Min_new=min(new_deaths_per_million),
                                               Max_new=max(new_deaths_per_million))

Decesos_ESP21 <- ESP21 %>% summarise(Promedio_total=mean(total_deaths),
                                       D.Est_total=sd(total_deaths),
                                       CV_total=sd(total_deaths)/mean(total_deaths)*100,
                                       Min_total=min(total_deaths),
                                       Max_total=max(total_deaths),
                                       Promedio_new=mean(new_deaths),
                                       D.Est_new=sd(new_deaths),
                                       CV_new=sd(new_deaths)/mean(new_deaths)*100,
                                       Min_new=min(new_deaths),
                                       Max_new=max(new_deaths))

Tasa_de_decesos_ESP21 <- ESP21 %>% summarise(Promedio_total=mean(total_deaths_per_million),
                                               D.Est_total=sd(total_deaths_per_million),
                                               CV_total=
                                                 sd(total_deaths_per_million)/mean(total_deaths_per_million)*100,
                                               Min_total=min(total_deaths_per_million),
                                               Max_total=max(total_deaths_per_million),
                                               Promedio_new=mean(new_deaths_per_million),
                                               D.Est_new=sd(new_deaths_per_million),
                                               CV_new=
                                                 sd(new_deaths_per_million)/mean(new_deaths_per_million)*100,
                                               Min_new=min(new_deaths_per_million),
                                               Max_new=max(new_deaths_per_million))

Decesos_ESP22 <- ESP22 %>% summarise(Promedio_total=mean(total_deaths),
                                       D.Est_total=sd(total_deaths),
                                       CV_total=sd(total_deaths)/mean(total_deaths)*100,
                                       Min_total=min(total_deaths),
                                       Max_total=max(total_deaths),
                                       Promedio_new=mean(new_deaths),
                                       D.Est_new=sd(new_deaths),
                                       CV_new=sd(new_deaths)/mean(new_deaths)*100,
                                       Min_new=min(new_deaths),
                                       Max_new=max(new_deaths))

Tasa_de_decesos_ESP22 <- ESP22 %>% summarise(Promedio_total=mean(total_deaths_per_million),
                                               D.Est_total=sd(total_deaths_per_million),
                                               CV_total=
                                                 sd(total_deaths_per_million)/mean(total_deaths_per_million)*100,
                                               Min_total=min(total_deaths_per_million),
                                               Max_total=max(total_deaths_per_million),
                                               Promedio_new=mean(new_deaths_per_million),
                                               D.Est_new=sd(new_deaths_per_million),
                                               CV_new=
                                                 sd(new_deaths_per_million)/mean(new_deaths_per_million)*100,
                                               Min_new=min(new_deaths_per_million),
                                               Max_new=max(new_deaths_per_million))

Decesos_ESP23 <- ESP23 %>% summarise(Promedio_total=mean(total_deaths),
                                       D.Est_total=sd(total_deaths),
                                       CV_total=sd(total_deaths)/mean(total_deaths)*100,
                                       Min_total=min(total_deaths),
                                       Max_total=max(total_deaths),
                                       Promedio_new=mean(new_deaths),
                                       D.Est_new=sd(new_deaths),
                                       CV_new=sd(new_deaths)/mean(new_deaths)*100,
                                       Min_new=min(new_deaths),
                                       Max_new=max(new_deaths))

Tasa_de_decesos_ESP23 <- ESP23 %>% summarise(Promedio_total=mean(total_deaths_per_million),
                                               D.Est_total=sd(total_deaths_per_million),
                                               CV_total=
                                                 sd(total_deaths_per_million)/mean(total_deaths_per_million)*100,
                                               Min_total=min(total_deaths_per_million),
                                               Max_total=max(total_deaths_per_million),
                                               Promedio_new=mean(new_deaths_per_million),
                                               D.Est_new=sd(new_deaths_per_million),
                                               CV_new=
                                                 sd(new_deaths_per_million)/mean(new_deaths_per_million)*100,
                                               Min_new=min(new_deaths_per_million),
                                               Max_new=max(new_deaths_per_million))

#LLamado de Librerias
library(tidyverse)
library(ggplot2)
library(readxl)
library(ggpmisc)

#Graficos del analisis

#Graficos España
#2020
#Total de casos y Nuevos casos de españa en el 2020

ggplot(ESP20, aes(x = date, y = total_cases)) +
  geom_area(fill = "#6551CC", alpha = 0.7) +
  geom_line(fill = "#260F99") +
  scale_x_date(date_breaks = "3 months") +
  labs(x = "Meses", y = "Casos totales") +
  ggtitle("Casos totales del 2020", subtitle = "Total de casos por día") + 
  theme(panel.background = element_rect(fill = "#EDE7F6")) +
  theme(panel.border = element_rect(fill = "transparent", 
                                    color = "#212121",           
                                    size = 1))    

ggplot(ESP20, aes(x = date, y = new_cases)) +
  geom_area(fill = "#6551CC", alpha = 0.7) +
  geom_line(fill = "#260F99") +
  scale_x_date(date_breaks = "3 months") +
  labs(x = "Meses", y = "Nuevos casos") +
  ggtitle("Nuevos casos del 2020", subtitle = "Nuevos casos por día") + 
  theme(panel.background = element_rect(fill = "#EDE7F6")) +
  theme(panel.border = element_rect(fill = "transparent", 
                                    color = "#212121",
                                    size = 1))

#Total de muertes y Nuevas muertes

ggplot(ESP20, aes(x = date, y = total_deaths)) +
  geom_area(fill = "#2E7D32", alpha = 0.7) +
  geom_line(fill = "#088158") +
  scale_x_date(date_breaks = "3 months") +
  labs(x = "Meses", y = "Decesos totales") +
  ggtitle("Decesos totales del 2020", subtitle = "Total de Decesos por día") + 
  theme(panel.background = element_rect(fill = "#E8F5E9")) +
  theme(panel.border = element_rect(fill = "transparent", 
                                    color = "#212121",           
                                    size = 1))

ggplot(ESP20, aes(x = date, y = new_deaths)) +
  geom_area(fill = "#2E7D32", alpha = 0.7) +
  geom_line(fill = "#088158") +
  scale_x_date(date_breaks = "3 months") +
  labs(x = "Meses", y = "Nuevas muertes") +
  ggtitle("Nuevas muertes del 2020", subtitle = "Nuevas muertes por día") + 
  theme(panel.background = element_rect(fill = "#E8F5E9")) +
  theme(panel.border = element_rect(fill = "transparent", 
                                    color = "#212121",           
                                    size = 1))  

#Vacunados
# El hecho de que no se registraran vacunas en España ya hasta el año 2021 finales de el mes
#de enero y comienzos de febrero, los graficos de los vacunados no muestran nada.


#2021
#Casos diarios 
ggplot(ESP21, aes(x = date, y = total_cases)) +
  geom_area(fill = "#6551CC", alpha = 0.7) +
  geom_line(fill = "#260F99") +
  scale_x_date(date_breaks = "3 months") +
  labs(x = "Meses", y = "Casos totales") +
  ggtitle("Casos totales del 2020", subtitle = "Total de casos por día") + 
  theme(panel.background = element_rect(fill = "#EDE7F6")) +
  theme(panel.border = element_rect(fill = "transparent", 
                                    color = "#212121",           
                                    size = 1))    

ggplot(ESP21, aes(x = date, y = new_cases)) +
  geom_area(fill = "#6551CC", alpha = 0.7) +
  geom_line(fill = "#260F99") +
  scale_x_date(date_breaks = "3 months") +
  labs(x = "Meses", y = "Nuevos casos") +
  ggtitle("Nuevos casos del 2020", subtitle = "Nuevos casos por día") + 
  theme(panel.background = element_rect(fill = "#EDE7F6")) +
  theme(panel.border = element_rect(fill = "transparent",
                                    color = "#212121",      
                                    size = 1)) +
  stat_peaks(geom = "point", span = 15, color = "#6551CC", size = 2)

#Decesos diarios

ggplot(ESP21, aes(x = date, y = total_deaths)) +
  geom_area(fill = "#2E7D32", alpha = 0.7) +
  geom_line(fill = "#088158") +
  scale_x_date(date_breaks = "3 months") +
  labs(x = "Meses", y = "Decesos totales") +
  ggtitle("Decesos totales del 2021", subtitle = "Total de Decesos por día") + 
  theme(panel.background = element_rect(fill = "#E8F5E9")) +
  theme(panel.border = element_rect(fill = "transparent", 
                                    color = "#212121",           
                                    size = 1))

ggplot(ESP21, aes(x = date, y = new_deaths)) +
  geom_area(fill = "#2E7D32", alpha = 0.7) +
  geom_line(fill = "#088158") +
  scale_x_date(date_breaks = "3 months") +
  labs(x = "Meses", y = "Nuevas muertes") +
  ggtitle("Nuevas muertes del 2021", subtitle = "Nuevas muertes por día") + 
  theme(panel.background = element_rect(fill = "#E8F5E9")) +
  theme(panel.border = element_rect(fill = "transparent", 
                                    color = "#212121",           
                                    size = 1))

#Vacunados diarios

ggplot(ESP21, aes(x = date, y = total_vaccinations)) +
  geom_area(fill = "#B26F2C", alpha = 0.7) +
  geom_line(fill = "#99540F") +
  scale_x_date(date_breaks = "3 months") +
  labs(x = "Meses", y = "Total de vacunas") +
  ggtitle("Total de vacunas del 2021", subtitle = "Total de Vacunas por día") + 
  theme(panel.background = element_rect(fill = "#F2DACE")) +
  theme(panel.border = element_rect(fill = "transparent", 
                                    color = "#212121",           
                                    size = 1))

ggplot(ESP21, aes(x = date, y = new_vaccinations)) +
  geom_area(fill = "#B26F2C", alpha = 0.7) +
  geom_line(fill = "#99540F") +
  scale_x_date(date_breaks = "3 months") +
  labs(x = "Meses", y = "Nuevas vacunas") +
  ggtitle("Nuevas vacunas del 2021", subtitle = "Nuevas vacunas por día") + 
  theme(panel.background = element_rect(fill = "#F2DACE")) +
  theme(panel.border = element_rect(fill = "transparent", 
                                    color = "#212121",           
                                    size = 1))

#2022

#Casos diarios 
ggplot(ESP22, aes(x = date, y = total_cases)) +
  geom_area(fill = "#6551CC", alpha = 0.7) +
  geom_line(fill = "#260F99") +
  scale_x_date(date_breaks = "3 months") +
  labs(x = "Meses", y = "Casos totales") +
  ggtitle("Casos totales del 2022", subtitle = "Total de casos por día") + 
  theme(panel.background = element_rect(fill = "#EDE7F6")) +
  theme(panel.border = element_rect(fill = "transparent", 
                                    color = "#212121",           
                                    size = 1))    

ggplot(ESP22, aes(x = date, y = new_cases)) +
  geom_area(fill = "#6551CC", alpha = 0.7) +
  geom_line(fill = "#260F99") +
  scale_x_date(date_breaks = "3 months") +
  labs(x = "Meses", y = "Nuevos casos") +
  ggtitle("Nuevos casos del 2022", subtitle = "Nuevos casos por día") + 
  theme(panel.background = element_rect(fill = "#EDE7F6")) +
  theme(panel.border = element_rect(fill = "transparent", 
                                    color = "#212121",
                                    size = 1))

#Decesos diarios

ggplot(ESP22, aes(x = date, y = total_deaths)) +
  geom_area(fill = "#2E7D32", alpha = 0.7) +
  geom_line(fill = "#088158") +
  scale_x_date(date_breaks = "3 months") +
  labs(x = "Meses", y = "Decesos totales") +
  ggtitle("Decesos totales del 2022", subtitle = "Total de Decesos por día") + 
  theme(panel.background = element_rect(fill = "#E8F5E9")) +
  theme(panel.border = element_rect(fill = "transparent", 
                                    color = "#212121",           
                                    size = 1))

ggplot(ESP22, aes(x = date, y = new_deaths)) +
  geom_area(fill = "#2E7D32", alpha = 0.7) +
  geom_line(fill = "#088158") +
  scale_x_date(date_breaks = "3 months") +
  labs(x = "Meses", y = "Nuevas muertes") +
  ggtitle("Nuevas muertes del 2022", subtitle = "Nuevas muertes por día") + 
  theme(panel.background = element_rect(fill = "#E8F5E9")) +
  theme(panel.border = element_rect(fill = "transparent", 
                                    color = "#212121",           
                                    size = 1))

#Vacunados diarios

ggplot(ESP22, aes(x = date, y = total_vaccinations)) +
  geom_area(fill = "#B26F2C", alpha = 0.7) +
  geom_line(fill = "#99540F") +
  scale_x_date(date_breaks = "3 months") +
  labs(x = "Meses", y = "Total de vacunas") +
  ggtitle("Total de vacunas del 2022", subtitle = "Total de Vacunas por día") + 
  theme(panel.background = element_rect(fill = "#F2DACE")) +
  theme(panel.border = element_rect(fill = "transparent", 
                                    color = "#212121",           
                                    size = 1))

ggplot(ESP22, aes(x = date, y = new_vaccinations)) +
  geom_area(fill = "#B26F2C", alpha = 0.7) +
  geom_line(fill = "#99540F") +
  scale_x_date(date_breaks = "3 months") +
  labs(x = "Meses", y = "Nuevas vacunas") +
  ggtitle("Nuevas vacunas del 2022", subtitle = "Nuevas vacunas por día") + 
  theme(panel.background = element_rect(fill = "#F2DACE")) +
  theme(panel.border = element_rect(fill = "transparent", 
                                    color = "#212121",           
                                    size = 1))

#2023

#Total de casos y nuevos casos
ggplot(ESP23, aes(x = date, y = total_cases)) +
  geom_area(fill = "#6551CC", alpha = 0.7) +
  geom_line(fill = "#260F99") +
  scale_x_date(date_breaks = "3 months") +
  labs(x = "Meses", y = "Casos totales") +
  ggtitle("Casos totales del 2023", subtitle = "Total de casos por día") + 
  theme(panel.background = element_rect(fill = "#EDE7F6")) +
  theme(panel.border = element_rect(fill = "transparent", 
                                    color = "#212121",           
                                    size = 1))    

ggplot(ESP23, aes(x = date, y = new_cases)) +
  geom_area(fill = "#6551CC", alpha = 0.7) +
  geom_line(fill = "#260F99") +
  scale_x_date(date_breaks = "3 months") +
  labs(x = "Meses", y = "Nuevos casos") +
  ggtitle("Nuevos casos del 2023", subtitle = "Nuevos casos por día") + 
  theme(panel.background = element_rect(fill = "#EDE7F6")) +
  theme(panel.border = element_rect(fill = "transparent", 
                                    color = "#212121",
                                    size = 1))

#Total de muertes y Nuevas muertes

ggplot(ESP23, aes(x = date, y = total_deaths)) +
  geom_area(fill = "#2E7D32", alpha = 0.7) +
  geom_line(fill = "#088158") +
  scale_x_date(date_breaks = "3 months") +
  labs(x = "Meses", y = "Decesos totales") +
  ggtitle("Decesos totales del 2023", subtitle = "Total de Decesos por día") + 
  theme(panel.background = element_rect(fill = "#E8F5E9")) +
  theme(panel.border = element_rect(fill = "transparent", 
                                    color = "#212121",           
                                    size = 1))

ggplot(ESP23, aes(x = date, y = new_deaths)) +
  geom_area(fill = "#2E7D32", alpha = 0.7) +
  geom_line(fill = "#088158") +
  scale_x_date(date_breaks = "3 months") +
  labs(x = "Meses", y = "Nuevas muertes") +
  ggtitle("Nuevas muertes del 2023", subtitle = "Nuevas muertes por día") + 
  theme(panel.background = element_rect(fill = "#E8F5E9")) +
  theme(panel.border = element_rect(fill = "transparent", 
                                    color = "#212121",           
                                    size = 1))

#Total de vacunas y nuevas vacunas

ggplot(ESP23, aes(x = date, y = total_vaccinations)) +
  geom_area(fill = "#B26F2C", alpha = 0.7) +
  geom_line(fill = "#99540F") +
  scale_x_date(date_breaks = "3 months") +
  labs(x = "Meses", y = "Total de vacunas") +
  ggtitle("Total de vacunas del 2023", subtitle = "Total de Vacunas por día") + 
  theme(panel.background = element_rect(fill = "#F2DACE")) +
  theme(panel.border = element_rect(fill = "transparent", 
                                    color = "#212121",           
                                    size = 1))

ggplot(ESP23, aes(x = date, y = new_vaccinations)) +
  geom_area(fill = "#B26F2C", alpha = 0.7) +
  geom_line(fill = "#99540F") +
  scale_x_date(date_breaks = "3 months") +
  labs(x = "Meses", y = "Nuevas vacunas") +
  ggtitle("Nuevas vacunas del 2023", subtitle = "Nuevas vacunas por día") + 
  theme(panel.background = element_rect(fill = "#F2DACE")) +
  theme(panel.border = element_rect(fill = "transparent", 
                                    color = "#212121",           
                                    size = 1))

#Pais reino unido
#Separamos a GBR por año
GBR20 <- data.frame (DfGBR [01:364,])
GBR21 <- data_frame (DfGBR [365:729,])
GBR22 <- data.frame (DfGBR [730:1094,])
GBR23 <- data.frame (DfGBR [1095:1266,])
 
#Verificamos los valores NA´s

#2020
sum(is.na(GBR20))

#Comenzamos reemplazando los valores
#casos y muertes
GBR20 [01:29,05] <- 0
GBR20 [01:29,08] <- 0.000
GBR20 [01:29,09] <- 0
GBR20 [01:29,12] <- 0.000
#Vacunados
GBR20 [01:364,13] <- 0
GBR20 [01:364,14] <- 0
GBR20 [01:364,15] <- 0.000
GBR20 [01:364,16] <- 0
GBR20 [01:364,17] <- 0.000

#2021
sum(is.na(GBR21))
#Dia 01 al 09
GBR21 [01:09,13] <- 0
GBR21 [01:09,14] <- 0
GBR21 [01:09,15] <- 0.000
GBR21 [01:09,16] <- 0
GBR21 [01:09,17] <- 0.000
#Dia 10
GBR21 [10,14] <- 2677971

#2022
#Dias 248 al 254
GBR22 [248:254,13] <- 151248820
GBR22 [248:254,14] <- 0
GBR22 [248:254,15] <- 224.04
#Dias 255 al 365
GBR22 [255:365,13] <- 151248820
GBR22 [255:365,14] <- 0
GBR22 [255:365,15] <- 224.04
GBR22 [255:365,16] <- 53813491
GBR22 [255:365,17] <- 79.71

#2023
GBR23 [01:172,13] <- 151248820
GBR23 [01:172,14] <- 0
GBR23 [01:172,15] <- 224.04
GBR23 [01:172,16] <- 53813491
GBR23 [01:172,17] <- 79.71

#Creacion de la variable new_vaccinations_per_hundred para cada pais

GBR20 <- mutate(GBR20, new_vaccinations_per_hundred=new_vaccinations/population*100)
GBR21 <- mutate(GBR21, new_vaccinations_per_hundred=new_vaccinations/population*100)
GBR22 <- mutate(GBR22, new_vaccinations_per_hundred=new_vaccinations/population*100)
GBR23 <- mutate(GBR23, new_vaccinations_per_hundred=new_vaccinations/population*100)

#Estadisticos por año 
#Reino Unido

#Casos
Casos_GBR20 <- GBR20 %>% summarise(Promedio_total=mean(total_cases),
                                     D.Est_total=sd(total_cases),
                                     CV_total=sd(total_cases)/mean(total_cases)*100,
                                     Min_total=min(total_cases),
                                     Max_total=max(total_cases),
                                     Promedio_new=mean(new_cases),
                                     D.Est_new=sd(new_cases),
                                     CV_new=sd(new_cases)/mean(new_cases)*100,
                                     Min_new=min(new_cases),
                                     Max_new=max(new_cases))

Tasa_de_casos_GBR20 <- GBR20 %>% summarise(Promedio_total=mean(total_cases_per_million),
                                             D.Est_total=sd(total_cases_per_million),
                                             CV_total=
                                               sd(total_cases_per_million)/mean(total_cases_per_million)*100,
                                             Min_total=min(total_cases_per_million),
                                             Max_total=max(total_cases_per_million),
                                             Promedio_new=mean(new_cases_per_million),
                                             D.Est_new=sd(new_cases_per_million),
                                             CV_new=
                                               sd(new_cases_per_million)/mean(new_cases_per_million)*100,
                                             Min_new=min(new_cases_per_million),
                                             Max_new=max(new_cases_per_million))


Casos_GBR21 <- GBR21 %>% summarise(Promedio_total=mean(total_cases),
                                     D.Est_total=sd(total_cases),
                                     CV_total=sd(total_cases)/mean(total_cases)*100,
                                     Min_total=min(total_cases),
                                     Max_total=max(total_cases),
                                     Promedio_new=mean(new_cases),
                                     D.Est_new=sd(new_cases),
                                     CV_new=sd(new_cases)/mean(new_cases)*100,
                                     Min_new=min(new_cases),
                                     Max_new=max(new_cases))

Tasa_de_casos_GBR21 <- GBR21 %>% summarise(Promedio_total=mean(total_cases_per_million),
                                             D.Est_total=sd(total_cases_per_million),
                                             CV_total=
                                               sd(total_cases_per_million)/mean(total_cases_per_million)*100,
                                             Min_total=min(total_cases_per_million),
                                             Max_total=max(total_cases_per_million),
                                             Promedio_new=mean(new_cases_per_million),
                                             D.Est_new=sd(new_cases_per_million),
                                             CV_new=
                                               sd(new_cases_per_million)/mean(new_cases_per_million)*100,
                                             Min_new=min(new_cases_per_million),
                                             Max_new=max(new_cases_per_million))

Casos_GBR22 <- GBR22 %>% summarise(Promedio_total=mean(total_cases),
                                     D.Est_total=sd(total_cases),
                                     CV_total=sd(total_cases)/mean(total_cases)*100,
                                     Min_total=min(total_cases),
                                     Max_total=max(total_cases),
                                     Promedio_new=mean(new_cases),
                                     D.Est_new=sd(new_cases),
                                     CV_new=sd(new_cases)/mean(new_cases)*100,
                                     Min_new=min(new_cases),
                                     Max_new=max(new_cases))

Tasa_de_casos_GBR22 <- GBR22 %>% summarise(Promedio_total=mean(total_cases_per_million),
                                             D.Est_total=sd(total_cases_per_million),
                                             CV_total=
                                               sd(total_cases_per_million)/mean(total_cases_per_million)*100,
                                             Min_total=min(total_cases_per_million),
                                             Max_total=max(total_cases_per_million),
                                             Promedio_new=mean(new_cases_per_million),
                                             D.Est_new=sd(new_cases_per_million),
                                             CV_new=
                                               sd(new_cases_per_million)/mean(new_cases_per_million)*100,
                                             Min_new=min(new_cases_per_million),
                                             Max_new=max(new_cases_per_million))

Casos_GBR23 <- GBR23 %>% summarise(Promedio_total=mean(total_cases),
                                     D.Est_total=sd(total_cases),
                                     CV_total=sd(total_cases)/mean(total_cases)*100,
                                     Min_total=min(total_cases),
                                     Max_total=max(total_cases),
                                     Promedio_new=mean(new_cases),
                                     D.Est_new=sd(new_cases),
                                     CV_new=sd(new_cases)/mean(new_cases)*100,
                                     Min_new=min(new_cases),
                                     Max_new=max(new_cases))

Tasa_de_casos_GBR23 <- GBR23 %>% summarise(Promedio_total=mean(total_cases_per_million),
                                             D.Est_total=sd(total_cases_per_million),
                                             CV_total=
                                               sd(total_cases_per_million)/mean(total_cases_per_million)*100,
                                             Min_total=min(total_cases_per_million),
                                             Max_total=max(total_cases_per_million),
                                             Promedio_new=mean(new_cases_per_million),
                                             D.Est_new=sd(new_cases_per_million),
                                             CV_new=
                                               sd(new_cases_per_million)/mean(new_cases_per_million)*100,
                                             Min_new=min(new_cases_per_million),
                                             Max_new=max(new_cases_per_million))
#GBR Vacunaciones 
Vacunas_GBR20 <- GBR20 %>% summarise(Promedio_total=mean(total_vaccinations),
                                       D.Est_total=sd(total_vaccinations),
                                       CV_total=sd(total_vaccinations)/mean(total_vaccinations)*100,
                                       Min_total=min(total_vaccinations),
                                       Max_total=max(total_vaccinations),
                                       Promedio_new=mean(new_vaccinations),
                                       D.Est_new=sd(new_vaccinations),
                                       CV_new=sd(new_vaccinations)/mean(new_vaccinations)*100,
                                       Min_new=min(new_vaccinations),
                                       Max_new=max(new_vaccinations))

Tasa_de_vacunas_GBR20 <- GBR20 %>% summarise(Promedio_total=mean(total_vaccinations_per_hundred),
                                               D.Est_total=sd(total_vaccinations_per_hundred),
                                               CV_total=
                                                 sd(total_vaccinations_per_hundred)/
                                                 mean(total_vaccinations_per_hundred)*100,
                                               Min_total=min(total_vaccinations_per_hundred),
                                               Max_total=max(total_vaccinations_per_hundred),
                                               Promedio_new=mean(new_vaccinations_per_hundred),
                                               D.Est_new=sd(new_vaccinations_per_hundred),
                                               CV_new=
                                                 sd(new_vaccinations_per_hundred)/
                                                 mean(new_vaccinations_per_hundred)*100,
                                               Min_new=min(new_vaccinations_per_hundred),
                                               Max_new=max(new_vaccinations_per_hundred))

Vacunas_GBR21 <- GBR21 %>% summarise(Promedio_total=mean(total_vaccinations),
                                       D.Est_total=sd(total_vaccinations),
                                       CV_total=sd(total_vaccinations)/mean(total_vaccinations)*100,
                                       Min_total=min(total_vaccinations),
                                       Max_total=max(total_vaccinations),
                                       Promedio_new=mean(new_vaccinations),
                                       D.Est_new=sd(new_vaccinations),
                                       CV_new=sd(new_vaccinations)/mean(new_vaccinations)*100,
                                       Min_new=min(new_vaccinations),
                                       Max_new=max(new_vaccinations))

Tasa_de_vacunas_GBR21 <- GBR21 %>% summarise(Promedio_total=mean(total_vaccinations_per_hundred),
                                               D.Est_total=sd(total_vaccinations_per_hundred),
                                               CV_total=
                                                 sd(total_vaccinations_per_hundred)/
                                                 mean(total_vaccinations_per_hundred)*100,
                                               Min_total=min(total_vaccinations_per_hundred),
                                               Max_total=max(total_vaccinations_per_hundred),
                                               Promedio_new=mean(new_vaccinations_per_hundred),
                                               D.Est_new=sd(new_vaccinations_per_hundred),
                                               CV_new=
                                                 sd(new_vaccinations_per_hundred)/
                                                 mean(new_vaccinations_per_hundred)*100,
                                               Min_new=min(new_vaccinations_per_hundred),
                                               Max_new=max(new_vaccinations_per_hundred))

Vacunas_GBR22 <- GBR22 %>% summarise(Promedio_total=mean(total_vaccinations),
                                       D.Est_total=sd(total_vaccinations),
                                       CV_total=sd(total_vaccinations)/mean(total_vaccinations)*100,
                                       Min_total=min(total_vaccinations),
                                       Max_total=max(total_vaccinations),
                                       Promedio_new=mean(new_vaccinations),
                                       D.Est_new=sd(new_vaccinations),
                                       CV_new=sd(new_vaccinations)/mean(new_vaccinations)*100,
                                       Min_new=min(new_vaccinations),
                                       Max_new=max(new_vaccinations))

Tasa_de_vacunas_GBR22 <- GBR22 %>% summarise(Promedio_total=mean(total_vaccinations_per_hundred),
                                               D.Est_total=sd(total_vaccinations_per_hundred),
                                               CV_total=
                                                 sd(total_vaccinations_per_hundred)/
                                                 mean(total_vaccinations_per_hundred)*100,
                                               Min_total=min(total_vaccinations_per_hundred),
                                               Max_total=max(total_vaccinations_per_hundred),
                                               Promedio_new=mean(new_vaccinations_per_hundred),
                                               D.Est_new=sd(new_vaccinations_per_hundred),
                                               CV_new=
                                                 sd(new_vaccinations_per_hundred)/
                                                 mean(new_vaccinations_per_hundred)*100,
                                               Min_new=min(new_vaccinations_per_hundred),
                                               Max_new=max(new_vaccinations_per_hundred))

Vacunas_GBR23 <- GBR23 %>% summarise(Promedio_total=mean(total_vaccinations),
                                       D.Est_total=sd(total_vaccinations),
                                       CV_total=sd(total_vaccinations)/mean(total_vaccinations)*100,
                                       Min_total=min(total_vaccinations),
                                       Max_total=max(total_vaccinations),
                                       Promedio_new=mean(new_vaccinations),
                                       D.Est_new=sd(new_vaccinations),
                                       CV_new=sd(new_vaccinations)/mean(new_vaccinations)*100,
                                       Min_new=min(new_vaccinations),
                                       Max_new=max(new_vaccinations))

Tasa_de_vacunas_GBR23 <- GBR23 %>% summarise(Promedio_total=mean(total_vaccinations_per_hundred),
                                               D.Est_total=sd(total_vaccinations_per_hundred),
                                               CV_total=
                                                 sd(total_vaccinations_per_hundred)/
                                                 mean(total_vaccinations_per_hundred)*100,
                                               Min_total=min(total_vaccinations_per_hundred),
                                               Max_total=max(total_vaccinations_per_hundred),
                                               Promedio_new=mean(new_vaccinations_per_hundred),
                                               D.Est_new=sd(new_vaccinations_per_hundred),
                                               CV_new=
                                                 sd(new_vaccinations_per_hundred)/
                                                 mean(new_vaccinations_per_hundred)*100,
                                               Min_new=min(new_vaccinations_per_hundred),
                                               Max_new=max(new_vaccinations_per_hundred))
#GBR, Decesos

Decesos_GBR20 <- GBR20 %>% summarise(Promedio_total=mean(total_deaths),
                                       D.Est_total=sd(total_deaths),
                                       CV_total=sd(total_deaths)/mean(total_deaths)*100,
                                       Min_total=min(total_deaths),
                                       Max_total=max(total_deaths),
                                       Promedio_new=mean(new_deaths),
                                       D.Est_new=sd(new_deaths),
                                       CV_new=sd(new_deaths)/mean(new_deaths)*100,
                                       Min_new=min(new_deaths),
                                       Max_new=max(new_deaths))

Tasa_de_decesos_GBR20 <- GBR20 %>% summarise(Promedio_total=mean(total_deaths_per_million),
                                               D.Est_total=sd(total_deaths_per_million),
                                               CV_total=
                                                 sd(total_deaths_per_million)/mean(total_deaths_per_million)*100,
                                               Min_total=min(total_deaths_per_million),
                                               Max_total=max(total_deaths_per_million),
                                               Promedio_new=mean(new_deaths_per_million),
                                               D.Est_new=sd(new_deaths_per_million),
                                               CV_new=
                                                 sd(new_deaths_per_million)/mean(new_deaths_per_million)*100,
                                               Min_new=min(new_deaths_per_million),
                                               Max_new=max(new_deaths_per_million))

Decesos_GBR21 <- GBR21 %>% summarise(Promedio_total=mean(total_deaths),
                                       D.Est_total=sd(total_deaths),
                                       CV_total=sd(total_deaths)/mean(total_deaths)*100,
                                       Min_total=min(total_deaths),
                                       Max_total=max(total_deaths),
                                       Promedio_new=mean(new_deaths),
                                       D.Est_new=sd(new_deaths),
                                       CV_new=sd(new_deaths)/mean(new_deaths)*100,
                                       Min_new=min(new_deaths),
                                       Max_new=max(new_deaths))

Tasa_de_decesos_GBR21 <- GBR21 %>% summarise(Promedio_total=mean(total_deaths_per_million),
                                               D.Est_total=sd(total_deaths_per_million),
                                               CV_total=
                                                 sd(total_deaths_per_million)/mean(total_deaths_per_million)*100,
                                               Min_total=min(total_deaths_per_million),
                                               Max_total=max(total_deaths_per_million),
                                               Promedio_new=mean(new_deaths_per_million),
                                               D.Est_new=sd(new_deaths_per_million),
                                               CV_new=
                                                 sd(new_deaths_per_million)/mean(new_deaths_per_million)*100,
                                               Min_new=min(new_deaths_per_million),
                                               Max_new=max(new_deaths_per_million))

Decesos_GBR22 <- GBR22 %>% summarise(Promedio_total=mean(total_deaths),
                                       D.Est_total=sd(total_deaths),
                                       CV_total=sd(total_deaths)/mean(total_deaths)*100,
                                       Min_total=min(total_deaths),
                                       Max_total=max(total_deaths),
                                       Promedio_new=mean(new_deaths),
                                       D.Est_new=sd(new_deaths),
                                       CV_new=sd(new_deaths)/mean(new_deaths)*100,
                                       Min_new=min(new_deaths),
                                       Max_new=max(new_deaths))

Tasa_de_decesos_GBR22 <- GBR22 %>% summarise(Promedio_total=mean(total_deaths_per_million),
                                               D.Est_total=sd(total_deaths_per_million),
                                               CV_total=
                                                 sd(total_deaths_per_million)/mean(total_deaths_per_million)*100,
                                               Min_total=min(total_deaths_per_million),
                                               Max_total=max(total_deaths_per_million),
                                               Promedio_new=mean(new_deaths_per_million),
                                               D.Est_new=sd(new_deaths_per_million),
                                               CV_new=
                                                 sd(new_deaths_per_million)/mean(new_deaths_per_million)*100,
                                               Min_new=min(new_deaths_per_million),
                                               Max_new=max(new_deaths_per_million))

Decesos_GBR23 <- GBR23 %>% summarise(Promedio_total=mean(total_deaths),
                                       D.Est_total=sd(total_deaths),
                                       CV_total=sd(total_deaths)/mean(total_deaths)*100,
                                       Min_total=min(total_deaths),
                                       Max_total=max(total_deaths),
                                       Promedio_new=mean(new_deaths),
                                       D.Est_new=sd(new_deaths),
                                       CV_new=sd(new_deaths)/mean(new_deaths)*100,
                                       Min_new=min(new_deaths),
                                       Max_new=max(new_deaths))

Tasa_de_decesos_GBR23 <- GBR23 %>% summarise(Promedio_total=mean(total_deaths_per_million),
                                               D.Est_total=sd(total_deaths_per_million),
                                               CV_total=
                                                 sd(total_deaths_per_million)/mean(total_deaths_per_million)*100,
                                               Min_total=min(total_deaths_per_million),
                                               Max_total=max(total_deaths_per_million),
                                               Promedio_new=mean(new_deaths_per_million),
                                               D.Est_new=sd(new_deaths_per_million),
                                               CV_new=
                                                 sd(new_deaths_per_million)/mean(new_deaths_per_million)*100,
                                               Min_new=min(new_deaths_per_million),
                                               Max_new=max(new_deaths_per_million))

#Comenzamos con el analisis de India y el reemplazo de sus valores NA´s

IND20 <- data.frame (DfIND [01:364,])
IND21 <- data.frame (DfIND [365:729,])
IND22 <- data.frame (DfIND [730:1094,])
IND23 <- data.frame (DfIND [1095:1270,])


#2020
#Verificamos cuantos NA's hay en este ano

sum(is.na(IND20))

#En el año no tenemos registros de casos de covid y muertes en todo el mes de enero
#los valores NA´s son 0
#Mes Enero
#Casos 
IND20 [01:27,05] <- 0
IND20 [01:27,08] <- 0.000
#Muertes
IND20 [01:70,09] <- 0
IND20 [01:70,12] <- 0.000
#Vacunados
IND20 [01:364,13] <- 0
IND20 [01:364,14] <- 0
IND20 [01:364,15] <- 0.000
IND20 [01:364,16] <- 0
IND20 [01:364,17] <- 0.000
#Dias 31 y 32 se acomoda porque hay un error de transcripcion
IND20 [31,05] <- 5
IND20 [31,06] <- 0
IND20 [31,07] <- 0.000
IND20 [31,08] <- 0.004
#Dias 32 y 33
IND20 [32,05] <- 5
IND20 [32,06] <- 0
IND20 [32,07] <- 0.000
IND20 [32,08] <- 0.004
#Dia 33
IND20 [33,05] <- 7
IND20 [33,06] <- 2
IND20 [33,08] <- 0.005
#Dia 60
IND20 [60,05] <- 7
IND20 [60,06] <- 0
IND20 [60,07] <- 0.000
IND20 [60,08] <- 0.004
#Dia 61
IND20 [61,06] <- 2

#Ahora verificamos que no nos quede ningun valor NA´s
sum(is.na(IND20))

#2021
#Verificamos cuantos valores NA´s tenemos en este año
sum(is.na(IND21))

#Dias del 01 al 15
IND21 [01:15,13] <- 0
IND21 [01:15,14] <- 0
IND21 [01:15,15] <- 0.000
IND21 [01:15,16] <- 0
IND21 [01:15,17] <- 0.000
#Dias 45 y 46
IND21 [45,13] <- 8516771
IND21 [45,14] <- 0
IND21 [45,15] <- 0.57
IND21 [45,16] <- 8044786
IND21 [45,17] <- 0.57
IND21 [46,14] <- (8516771-8516771)
#Dia 110
IND21 [110,13] <- 127129113
IND21 [110,14] <- 0
IND21 [110,15] <- 8.97
IND21 [110,16] <- 109659181
IND21 [110,17] <- 7.74
#Dia 111
IND21 [111,14] <- (129646105-127129113)
#Dias 113 al 114
IND21 [113:114,13] <- 132754608
IND21 [113:114,14] <- 0
IND21 [113:114,15] <- 9.37
IND21 [113:114,16] <- 113584269
IND21 [113:114,17] <- 8.01
#Dia 115
IND21 [115,14] <- (139185173-132754608)
#Dia 141
IND21 [141,13] <- 187886885
IND21 [141,14] <- 0
IND21 [141,15] <- 13.26
IND21 [141,16] <- 146624652
IND21 [141,17] <- 10.35
#Dia 142
IND21 [142,14] <- (190842497-187886885)
#Dia 190
IND21 [190,13] <- 368991222
IND21 [190,14] <- 0
IND21 [190,15] <- 26.04
IND21 [190,16] <- 298678913
IND21 [190,17] <- 21.08
#Dia 191
IND21 [191,14] <- (376032586-368991222)
#Dia 199
IND21 [199,13] <- 404931715
IND21 [199,14] <- 0
IND21 [199,15] <- 28.57
IND21 [199,16] <- 321893762
IND21 [199,17] <- 22.71
#Dia 200
IND21 [200,14] <- (411846401-404931715)
#Dia 212
IND21 [212,13] <- 461518479
IND21 [212,14] <- 0
IND21 [212,15] <- 32.57
IND21 [212,16] <- 359820313
IND21 [212,17] <- 25.39
#Dia 213
IND21 [213,14] <- (472223639-461518479)
#Dia 214
IND21 [214,13] <- 472223639
IND21 [214,14] <- 0
IND21 [214,15] <- 33.32
IND21 [214,16] <- 367994586
IND21 [214,17] <- 25.97
#Dia 215
IND21 [215,14] <- (485286570-472223639)
#Dia 224
IND21 [224,13] <- 523671019
IND21 [224,14] <- 0
IND21 [224,15] <- 36.95
IND21 [224,16] <- 406980329
IND21 [224,17] <- 28.72
#Dia 225
IND21 [225,14] <- (536189903-523671019)
#Dia 227
IND21 [227,13] <- 543846290
IND21 [227,14] <- 0
IND21 [227,15] <- 38.38
IND21 [227,16] <- 422575401
IND21 [227,17] <- 29.82
#Dia 228
IND21 [228,14] <- (550520038-543846290)
sum(is.na(IND21))
#2022
#Verificamos los valores NA´s
sum(is.na(IND22))

#Dia 92 al 93
IND22 [92:93,13] <- 1843313964
IND22 [92:93,14] <- 0
IND22 [92:93,15] <- 130.07
IND22 [92:93,16] <- 989520028
IND22 [92:93,17] <- 69.82
#Dia 94
IND22 [94,14] <- (1847000842-1843313964)
#Dia 106 al 107
IND22 [106:107,13] <- 1862475218
IND22 [106:107,14] <- 0
IND22 [106:107,15] <- 131.42
IND22 [106:107,16] <- 997038307
IND22 [106:107,17] <- 70.35
#Dia 108
IND22 [108,14] <- (1865530788-1862475218)
#Dia 113
IND22 [113,13] <- 1872146953
IND22 [113,14] <- 0
IND22 [113,15] <- 132.10
IND22 [113,16] <- 999458254
IND22 [113,17] <- 70.52
#Dia 114
IND22 [114,14] <- (1875857737-1872146953)
#Dia 190
IND22 [190,13] <- 1984334136
IND22 [190,14] <- 0
IND22 [190,15] <- 140.02
IND22 [190,16] <- 1017673041
IND22 [190,17] <- 71.81
#Dia 191
IND22 [191,14] <- (1986908392-1984334136)
#Dia 197 al 198
IND22 [197:198,13] <- 1996965088
IND22 [197:198,14] <- 0
IND22 [197:198,15] <- 140.91
IND22 [197:198,16] <- 1018896980
IND22 [197:198,17] <- 71.90
#Dia 199
IND22 [199,14] <- (2001968030-1996965088)
#Dia 215
IND22 [215,13] <- 2046516773
IND22 [215,14] <- 0
IND22 [215,15] <- 144.41
IND22 [215,16] <- 1021500031
IND22 [215,17] <- 72.08
#Dia 216
IND22 [216,14] <- (2052900354-2046516773)
#Dia 286
IND22 [286,13] <- 2191338497
IND22 [286,14] <- 0
IND22 [286,15] <- 154.63
IND22 [286,16] <- 1026581890
IND22 [286,17] <- 72.44
#Dia 287
IND22 [287,14] <- (2192191132-2191338497)
#Dia 304
IND22 [304,13] <- 2196335748
IND22 [304,14] <- 0
IND22 [304,15] <- 154.98
IND22 [304,16] <- 1026856716
IND22 [304,17] <- 72.46
#Dia 305
IND22 [305,14] <- (2196483169-2196335748)
#Dia 319 en este dia se encntro una inconlcuencia que tiene menos casos totales que el dia anterior
#Lo colocamos igual al dia 318 
IND22 [318:319,05] <- 44666377
IND22 [318:319,06] <- 0
IND22 [318:319,07] <- 0.5180000
IND22 [318:319,08] <- 31517.94
#Dia 320
IND22 [320,06] <- 299
#Verificamos que no quede algun valor NA
sum(is.na(IND22))

#2023
#Contamos los valores NA's que tenemos en este año
sum(is.na(IND23))

#Dia 77
IND23 [77,13] <- 2206462789
IND23 [77,14] <- 0
IND23 [77,15] <- 155.69
IND23 [77,16] <- 1027385887
IND23 [77,17] <- 72.50
#Dia 78
IND23 [78,14] <- (2206477662-2206462789)
#Dia 99
IND23 [99,13] <- 2206595334
IND23 [99,14] <- 0
IND23 [99,15] <- 155.70
IND23 [99,16] <- 1027401427
IND23 [99,17] <- 72.50
#Dia 100
IND23 [100,14] <- (2206596119-2206595334)
#Dia 158
IND23 [158,13] <- 2206698880
IND23 [158,14] <- 0
IND23 [158,15] <- 155.71
IND23 [158,16] <- 1027409573
IND23 [158,17] <- 72.50
#Dia 159
IND23 [159,14] <- (2206700691-2206698880)
#Dias 173 al176
IND23 [173:176,05] <- 44993543
IND23 [173:176,06] <- 0
IND23 [173:176,07] <- 0.000
IND23 [173:176,08] <- 31748.80
IND23 [173:176,09] <- 531896
IND23 [173:176,10] <- 0
IND23 [173:176,11] <- 0.000
IND23 [173:176,12] <- 375.322

#Creacion de la variable new_vaccinations_per_hundred para cada pais

IND20 <- mutate(IND20, new_vaccinations_per_hundred=new_vaccinations/population*100)
IND21 <- mutate(IND21, new_vaccinations_per_hundred=new_vaccinations/population*100)
IND22 <- mutate(IND22, new_vaccinations_per_hundred=new_vaccinations/population*100)
IND23 <- mutate(IND23, new_vaccinations_per_hundred=new_vaccinations/population*100)

#Estadisticos por año 

#Casos
Casos_IND20 <- IND20 %>% summarise(Promedio_total=mean(total_cases),
                                   D.Est_total=sd(total_cases),
                                   CV_total=sd(total_cases)/mean(total_cases)*100,
                                   Min_total=min(total_cases),
                                   Max_total=max(total_cases),
                                   Promedio_new=mean(new_cases),
                                   D.Est_new=sd(new_cases),
                                   CV_new=sd(new_cases)/mean(new_cases)*100,
                                   Min_new=min(new_cases),
                                   Max_new=max(new_cases))

Tasa_de_casos_IND20 <- IND20 %>% summarise(Promedio_total=mean(total_cases_per_million),
                                           D.Est_total=sd(total_cases_per_million),
                                           CV_total=
                                             sd(total_cases_per_million)/mean(total_cases_per_million)*100,
                                           Min_total=min(total_cases_per_million),
                                           Max_total=max(total_cases_per_million),
                                           Promedio_new=mean(new_cases_per_million),
                                           D.Est_new=sd(new_cases_per_million),
                                           CV_new=
                                             sd(new_cases_per_million)/mean(new_cases_per_million)*100,
                                           Min_new=min(new_cases_per_million),
                                           Max_new=max(new_cases_per_million))


Casos_IND21 <- IND21 %>% summarise(Promedio_total=mean(total_cases),
                                   D.Est_total=sd(total_cases),
                                   CV_total=sd(total_cases)/mean(total_cases)*100,
                                   Min_total=min(total_cases),
                                   Max_total=max(total_cases),
                                   Promedio_new=mean(new_cases),
                                   D.Est_new=sd(new_cases),
                                   CV_new=sd(new_cases)/mean(new_cases)*100,
                                   Min_new=min(new_cases),
                                   Max_new=max(new_cases))

Tasa_de_casos_IND21 <- IND21 %>% summarise(Promedio_total=mean(total_cases_per_million),
                                           D.Est_total=sd(total_cases_per_million),
                                           CV_total=
                                             sd(total_cases_per_million)/mean(total_cases_per_million)*100,
                                           Min_total=min(total_cases_per_million),
                                           Max_total=max(total_cases_per_million),
                                           Promedio_new=mean(new_cases_per_million),
                                           D.Est_new=sd(new_cases_per_million),
                                           CV_new=
                                             sd(new_cases_per_million)/mean(new_cases_per_million)*100,
                                           Min_new=min(new_cases_per_million),
                                           Max_new=max(new_cases_per_million))

Casos_IND22 <- IND22 %>% summarise(Promedio_total=mean(total_cases),
                                   D.Est_total=sd(total_cases),
                                   CV_total=sd(total_cases)/mean(total_cases)*100,
                                   Min_total=min(total_cases),
                                   Max_total=max(total_cases),
                                   Promedio_new=mean(new_cases),
                                   D.Est_new=sd(new_cases),
                                   CV_new=sd(new_cases)/mean(new_cases)*100,
                                   Min_new=min(new_cases),
                                   Max_new=max(new_cases))

Tasa_de_casos_IND22 <- IND22 %>% summarise(Promedio_total=mean(total_cases_per_million),
                                           D.Est_total=sd(total_cases_per_million),
                                           CV_total=
                                             sd(total_cases_per_million)/mean(total_cases_per_million)*100,
                                           Min_total=min(total_cases_per_million),
                                           Max_total=max(total_cases_per_million),
                                           Promedio_new=mean(new_cases_per_million),
                                           D.Est_new=sd(new_cases_per_million),
                                           CV_new=
                                             sd(new_cases_per_million)/mean(new_cases_per_million)*100,
                                           Min_new=min(new_cases_per_million),
                                           Max_new=max(new_cases_per_million))

Casos_IND23 <- IND23 %>% summarise(Promedio_total=mean(total_cases),
                                   D.Est_total=sd(total_cases),
                                   CV_total=sd(total_cases)/mean(total_cases)*100,
                                   Min_total=min(total_cases),
                                   Max_total=max(total_cases),
                                   Promedio_new=mean(new_cases),
                                   D.Est_new=sd(new_cases),
                                   CV_new=sd(new_cases)/mean(new_cases)*100,
                                   Min_new=min(new_cases),
                                   Max_new=max(new_cases))

Tasa_de_casos_IND23 <- IND23 %>% summarise(Promedio_total=mean(total_cases_per_million),
                                           D.Est_total=sd(total_cases_per_million),
                                           CV_total=
                                             sd(total_cases_per_million)/mean(total_cases_per_million)*100,
                                           Min_total=min(total_cases_per_million),
                                           Max_total=max(total_cases_per_million),
                                           Promedio_new=mean(new_cases_per_million),
                                           D.Est_new=sd(new_cases_per_million),
                                           CV_new=
                                             sd(new_cases_per_million)/mean(new_cases_per_million)*100,
                                           Min_new=min(new_cases_per_million),
                                           Max_new=max(new_cases_per_million))
#Vacunaciones 
Vacunas_IND20 <- IND20 %>% summarise(Promedio_total=mean(total_vaccinations),
                                     D.Est_total=sd(total_vaccinations),
                                     CV_total=sd(total_vaccinations)/mean(total_vaccinations)*100,
                                     Min_total=min(total_vaccinations),
                                     Max_total=max(total_vaccinations),
                                     Promedio_new=mean(new_vaccinations),
                                     D.Est_new=sd(new_vaccinations),
                                     CV_new=sd(new_vaccinations)/mean(new_vaccinations)*100,
                                     Min_new=min(new_vaccinations),
                                     Max_new=max(new_vaccinations))

Tasa_de_vacunas_IND20 <- IND20 %>% summarise(Promedio_total=mean(total_vaccinations_per_hundred),
                                             D.Est_total=sd(total_vaccinations_per_hundred),
                                             CV_total=
                                               sd(total_vaccinations_per_hundred)/
                                               mean(total_vaccinations_per_hundred)*100,
                                             Min_total=min(total_vaccinations_per_hundred),
                                             Max_total=max(total_vaccinations_per_hundred),
                                             Promedio_new=mean(new_vaccinations_per_hundred),
                                             D.Est_new=sd(new_vaccinations_per_hundred),
                                             CV_new=
                                               sd(new_vaccinations_per_hundred)/
                                               mean(new_vaccinations_per_hundred)*100,
                                             Min_new=min(new_vaccinations_per_hundred),
                                             Max_new=max(new_vaccinations_per_hundred))

Vacunas_IND21 <- IND21 %>% summarise(Promedio_total=mean(total_vaccinations),
                                     D.Est_total=sd(total_vaccinations),
                                     CV_total=sd(total_vaccinations)/mean(total_vaccinations)*100,
                                     Min_total=min(total_vaccinations),
                                     Max_total=max(total_vaccinations),
                                     Promedio_new=mean(new_vaccinations),
                                     D.Est_new=sd(new_vaccinations),
                                     CV_new=sd(new_vaccinations)/mean(new_vaccinations)*100,
                                     Min_new=min(new_vaccinations),
                                     Max_new=max(new_vaccinations))

Tasa_de_vacunas_IND21 <- IND21 %>% summarise(Promedio_total=mean(total_vaccinations_per_hundred),
                                             D.Est_total=sd(total_vaccinations_per_hundred),
                                             CV_total=
                                               sd(total_vaccinations_per_hundred)/
                                               mean(total_vaccinations_per_hundred)*100,
                                             Min_total=min(total_vaccinations_per_hundred),
                                             Max_total=max(total_vaccinations_per_hundred),
                                             Promedio_new=mean(new_vaccinations_per_hundred),
                                             D.Est_new=sd(new_vaccinations_per_hundred),
                                             CV_new=
                                               sd(new_vaccinations_per_hundred)/
                                               mean(new_vaccinations_per_hundred)*100,
                                             Min_new=min(new_vaccinations_per_hundred),
                                             Max_new=max(new_vaccinations_per_hundred))

Vacunas_IND22 <- IND22 %>% summarise(Promedio_total=mean(total_vaccinations),
                                     D.Est_total=sd(total_vaccinations),
                                     CV_total=sd(total_vaccinations)/mean(total_vaccinations)*100,
                                     Min_total=min(total_vaccinations),
                                     Max_total=max(total_vaccinations),
                                     Promedio_new=mean(new_vaccinations),
                                     D.Est_new=sd(new_vaccinations),
                                     CV_new=sd(new_vaccinations)/mean(new_vaccinations)*100,
                                     Min_new=min(new_vaccinations),
                                     Max_new=max(new_vaccinations))

Tasa_de_vacunas_IND22 <- IND22 %>% summarise(Promedio_total=mean(total_vaccinations_per_hundred),
                                             D.Est_total=sd(total_vaccinations_per_hundred),
                                             CV_total=
                                               sd(total_vaccinations_per_hundred)/
                                               mean(total_vaccinations_per_hundred)*100,
                                             Min_total=min(total_vaccinations_per_hundred),
                                             Max_total=max(total_vaccinations_per_hundred),
                                             Promedio_new=mean(new_vaccinations_per_hundred),
                                             D.Est_new=sd(new_vaccinations_per_hundred),
                                             CV_new=
                                               sd(new_vaccinations_per_hundred)/
                                               mean(new_vaccinations_per_hundred)*100,
                                             Min_new=min(new_vaccinations_per_hundred),
                                             Max_new=max(new_vaccinations_per_hundred))

Vacunas_IND23 <- IND23 %>% summarise(Promedio_total=mean(total_vaccinations),
                                     D.Est_total=sd(total_vaccinations),
                                     CV_total=sd(total_vaccinations)/mean(total_vaccinations)*100,
                                     Min_total=min(total_vaccinations),
                                     Max_total=max(total_vaccinations),
                                     Promedio_new=mean(new_vaccinations),
                                     D.Est_new=sd(new_vaccinations),
                                     CV_new=sd(new_vaccinations)/mean(new_vaccinations)*100,
                                     Min_new=min(new_vaccinations),
                                     Max_new=max(new_vaccinations))

Tasa_de_vacunas_IND23 <- IND23 %>% summarise(Promedio_total=mean(total_vaccinations_per_hundred),
                                             D.Est_total=sd(total_vaccinations_per_hundred),
                                             CV_total=
                                               sd(total_vaccinations_per_hundred)/
                                               mean(total_vaccinations_per_hundred)*100,
                                             Min_total=min(total_vaccinations_per_hundred),
                                             Max_total=max(total_vaccinations_per_hundred),
                                             Promedio_new=mean(new_vaccinations_per_hundred),
                                             D.Est_new=sd(new_vaccinations_per_hundred),
                                             CV_new=
                                               sd(new_vaccinations_per_hundred)/
                                               mean(new_vaccinations_per_hundred)*100,
                                             Min_new=min(new_vaccinations_per_hundred),
                                             Max_new=max(new_vaccinations_per_hundred))
#IND, Decesos

Decesos_IND20 <- IND20 %>% summarise(Promedio_total=mean(total_deaths),
                                     D.Est_total=sd(total_deaths),
                                     CV_total=sd(total_deaths)/mean(total_deaths)*100,
                                     Min_total=min(total_deaths),
                                     Max_total=max(total_deaths),
                                     Promedio_new=mean(new_deaths),
                                     D.Est_new=sd(new_deaths),
                                     CV_new=sd(new_deaths)/mean(new_deaths)*100,
                                     Min_new=min(new_deaths),
                                     Max_new=max(new_deaths))

Tasa_de_decesos_IND20 <- IND20 %>% summarise(Promedio_total=mean(total_deaths_per_million),
                                             D.Est_total=sd(total_deaths_per_million),
                                             CV_total=
                                               sd(total_deaths_per_million)/mean(total_deaths_per_million)*100,
                                             Min_total=min(total_deaths_per_million),
                                             Max_total=max(total_deaths_per_million),
                                             Promedio_new=mean(new_deaths_per_million),
                                             D.Est_new=sd(new_deaths_per_million),
                                             CV_new=
                                               sd(new_deaths_per_million)/mean(new_deaths_per_million)*100,
                                             Min_new=min(new_deaths_per_million),
                                             Max_new=max(new_deaths_per_million))

Decesos_IND21 <- IND21 %>% summarise(Promedio_total=mean(total_deaths),
                                     D.Est_total=sd(total_deaths),
                                     CV_total=sd(total_deaths)/mean(total_deaths)*100,
                                     Min_total=min(total_deaths),
                                     Max_total=max(total_deaths),
                                     Promedio_new=mean(new_deaths),
                                     D.Est_new=sd(new_deaths),
                                     CV_new=sd(new_deaths)/mean(new_deaths)*100,
                                     Min_new=min(new_deaths),
                                     Max_new=max(new_deaths))

Tasa_de_decesos_IND21 <- IND21 %>% summarise(Promedio_total=mean(total_deaths_per_million),
                                             D.Est_total=sd(total_deaths_per_million),
                                             CV_total=
                                               sd(total_deaths_per_million)/mean(total_deaths_per_million)*100,
                                             Min_total=min(total_deaths_per_million),
                                             Max_total=max(total_deaths_per_million),
                                             Promedio_new=mean(new_deaths_per_million),
                                             D.Est_new=sd(new_deaths_per_million),
                                             CV_new=
                                               sd(new_deaths_per_million)/mean(new_deaths_per_million)*100,
                                             Min_new=min(new_deaths_per_million),
                                             Max_new=max(new_deaths_per_million))

Decesos_IND22 <- IND22 %>% summarise(Promedio_total=mean(total_deaths),
                                     D.Est_total=sd(total_deaths),
                                     CV_total=sd(total_deaths)/mean(total_deaths)*100,
                                     Min_total=min(total_deaths),
                                     Max_total=max(total_deaths),
                                     Promedio_new=mean(new_deaths),
                                     D.Est_new=sd(new_deaths),
                                     CV_new=sd(new_deaths)/mean(new_deaths)*100,
                                     Min_new=min(new_deaths),
                                     Max_new=max(new_deaths))

Tasa_de_decesos_IND22 <- IND22 %>% summarise(Promedio_total=mean(total_deaths_per_million),
                                             D.Est_total=sd(total_deaths_per_million),
                                             CV_total=
                                               sd(total_deaths_per_million)/mean(total_deaths_per_million)*100,
                                             Min_total=min(total_deaths_per_million),
                                             Max_total=max(total_deaths_per_million),
                                             Promedio_new=mean(new_deaths_per_million),
                                             D.Est_new=sd(new_deaths_per_million),
                                             CV_new=
                                               sd(new_deaths_per_million)/mean(new_deaths_per_million)*100,
                                             Min_new=min(new_deaths_per_million),
                                             Max_new=max(new_deaths_per_million))

Decesos_IND23 <- IND23 %>% summarise(Promedio_total=mean(total_deaths),
                                     D.Est_total=sd(total_deaths),
                                     CV_total=sd(total_deaths)/mean(total_deaths)*100,
                                     Min_total=min(total_deaths),
                                     Max_total=max(total_deaths),
                                     Promedio_new=mean(new_deaths),
                                     D.Est_new=sd(new_deaths),
                                     CV_new=sd(new_deaths)/mean(new_deaths)*100,
                                     Min_new=min(new_deaths),
                                     Max_new=max(new_deaths))

Tasa_de_decesos_IND23 <- IND23 %>% summarise(Promedio_total=mean(total_deaths_per_million),
                                             D.Est_total=sd(total_deaths_per_million),
                                             CV_total=
                                               sd(total_deaths_per_million)/mean(total_deaths_per_million)*100,
                                             Min_total=min(total_deaths_per_million),
                                             Max_total=max(total_deaths_per_million),
                                             Promedio_new=mean(new_deaths_per_million),
                                             D.Est_new=sd(new_deaths_per_million),
                                             CV_new=
                                               sd(new_deaths_per_million)/mean(new_deaths_per_million)*100,
                                             Min_new=min(new_deaths_per_million),
                                             Max_new=max(new_deaths_per_million))
#Tablas
TablaDEU20 <- rbind(Casos = Casos_DfDEU20, Decesos = Decesos_DfDEU20,
                    Vacunados = Vacunas_DfDEU20)


TablaDEU21 <- rbind(Casos = Casos_DfDEU21, Decesos = Decesos_DfDEU21,
                    Vacunados = Vacunas_DfDEU21)

TablaDEU22 <- rbind(Casos = Casos_DfDEU22, Decesos = Decesos_DfDEU22,
                    Vacunados = Vacunas_DfDEU22)

TablaDEU23 <- rbind(Casos = Casos_DfDEU23, Decesos = Decesos_DfDEU23,
                    Vacunados = Vacunas_DfDEU23)

TablaTUR20 <- rbind(Casos = Casos_DfTUR20, Decesos = Decesos_DfTUR20,
                    Vacunados = Vacunas_DfTUR20)


TablaTUR21 <- rbind(Casos = Casos_DfTUR21, Decesos = Decesos_DfTUR21,
                    Vacunados = Vacunas_DfTUR21)

TablaTUR22 <- rbind(Casos = Casos_DfTUR22, Decesos = Decesos_DfTUR22,
                    Vacunados = Vacunas_DfTUR22)

TablaTUR23 <- rbind(Casos = Casos_DfTUR23, Decesos = Decesos_DfTUR23,
                    Vacunados = Vacunas_DfTUR23)

TablaESP20 <- rbind(Casos = Casos_ESP20, Decesos = Decesos_ESP20,
                    Vacunados = Vacunas_ESP20)

TablaESP21 <- rbind(Casos = Casos_ESP21, Decesos = Decesos_ESP21,
                    Vacunados = Vacunas_ESP21)

TablaESP22 <- rbind(Casos = Casos_ESP22, Decesos = Decesos_ESP22,
                    Vacunados = Vacunas_ESP22)

TablaESP23 <- rbind(Casos = Casos_ESP23, Decesos = Decesos_ESP23,
                    Vacunados = Vacunas_ESP23)

TablaGBR20 <- rbind(Casos = Casos_GBR20, Decesos = Decesos_GBR20,
                    Vacunados = Vacunas_GBR20)

TablaGBR21 <- rbind(Casos = Casos_GBR21, Decesos = Decesos_GBR21,
                    Vacunados = Vacunas_GBR21)

TablaGBR22 <- rbind(Casos = Casos_GBR22, Decesos = Decesos_GBR22,
                    Vacunados = Vacunas_GBR22)

TablaGBR23 <- rbind(Casos = Casos_GBR23, Decesos = Decesos_GBR23,
                    Vacunados = Vacunas_GBR23)

TablaIND20 <- rbind(Casos = Casos_IND20, Decesos = Decesos_IND20,
                    Vacunados = Vacunas_IND20)

TablaIND21 <- rbind(Casos = Casos_IND21, Decesos = Decesos_IND21,
                    Vacunados = Vacunas_IND21)

TablaIND22 <- rbind(Casos = Casos_IND22, Decesos = Decesos_IND22,
                    Vacunados = Vacunas_IND22)

TablaIND23 <- rbind(Casos = Casos_IND23, Decesos = Decesos_IND23,
                    Vacunados = Vacunas_IND23)

#Se muestran los NA en el primer año de Covid en CHN
apply(CHN[CHN$date<"2021-01-01",], MARGIN = 2, function(x) sum(is.na(x))) %>% View()

#Eliminación de variables no necesarias para el trabajo
CHN <- CHN %>% select(index, iso_code, continent, location, date, 
                      total_cases, new_cases, new_cases_per_million,
                      total_cases_per_million, 
                      total_deaths, new_deaths, new_deaths_per_million, 
                      total_deaths_per_million, 
                      total_vaccinations, new_vaccinations, 
                      total_vaccinations_per_hundred, population)

# Cambio de valores NA en China en el primer año
DfCHN20 <- data.frame (CHN [CHN$date<"2021-01-01",], na.rm = TRUE)
DfCHN20 <- DfCHN20 %>% mutate(total_cases =if_else(is.na(total_cases), 0, total_cases))
DfCHN20 <- DfCHN20 %>% mutate(total_cases_per_million =if_else(is.na(total_cases_per_million), 0, total_cases_per_million))
DfCHN20 <- DfCHN20 %>% mutate(total_deaths_per_million =if_else(is.na(total_deaths_per_million), 0, total_deaths_per_million))
DfCHN20 <- DfCHN20 %>% mutate(total_vaccinations =if_else(is.na(total_vaccinations), 0, total_vaccinations))
DfCHN20 <- DfCHN20 %>% mutate(total_vaccinations_per_hundred =if_else(is.na(total_vaccinations_per_hundred), 0, total_vaccinations_per_hundred))
DfCHN20 <- DfCHN20 %>% mutate(new_vaccinations =if_else(is.na(new_vaccinations), 0, new_vaccinations))

#Se muestran los NA del segundo año en CHN
apply(CHN[365:729,], MARGIN = 2, function(x) sum(is.na(x))) %>% View()
#Eliminación de los mismos en el segundo año
DfCHN21 <- data.frame(CHN [365:729,])
DfCHN21 <- DfCHN21 %>% mutate(total_vaccinations =if_else(is.na(total_vaccinations), 0, total_vaccinations))
DfCHN21 <- DfCHN21 %>% mutate(new_vaccinations =if_else(is.na(new_vaccinations), 0, new_vaccinations))
DfCHN21 <- DfCHN21 %>% mutate(total_vaccinations_per_hundred =if_else(is.na(total_vaccinations_per_hundred), 0, total_vaccinations_per_hundred))
DfCHN21 <- DfCHN21 %>% mutate(people_vaccinated =if_else(is.na(people_vaccinated), 0, people_vaccinated))
DfCHN21 <- DfCHN21 %>% mutate(people_fully_vaccinated =if_else(is.na(people_fully_vaccinated), 0, people_fully_vaccinated))
DfCHN21 <- DfCHN21 %>% mutate(people_vaccinated_per_hundred =if_else(is.na(people_vaccinated_per_hundred), 0, people_vaccinated_per_hundred))
DfCHN21 <- DfCHN21 %>% mutate(people_fully_vaccinated_per_hundred =if_else(is.na(people_fully_vaccinated_per_hundred), 0, people_fully_vaccinated_per_hundred))
#Se muestran los NA del tercer año en CHN
apply(CHN[730:1094,], MARGIN = 2, function(x) sum(is.na(x))) %>% View()
#Se eliminan los NA del mismo
DfCHN22 <- data.frame(CHN [730:1094,])
DfCHN22 <- DfCHN22 %>% mutate(people_vaccinated =if_else(is.na(people_vaccinated), 0, people_vaccinated))
DfCHN22 <- DfCHN22 %>% mutate(people_fully_vaccinated =if_else(is.na(people_fully_vaccinated), 0, people_fully_vaccinated))
DfCHN22 <- DfCHN22 %>% mutate(people_vaccinated_per_hundred =if_else(is.na(people_vaccinated_per_hundred), 0, people_vaccinated_per_hundred))
DfCHN22 <- DfCHN22 %>% mutate(people_fully_vaccinated_per_hundred =if_else(is.na(people_fully_vaccinated_per_hundred), 0, people_fully_vaccinated_per_hundred))
#Se muestran los NA del cuarto año de CHN
apply(CHN[CHN$date>"2021-12-31",], MARGIN = 2, function(x) sum(is.na(x))) %>% View()
#Se eleminan los NA del mismo
DfCHN23 <- data.frame(CHN[CHN$date>"2022-12-31",])
DfCHN23 <- DfCHN23 %>% mutate(new_cases =if_else(is.na(new_cases), 0, new_cases))
DfCHN23 <- DfCHN23 %>% mutate(new_cases_per_million =if_else(is.na(new_cases_per_million), 0, new_cases_per_million))
DfCHN23 <- DfCHN23 %>% mutate(total_vaccinations =if_else(is.na(total_vaccinations), 0, total_vaccinations))
DfCHN23 <- DfCHN23 %>% mutate(new_vaccinations =if_else(is.na(new_vaccinations), 0, new_vaccinations))
DfCHN23 <- DfCHN23 %>% mutate(total_vaccinations_per_hundred =if_else(is.na(total_vaccinations_per_hundred), 0, total_vaccinations_per_hundred))
DfCHN23 <- DfCHN23 %>% mutate(people_vaccinated =if_else(is.na(people_vaccinated), 0, people_vaccinated))
DfCHN23 <- DfCHN23 %>% mutate(people_fully_vaccinated =if_else(is.na(people_fully_vaccinated), 0, people_fully_vaccinated))
DfCHN23 <- DfCHN23 %>% mutate(people_vaccinated_per_hundred =if_else(is.na(people_vaccinated_per_hundred), 0, people_vaccinated_per_hundred))
DfCHN23 <- DfCHN23 %>% mutate(people_fully_vaccinated_per_hundred =if_else(is.na(people_fully_vaccinated_per_hundred), 0, people_fully_vaccinated_per_hundred))

      apply(DfRUS [DfRUS$date<"2021-01-01",], MARGIN = 2, function(x) sum(is.na(x))) %>% View()

DfRUS20 <- data.frame(DfRUS [DfRUS$date<"2021-01-01",], na.rm = TRUE)
DfRUS20 <- DfRUS20 %>% mutate(total_cases =if_else(is.na(total_cases), 0, total_cases))
DfRUS20 <- DfRUS20 %>% mutate(total_cases_per_million =if_else(is.na(total_cases_per_million), 0, total_cases_per_million))
DfRUS20 <- DfRUS20 %>% mutate(total_deaths =if_else(is.na(total_deaths), 0, total_deaths))
DfRUS20 <- DfRUS20 %>% mutate(total_deaths_per_million =if_else(is.na(total_deaths_per_million), 0, total_deaths_per_million))
DfRUS20 <- DfRUS20 %>% mutate(total_vaccinations =if_else(is.na(total_vaccinations), 0, total_vaccinations))
DfRUS20 <- DfRUS20 %>% mutate(new_vaccinations =if_else(is.na(new_vaccinations), 0, new_vaccinations))
DfRUS20 <- DfRUS20 %>% mutate(total_vaccinations_per_hundred =if_else(is.na(total_vaccinations_per_hundred), 0, total_vaccinations_per_hundred))

apply(DfRUS[365:729,], MARGIN = 2, function(x) sum(is.na(x))) %>% View()
DfRUS21 <- data.frame(DfRUS [365:729,])
DfRUS21 <- DfRUS21 %>% mutate(total_vaccinations =if_else(is.na(total_vaccinations), 0, total_vaccinations))
DfRUS21 <- DfRUS21 %>% mutate(total_vaccinations_per_hundred =if_else(is.na(total_vaccinations_per_hundred), 0, total_vaccinations_per_hundred))

apply(DfRUS[730:1094,], MARGIN = 2, function(x) sum(is.na(x))) %>% View()
DfRUS22 <- data.frame(DfRUS [730:1094,])

library(dplyr)
DfRUS22 <- DfRUS22 %>% mutate(total_vaccinations =if_else(is.na(total_vaccinations), 0, total_vaccinations))
DfRUS22 <- DfRUS22 %>% mutate(total_vaccinations_per_hundred =if_else(is.na(total_vaccinations_per_hundred), 0, total_vaccinations_per_hundred))
DfRUS22 <- DfRUS22 %>% mutate(new_vaccinations =if_else(is.na(new_vaccinations), 0, new_vaccinations))

apply(DfRUS [DfRUS$date>"2022-12-31",], MARGIN = 2, function(x) sum(is.na(x))) %>% View()
DfRUS23 <- data.frame(DfRUS[DfRUS$date>"2022-12-31",])
DfRUS23 <- DfRUS23 %>% mutate(new_cases =if_else(is.na(new_cases), 0, new_cases))
DfRUS23 <- DfRUS23 %>% mutate(new_cases_per_million =if_else(is.na(new_cases_per_million), 0, new_cases_per_million))
DfRUS23 <- DfRUS23 %>% mutate(new_vaccinations =if_else(is.na(new_vaccinations), 0, new_vaccinations))
DfRUS23 <- DfRUS23 %>% mutate(total_vaccinations =if_else(is.na(total_vaccinations), 0, total_vaccinations))
DfRUS23 <- DfRUS23 %>% mutate(total_vaccinations_per_hundred =if_else(is.na(total_vaccinations_per_hundred), 0, total_vaccinations_per_hundred))

      apply(DfIRN[DfIRN$date<"2021-01-01",], MARGIN = 2, function(x) sum(is.na(x))) %>% View()
DfIRN20 <- data.frame(DfIRN[DfIRN$date<"2021-01-01",], na.rm = TRUE)
DfIRN20 <- DfIRN20 %>% mutate(total_cases =if_else(is.na(total_cases), 0, total_cases))
DfIRN20 <- DfIRN20 %>% mutate(total_cases_per_million =if_else(is.na(total_cases_per_million), 0, total_cases_per_million))
DfIRN20 <- DfIRN20 %>% mutate(total_deaths =if_else(is.na(total_deaths), 0, total_deaths))
DfIRN20 <- DfIRN20 %>% mutate(total_deaths_per_million =if_else(is.na(total_deaths_per_million), 0, total_deaths_per_million))
DfIRN20 <- DfIRN20 %>% mutate(total_vaccinations =if_else(is.na(total_vaccinations), 0, total_vaccinations))
DfIRN20 <- DfIRN20 %>% mutate(total_vaccinations_per_hundred =if_else(is.na(total_vaccinations_per_hundred), 0, total_vaccinations_per_hundred))
DfIRN20 <- DfIRN20 %>% mutate(new_vaccinations =if_else(is.na(new_vaccinations), 0, new_vaccinations))

apply(DfIRN[365:729,], MARGIN = 2, function(x) sum(is.na(x))) %>% View()
DfIRN21 <- data.frame(DfIRN[365:729,])
DfIRN21 <- DfIRN21 %>% mutate(new_vaccinations =if_else(is.na(new_vaccinations), 0, new_vaccinations))
DfIRN21 <- DfIRN21 %>% mutate(total_vaccinations =if_else(is.na(total_vaccinations), 0, total_vaccinations))
DfIRN21 <- DfIRN21 %>% mutate(total_vaccinations_per_hundred =if_else(is.na(total_vaccinations_per_hundred), 0, total_vaccinations_per_hundred))

apply(DfIRN[730:1094,], MARGIN = 2, function(x) sum(is.na(x))) %>% View()
DfIRN22 <- data.frame(DfIRN[730:1094,])
DfIRN22 <- DfIRN22 %>% mutate(new_vaccinations =if_else(is.na(new_vaccinations), 0, new_vaccinations))

library(dplyr)

DfIRN22 <- DfIRN22 %>% mutate(total_vaccinations =if_else(is.na(total_vaccinations), 0, total_vaccinations))
DfIRN22 <- DfIRN22 %>% mutate(total_vaccinations_per_hundred =if_else(is.na(total_vaccinations_per_hundred), 0, total_vaccinations_per_hundred))

apply(DfIRN [DfIRN$date>"2022-12-31",], MARGIN = 2, function(x) sum(is.na(x))) %>% View()
DfIRN23 <- data.frame(DfIRN[DfIRN$date>"2022-12-31",])
DfIRN23 <- DfIRN23 %>% mutate(new_vaccinations =if_else(is.na(new_vaccinations), 0, new_vaccinations))
DfIRN23 <- DfIRN23 %>% mutate(total_vaccinations =if_else(is.na(total_vaccinations), 0, total_vaccinations))
DfIRN23 <- DfIRN23 %>% mutate(total_vaccinations_per_hundred =if_else(is.na(total_vaccinations_per_hundred), 0, total_vaccinations_per_hundred))

      apply(DfCUB[DfCUB$date<"2021-01-01",], MARGIN = 2, function(x) sum(is.na(x))) %>% View()
DfCUB20 <- data.frame(DfCUB[DfCUB$date<"2021-01-01",], na.rm = TRUE)
DfCUB20 <- DfCUB20 %>% mutate(total_cases = if_else(is.na(total_cases), 0, total_cases))
DfCUB20 <- DfCUB20 %>% mutate(total_cases_per_million = if_else(is.na(total_cases_per_million), 0, total_cases_per_million))
DfCUB20 <- DfCUB20 %>% mutate(total_deaths = if_else(is.na(total_deaths), 0, total_deaths))
DfCUB20 <- DfCUB20 %>% mutate(total_deaths_per_million = if_else(is.na(total_deaths_per_million), 0, total_deaths_per_million))
DfCUB20 <- DfCUB20 %>% mutate(new_vaccinations =if_else(is.na(new_vaccinations), 0, new_vaccinations))
DfCUB20 <- DfCUB20 %>% mutate(total_vaccinations =if_else(is.na(total_vaccinations), 0, total_vaccinations))
DfCUB20 <- DfCUB20 %>% mutate(total_vaccinations_per_hundred =if_else(is.na(total_vaccinations_per_hundred), 0, total_vaccinations_per_hundred))

apply(DfCUB[365:729,], MARGIN = 2, function(x) sum(is.na(x))) %>% View()
DfCUB21 <- data.frame(DfCUB[365:729,])
DfCUB21 <- DfCUB21 %>% mutate(new_vaccinations =if_else(is.na(new_vaccinations), 0, new_vaccinations))
DfCUB21 <- DfCUB21 %>% mutate(total_vaccinations =if_else(is.na(total_vaccinations), 0, total_vaccinations))
DfCUB21 <- DfCUB21 %>% mutate(total_vaccinations_per_hundred =if_else(is.na(total_vaccinations_per_hundred), 0, total_vaccinations_per_hundred))

apply(DfCUB[730:1094,], MARGIN = 2, function(x) sum(is.na(x))) %>% View()
DfCUB22 <- data.frame(DfCUB[730:1094,])
DfCUB22 <- DfCUB22 %>% mutate(new_vaccinations =if_else(is.na(new_vaccinations), 0, new_vaccinations))
DfCUB22 <- DfCUB22 %>% mutate(total_vaccinations =if_else(is.na(total_vaccinations), 0, total_vaccinations))
DfCUB22 <- DfCUB22 %>% mutate(total_vaccinations_per_hundred =if_else(is.na(total_vaccinations_per_hundred), 0, total_vaccinations_per_hundred))

apply(DfCUB [DfCUB$date>"2022-12-31",], MARGIN = 2, function(x) sum(is.na(x))) %>% View()
DfCUB23 <- data.frame(DfCUB[DfCUB$date>"2022-12-31",])
DfCUB23 <- DfCUB23 %>% mutate(total_cases =if_else(is.na(total_cases), 0, total_cases))
DfCUB23 <- DfCUB23 %>% mutate(total_cases_per_million =if_else(is.na(total_cases_per_million), 0, total_cases_per_million))
DfCUB23 <- DfCUB23 %>% mutate(new_cases =if_else(is.na(new_cases), 0, new_cases))
DfCUB23 <- DfCUB23 %>% mutate(new_cases_per_million =if_else(is.na(new_cases_per_million), 0, new_cases_per_million))
DfCUB23 <- DfCUB23 %>% mutate(total_deaths =if_else(is.na(total_deaths), 0, total_deaths))
DfCUB23 <- DfCUB23 %>% mutate(total_deaths_per_million =if_else(is.na(total_deaths_per_million), 0, total_deaths_per_million))
DfCUB23 <- DfCUB23 %>% mutate(new_deaths =if_else(is.na(new_deaths), 0, new_deaths))
DfCUB23 <- DfCUB23 %>% mutate(new_deaths_per_million =if_else(is.na(new_deaths_per_million), 0, new_deaths_per_million))
DfCUB23 <- DfCUB23 %>% mutate(new_vaccinations =if_else(is.na(new_vaccinations), 0, new_vaccinations))
DfCUB23 <- DfCUB23 %>% mutate(total_vaccinations =if_else(is.na(total_vaccinations), 0, total_vaccinations))
DfCUB23 <- DfCUB23 %>% mutate(total_vaccinations_per_hundred =if_else(is.na(total_vaccinations_per_hundred), 0, total_vaccinations_per_hundred))

      # Estadísticas descriptivas  y tasas de DfCHN por año
#Casos
casos_DfCHN20 <- DfCHN20 %>% summarise(Promedio_total_cases=mean(total_cases),
                                       D.Est_total_cases=sd(total_cases),
                                       CV_total_cases=sd(total_cases)/mean(total_cases)*100,
                                       Min_total_cases=min(total_cases),
                                       Max_total_cases=max(total_cases),
                                       Promedio_new_cases=mean(new_cases),
                                       D.Est_new_cases=sd(new_cases),
                                       CV_new_cases=sd(new_cases)/mean(new_cases)*100,
                                       Min_new_cases=min(new_cases),
                                       Max_new_cases=max(new_cases))

Tasa_de_casos_DfCHN20 <-DfCHN20 %>% summarise(Promedio_total_cases=mean(total_cases),
                                              D.Est_total_cases=sd(total_cases),
                                              CV_total_cases=sd(total_cases)/mean(total_cases)*100,
                                              Min_total_cases=min(total_cases),
                                              Max_total_cases=max(total_cases),
                                              Promedio_new_cases=mean(new_cases),
                                              D.Est_new_cases=sd(new_cases),
                                              CV_new_cases=sd(new_cases)/mean(new_cases)*100,
                                              Min_new_cases=min(new_cases),
                                              Max_new_cases=max(new_cases))

casos_DfCHN21 <- DfCHN21 %>% summarise(Promedio_total_cases=mean(total_cases),
                                       D.Est_total_cases=sd(total_cases),
                                       CV_total_cases=sd(total_cases)/mean(total_cases)*100,
                                       Min_total_cases=min(total_cases),
                                       Max_total_cases=max(total_cases),
                                       Promedio_new_cases=mean(new_cases),
                                       D.Est_new_cases=sd(new_cases),
                                       CV_new_cases=sd(new_cases)/mean(new_cases)*100,
                                       Min_new_cases=min(new_cases),
                                       Max_new_cases=max(new_cases))

Tasa_de_casos_DfCHN21<-DfCHN21 %>% summarise(Promedio_total_cases=mean(total_cases),
                                    D.Est_total_cases=sd(total_cases),
                                    CV_total_cases=sd(total_cases)/mean(total_cases)*100,
                                    Min_total_cases=min(total_cases),
                                    Max_total_cases=max(total_cases),
                                    Promedio_new_cases=mean(new_cases),
                                    D.Est_new_cases=sd(new_cases),
                                    CV_new_cases=sd(new_cases)/mean(new_cases)*100,
                                    Min_new_cases=min(new_cases),
                                    Max_new_cases=max(new_cases))

casos_DfCHN22 <- DfCHN22 %>% summarise(Promedio_total_cases=mean(total_cases),
                                       D.Est_total_cases=sd(total_cases),
                                       CV_total_cases=sd(total_cases)/mean(total_cases)*100,
                                       Min_total_cases=min(total_cases),
                                       Max_total_cases=max(total_cases),
                                       Promedio_new_cases=mean(new_cases),
                                       D.Est_new_cases=sd(new_cases),
                                       CV_new_cases=sd(new_cases)/mean(new_cases)*100,
                                       Min_new_cases=min(new_cases),
                                       Max_new_cases=max(new_cases))

Tasa_de_casos_DfCHN22<-DfCHN22 %>% summarise(Promedio_total_cases=mean(total_cases),
                                             D.Est_total_cases=sd(total_cases),
                                             CV_total_cases=sd(total_cases)/mean(total_cases)*100,
                                             Min_total_cases=min(total_cases),
                                             Max_total_cases=max(total_cases),
                                             Promedio_new_cases=mean(new_cases),
                                             D.Est_new_cases=sd(new_cases),
                                             CV_new_cases=sd(new_cases)/mean(new_cases)*100,
                                             Min_new_cases=min(new_cases),
                                             Max_new_cases=max(new_cases))

casos_DfCHN23 <- DfCHN23 %>% summarise(Promedio_total_cases=mean(total_cases),
                                       D.Est_total_cases=sd(total_cases),
                                       CV_total_cases=sd(total_cases)/mean(total_cases)*100,
                                       Min_total_cases=min(total_cases),
                                       Max_total_cases=max(total_cases),
                                       Promedio_new_cases=mean(new_cases),
                                       D.Est_new_cases=sd(new_cases),
                                       CV_new_cases=sd(new_cases)/mean(new_cases)*100,
                                       Min_new_cases=min(new_cases),
                                       Max_new_cases=max(new_cases))

Tasa_de_casos_DfCHN23<- DfCHN23 %>% summarise(Promedio_total_cases=mean(total_cases),
                                              D.Est_total_cases=sd(total_cases),
                                              CV_total_cases=sd(total_cases)/mean(total_cases)*100,
                                              Min_total_cases=min(total_cases),
                                              Max_total_cases=max(total_cases),
                                              Promedio_new_cases=mean(new_cases),
                                              D.Est_new_cases=sd(new_cases),
                                              CV_new_cases=sd(new_cases)/mean(new_cases)*100,
                                              Min_new_cases=min(new_cases),
                                              Max_new_cases=max(new_cases))

      #Decesos
Decesos_DfCHN20 <- DfCHN20 %>% summarise(Promedio_total_deaths=mean(total_deaths),
                                         D.Est_total_deaths=sd(total_deaths),
                                         CV_total_deaths=sd(total_deaths)/mean(total_deaths)*100,
                                         Min_total_deaths=min(total_deaths),
                                         Max_total_deaths=max(total_deaths),
                                         Promedio_new_deaths=mean(new_deaths),
                                         D.Est_new_deaths=sd(new_deaths),
                                         CV_new_deaths=sd(new_deaths)/mean(new_deaths)*100,
                                         Min_new_deaths=min(new_deaths),
                                         Max_new_deaths=max(new_deaths))

Tasa_de_decesos_DfCHN20<-DfCHN20 %>% summarise(Promedio_total_deaths=mean(total_deaths_per_million),
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

Decesos_DfCHN21 <- DfCHN21 %>% summarise(Promedio_total_deaths=mean(total_deaths),
                                         D.Est_total_deaths=sd(total_deaths),
                                         CV_total_deaths=sd(total_deaths)/mean(total_deaths)*100,
                                         Min_total_deaths=min(total_deaths),
                                         Max_total_deaths=max(total_deaths),
                                         Promedio_new_deaths=mean(new_deaths),
                                         D.Est_new_deaths=sd(new_deaths),
                                         CV_new_deaths=sd(new_deaths)/mean(new_deaths)*100,
                                         Min_new_deaths=min(new_deaths),
                                         Max_new_deaths=max(new_deaths))

Tasa_de_decesos_DfCHN21<- DfCHN21 %>% summarise(Promedio_total_deaths=mean(total_deaths_per_million),
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

Decesos_DfCHN22 <- DfCHN22 %>% summarise(Promedio_total_deaths=mean(total_deaths),
                                         D.Est_total_deaths=sd(total_deaths),
                                         CV_total_deaths=sd(total_deaths)/mean(total_deaths)*100,
                                         Min_total_deaths=min(total_deaths),
                                         Max_total_deaths=max(total_deaths),
                                         Promedio_new_deaths=mean(new_deaths),
                                         D.Est_new_deaths=sd(new_deaths),
                                         CV_new_deaths=sd(new_deaths)/mean(new_deaths)*100,
                                         Min_new_deaths=min(new_deaths),
                                         Max_new_deaths=max(new_deaths))

Tasa_de_decesos_DfCHN22<-DfCHN22 %>% summarise(Promedio_total_deaths=mean(total_deaths_per_million),
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


Decesos_DfCHN23 <- DfCHN23 %>% summarise(Promedio_total_deaths=mean(total_deaths),
                                         D.Est_total_deaths=sd(total_deaths),
                                         CV_total_deaths=sd(total_deaths)/mean(total_deaths)*100,
                                         Min_total_deaths=min(total_deaths),
                                         Max_total_deaths=max(total_deaths),
                                         Promedio_new_deaths=mean(new_deaths),
                                         D.Est_new_deaths=sd(new_deaths),
                                         CV_new_deaths=sd(new_deaths)/mean(new_deaths)*100,
                                         Min_new_deaths=min(new_deaths),
                                         Max_new_deaths=max(new_deaths))

Tasa_de_decesos_DfCHN23<-DfCHN23 %>% summarise(Promedio_total_deaths=mean(total_deaths_per_million),
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

#Vacunaciones

Vacunas_DfCHN20 <- DfCHN20 %>% summarise(Promedio_total_vaccinations=mean(total_vaccinations),
                                         D.Est_total_vaccinations=sd(total_vaccinations),
                                         CV_total_vaccinations=sd(total_vaccinations)/mean(total_vaccinations)*100,
                                         Min_total_vaccinations=min(total_vaccinations),
                                         Max_total_vaccinations=max(total_vaccinations),
                                         Promedio_new_vaccinations=mean(new_vaccinations),
                                         D.Est_new_vaccinations=sd(new_vaccinations),
                                         CV_new_vaccinations=sd(new_vaccinations)/mean(new_vaccinations)*100,
                                         Min_new_vaccinations=min(new_vaccinations),
                                         Max_new_vaccinations=max(new_vaccinations))

Tasa_de_vacunas_DfCHN20<-DfCHN20 %>% summarise(Promedio_total_vaccinations=mean(total_vaccinations),
                                               D.Est_total_vaccinations=sd(total_vaccinations),
                                               CV_total_vaccinations=sd(total_vaccinations)/mean(total_vaccinations)*100,
                                               Min_total_vaccinations=min(total_vaccinations),
                                               Max_total_vaccinations=max(total_vaccinations),
                                               Promedio_new_vaccinations=mean(new_vaccinations),
                                               D.Est_new_vaccinations=sd(new_vaccinations),
                                               CV_new_vaccinations=sd(new_vaccinations)/mean(new_vaccinations)*100,
                                               Min_new_vaccinations=min(new_vaccinations),
                                               Max_new_vaccinations=max(new_vaccinations))

Vacunas_DfCHN21 <- DfCHN21 %>% summarise(Promedio_total_vaccinations=mean(total_vaccinations),
                                         D.Est_total_vaccinations=sd(total_vaccinations),
                                         CV_total_vaccinations=sd(total_vaccinations)/mean(total_vaccinations)*100,
                                         Min_total_vaccinations=min(total_vaccinations),
                                         Max_total_vaccinations=max(total_vaccinations),
                                         Promedio_new_vaccinations=mean(new_vaccinations),
                                         D.Est_new_vaccinations=sd(new_vaccinations),
                                         CV_new_vaccinations=sd(new_vaccinations)/mean(new_vaccinations)*100,
                                         Min_new_vaccinations=min(new_vaccinations),
                                         Max_new_vaccinations=max(new_vaccinations))

Tasa_de_vacunas_DfCHN21<-DfCHN21 %>% summarise(Promedio_total_vaccinations=mean(total_vaccinations),
                                               D.Est_total_vaccinations=sd(total_vaccinations),
                                               CV_total_vaccinations=sd(total_vaccinations)/mean(total_vaccinations)*100,
                                               Min_total_vaccinations=min(total_vaccinations),
                                               Max_total_vaccinations=max(total_vaccinations),
                                               Promedio_new_vaccinations=mean(new_vaccinations),
                                               D.Est_new_vaccinations=sd(new_vaccinations),
                                               CV_new_vaccinations=sd(new_vaccinations)/mean(new_vaccinations)*100,
                                               Min_new_vaccinations=min(new_vaccinations),
                                               Max_new_vaccinations=max(new_vaccinations))

Vacunas_DfCHN22 <- DfCHN22 %>% summarise(Promedio_total_vaccinations=mean(total_vaccinations),
                                         D.Est_total_vaccinations=sd(total_vaccinations),
                                         CV_total_vaccinations=sd(total_vaccinations)/mean(total_vaccinations)*100,
                                         Min_total_vaccinations=min(total_vaccinations),
                                         Max_total_vaccinations=max(total_vaccinations),
                                         Promedio_new_vaccinations=mean(new_vaccinations),
                                         D.Est_new_vaccinations=sd(new_vaccinations),
                                         CV_new_vaccinations=sd(new_vaccinations)/mean(new_vaccinations)*100,
                                         Min_new_vaccinations=min(new_vaccinations),
                                         Max_new_vaccinations=max(new_vaccinations))

Tasa_de_vacunas_DfCHN22<-DfCHN22 %>% summarise(Promedio_total_vaccinations=mean(total_vaccinations),
                                               D.Est_total_vaccinations=sd(total_vaccinations),
                                               CV_total_vaccinations=sd(total_vaccinations)/mean(total_vaccinations)*100,
                                               Min_total_vaccinations=min(total_vaccinations),
                                               Max_total_vaccinations=max(total_vaccinations),
                                               Promedio_new_vaccinations=mean(new_vaccinations),
                                               D.Est_new_vaccinations=sd(new_vaccinations),
                                               CV_new_vaccinations=sd(new_vaccinations)/mean(new_vaccinations)*100,
                                               Min_new_vaccinations=min(new_vaccinations),
                                               Max_new_vaccinations=max(new_vaccinations))

Vacunas_DfCHN23 <- DfCHN23 %>% summarise(Promedio_total_vaccinations=mean(total_vaccinations),
                                         D.Est_total_vaccinations=sd(total_vaccinations),
                                         CV_total_vaccinations=sd(total_vaccinations)/mean(total_vaccinations)*100,
                                         Min_total_vaccinations=min(total_vaccinations),
                                         Max_total_vaccinations=max(total_vaccinations),
                                         Promedio_new_vaccinations=mean(new_vaccinations),
                                         D.Est_new_vaccinations=sd(new_vaccinations),
                                         CV_new_vaccinations=sd(new_vaccinations)/mean(new_vaccinations)*100,
                                         Min_new_vaccinations=min(new_vaccinations),
                                         Max_new_vaccinations=max(new_vaccinations))

Tasa_de_vacunas_DfCHN23<-DfCHN23 %>% summarise(Promedio_total_vaccinations=mean(total_vaccinations),
                                               D.Est_total_vaccinations=sd(total_vaccinations),
                                               CV_total_vaccinations=sd(total_vaccinations)/mean(total_vaccinations)*100,
                                               Min_total_vaccinations=min(total_vaccinations),
                                               Max_total_vaccinations=max(total_vaccinations),
                                               Promedio_new_vaccinations=mean(new_vaccinations),
                                               D.Est_new_vaccinations=sd(new_vaccinations),
                                               CV_new_vaccinations=sd(new_vaccinations)/mean(new_vaccinations)*100,
                                               Min_new_vaccinations=min(new_vaccinations),
                                               Max_new_vaccinations=max(new_vaccinations))

# Estadísticas descriptivas  y tasas de DfRUS por año
#Casos

Casos_DfRUS20 <- DfRUS20 %>% summarise(Promedio_total_cases=mean(total_cases),
                                       D.Est_total_cases=sd(total_cases),
                                       CV_total_cases=sd(total_cases)/mean(total_cases)*100,
                                       Min_total_cases=min(total_cases),
                                       Max_total_cases=max(total_cases),
                                       Promedio_new_cases=mean(new_cases),
                                       D.Est_new_cases=sd(new_cases),
                                       CV_new_cases=sd(new_cases)/mean(new_cases)*100,
                                       Min_new_cases=min(new_cases),
                                       Max_new_cases=max(new_cases))

Tasa_de_casos_DfRUS20<-DfRUS20 %>% summarise(Promedio_total_cases=mean(total_cases),
                                             D.Est_total_cases=sd(total_cases),
                                             CV_total_cases=sd(total_cases)/mean(total_cases)*100,
                                             Min_total_cases=min(total_cases),
                                             Max_total_cases=max(total_cases),
                                             Promedio_new_cases=mean(new_cases),
                                             D.Est_new_cases=sd(new_cases),
                                             CV_new_cases=sd(new_cases)/mean(new_cases)*100,
                                             Min_new_cases=min(new_cases),
                                             Max_new_cases=max(new_cases))

Casos_DfRUS21 <- DfRUS21 %>% summarise(Promedio_total_cases=mean(total_cases),
                                       D.Est_total_cases=sd(total_cases),
                                       CV_total_cases=sd(total_cases)/mean(total_cases)*100,
                                       Min_total_cases=min(total_cases),
                                       Max_total_cases=max(total_cases),
                                       Promedio_new_cases=mean(new_cases),
                                       D.Est_new_cases=sd(new_cases),
                                       CV_new_cases=sd(new_cases)/mean(new_cases)*100,
                                       Min_new_cases=min(new_cases),
                                       Max_new_cases=max(new_cases))

Tasa_de_casos_DfRUS21<-DfRUS21 %>% summarise(Promedio_total_cases=mean(total_cases),
                                             D.Est_total_cases=sd(total_cases),
                                             CV_total_cases=sd(total_cases)/mean(total_cases)*100,
                                             Min_total_cases=min(total_cases),
                                             Max_total_cases=max(total_cases),
                                             Promedio_new_cases=mean(new_cases),
                                             D.Est_new_cases=sd(new_cases),
                                             CV_new_cases=sd(new_cases)/mean(new_cases)*100,
                                             Min_new_cases=min(new_cases),
                                             Max_new_cases=max(new_cases))

Casos_DfRUS22 <- DfRUS22 %>% summarise(Promedio_total_cases=mean(total_cases),
                                       D.Est_total_cases=sd(total_cases),
                                       CV_total_cases=sd(total_cases)/mean(total_cases)*100,
                                       Min_total_cases=min(total_cases),
                                       Max_total_cases=max(total_cases),
                                       Promedio_new_cases=mean(new_cases),
                                       D.Est_new_cases=sd(new_cases),
                                       CV_new_cases=sd(new_cases)/mean(new_cases)*100,
                                       Min_new_cases=min(new_cases),
                                       Max_new_cases=max(new_cases))

Tasa_de_casos_DfRUS22<-DfRUS22 %>% summarise(Promedio_total_cases=mean(total_cases),
                                             D.Est_total_cases=sd(total_cases),
                                             CV_total_cases=sd(total_cases)/mean(total_cases)*100,
                                             Min_total_cases=min(total_cases),
                                             Max_total_cases=max(total_cases),
                                             Promedio_new_cases=mean(new_cases),
                                             D.Est_new_cases=sd(new_cases),
                                             CV_new_cases=sd(new_cases)/mean(new_cases)*100,
                                             Min_new_cases=min(new_cases),
                                             Max_new_cases=max(new_cases))

Casos_DfRUS23 <- DfRUS23 %>% summarise(Promedio_total_cases=mean(total_cases),
                                       D.Est_total_cases=sd(total_cases),
                                       CV_total_cases=sd(total_cases)/mean(total_cases)*100,
                                       Min_total_cases=min(total_cases),
                                       Max_total_cases=max(total_cases),
                                       Promedio_new_cases=mean(new_cases),
                                       D.Est_new_cases=sd(new_cases),
                                       CV_new_cases=sd(new_cases)/mean(new_cases)*100,
                                       Min_new_cases=min(new_cases),
                                       Max_new_cases=max(new_cases))

Tasa_de_casos_DfRUS23<-DfRUS23 %>% summarise(Promedio_total_cases=mean(total_cases),
                                             D.Est_total_cases=sd(total_cases),
                                             CV_total_cases=sd(total_cases)/mean(total_cases)*100,
                                             Min_total_cases=min(total_cases),
                                             Max_total_cases=max(total_cases),
                                             Promedio_new_cases=mean(new_cases),
                                             D.Est_new_cases=sd(new_cases),
                                             CV_new_cases=sd(new_cases)/mean(new_cases)*100,
                                             Min_new_cases=min(new_cases),
                                             Max_new_cases=max(new_cases))
#Vacunados

Vacunas_DfRUS20 <- DfRUS20 %>% summarise(Promedio_total_vaccinations=mean(total_vaccinations),
                                         D.Est_total_vaccinations=sd(total_vaccinations),
                                         CV_total_vaccinations=sd(total_vaccinations)/mean(total_vaccinations)*100,
                                         Min_total_vaccinations=min(total_vaccinations),
                                         Max_total_vaccinations=max(total_vaccinations),
                                         Promedio_new_vaccinations=mean(new_vaccinations),
                                         D.Est_new_vaccinations=sd(new_vaccinations),
                                         CV_new_vaccinations=sd(new_vaccinations)/mean(new_vaccinations)*100,
                                         Min_new_vaccinations=min(new_vaccinations),
                                         Max_new_vaccinations=max(new_vaccinations))

Tasa_de_vacunas_DfRUS20<-DfRUS20 %>% summarise(Promedio_total_vaccinations=mean(total_vaccinations),
                                               D.Est_total_vaccinations=sd(total_vaccinations),
                                               CV_total_vaccinations=sd(total_vaccinations)/mean(total_vaccinations)*100,
                                               Min_total_vaccinations=min(total_vaccinations),
                                               Max_total_vaccinations=max(total_vaccinations),
                                               Promedio_new_vaccinations=mean(new_vaccinations),
                                               D.Est_new_vaccinations=sd(new_vaccinations),
                                               CV_new_vaccinations=sd(new_vaccinations)/mean(new_vaccinations)*100,
                                               Min_new_vaccinations=min(new_vaccinations),
                                               Max_new_vaccinations=max(new_vaccinations))

Vacunas_DfRUS21 <- DfRUS21 %>% summarise(Promedio_total_vaccinations=mean(total_vaccinations),
                                         D.Est_total_vaccinations=sd(total_vaccinations),
                                         CV_total_vaccinations=sd(total_vaccinations)/mean(total_vaccinations)*100,
                                         Min_total_vaccinations=min(total_vaccinations),
                                         Max_total_vaccinations=max(total_vaccinations),
                                         Promedio_new_vaccinations=mean(new_vaccinations),
                                         D.Est_new_vaccinations=sd(new_vaccinations),
                                         CV_new_vaccinations=sd(new_vaccinations)/mean(new_vaccinations)*100,
                                         Min_new_vaccinations=min(new_vaccinations),
                                         Max_new_vaccinations=max(new_vaccinations))

Tasa_de_vacunas_DfRUS21<-DfRUS21 %>% summarise(Promedio_total_vaccinations=mean(total_vaccinations),
                                               D.Est_total_vaccinations=sd(total_vaccinations),
                                               CV_total_vaccinations=sd(total_vaccinations)/mean(total_vaccinations)*100,
                                               Min_total_vaccinations=min(total_vaccinations),
                                               Max_total_vaccinations=max(total_vaccinations),
                                               Promedio_new_vaccinations=mean(new_vaccinations),
                                               D.Est_new_vaccinations=sd(new_vaccinations),
                                               CV_new_vaccinations=sd(new_vaccinations)/mean(new_vaccinations)*100,
                                               Min_new_vaccinations=min(new_vaccinations),
                                               Max_new_vaccinations=max(new_vaccinations))

Vacunas_DfRUS22 <- DfRUS22 %>% summarise(Promedio_total_vaccinations=mean(total_vaccinations),
                                         D.Est_total_vaccinations=sd(total_vaccinations),
                                         CV_total_vaccinations=sd(total_vaccinations)/mean(total_vaccinations)*100,
                                         Min_total_vaccinations=min(total_vaccinations),
                                         Max_total_vaccinations=max(total_vaccinations),
                                         Promedio_new_vaccinations=mean(new_vaccinations),
                                         D.Est_new_vaccinations=sd(new_vaccinations),
                                         CV_new_vaccinations=sd(new_vaccinations)/mean(new_vaccinations)*100,
                                         Min_new_vaccinations=min(new_vaccinations),
                                         Max_new_vaccinations=max(new_vaccinations))

Tasa_de_vacunas_DfRUS22<-DfRUS22 %>% summarise(Promedio_total_vaccinations=mean(total_vaccinations),
                                               D.Est_total_vaccinations=sd(total_vaccinations),
                                               CV_total_vaccinations=sd(total_vaccinations)/mean(total_vaccinations)*100,
                                               Min_total_vaccinations=min(total_vaccinations),
                                               Max_total_vaccinations=max(total_vaccinations),
                                               Promedio_new_vaccinations=mean(new_vaccinations),
                                               D.Est_new_vaccinations=sd(new_vaccinations),
                                               CV_new_vaccinations=sd(new_vaccinations)/mean(new_vaccinations)*100,
                                               Min_new_vaccinations=min(new_vaccinations),
                                               Max_new_vaccinations=max(new_vaccinations))

Vacunas_DfRUS23 <- DfRUS23 %>% summarise(Promedio_total_vaccinations=mean(total_vaccinations),
                                         D.Est_total_vaccinations=sd(total_vaccinations),
                                         CV_total_vaccinations=sd(total_vaccinations)/mean(total_vaccinations)*100,
                                         Min_total_vaccinations=min(total_vaccinations),
                                         Max_total_vaccinations=max(total_vaccinations),
                                         Promedio_new_vaccinations=mean(new_vaccinations),
                                         D.Est_new_vaccinations=sd(new_vaccinations),
                                         CV_new_vaccinations=sd(new_vaccinations)/mean(new_vaccinations)*100,
                                         Min_new_vaccinations=min(new_vaccinations),
                                         Max_new_vaccinations=max(new_vaccinations))

Tasa_de_vacunas_DfRUS23<-DfRUS23 %>% summarise(Promedio_total_vaccinations=mean(total_vaccinations),
                                               D.Est_total_vaccinations=sd(total_vaccinations),
                                               CV_total_vaccinations=sd(total_vaccinations)/mean(total_vaccinations)*100,
                                               Min_total_vaccinations=min(total_vaccinations),
                                               Max_total_vaccinations=max(total_vaccinations),
                                               Promedio_new_vaccinations=mean(new_vaccinations),
                                               D.Est_new_vaccinations=sd(new_vaccinations),
                                               CV_new_vaccinations=sd(new_vaccinations)/mean(new_vaccinations)*100,
                                               Min_new_vaccinations=min(new_vaccinations),
                                               Max_new_vaccinations=max(new_vaccinations))

#Decesos
Decesos_DfRUS20 <- DfRUS20 %>% summarise(Promedio_total_deaths=mean(total_deaths),
                                        D.Est_total_deaths=sd(total_deaths),
                                        CV_total_deaths=sd(total_deaths)/mean(total_deaths)*100,
                                        Min_total_deaths=min(total_deaths),
                                        Max_total_deaths=max(total_deaths),
                                        Promedio_new_deaths=mean(new_deaths),
                                        D.Est_new_deaths=sd(new_deaths),
                                        CV_new_deaths=sd(new_deaths)/mean(new_deaths)*100,
                                        Min_new_deaths=min(new_deaths),
                                        Max_new_deaths=max(new_deaths))

Tasas_de_decesos_DfRUS20 <- DfRUS20 %>% summarise(Promedio_total_deaths=mean(total_deaths_per_million),
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

Decesos_DfRUS21<-DfRUS21 %>% summarise(Promedio_total_deaths=mean(total_deaths),
                                       D.Est_total_deaths=sd(total_deaths),
                                       CV_total_deaths=sd(total_deaths)/mean(total_deaths)*100,
                                       Min_total_deaths=min(total_deaths),
                                       Max_total_deaths=max(total_deaths),
                                       Promedio_new_deaths=mean(new_deaths),
                                       D.Est_new_deaths=sd(new_deaths),
                                       CV_new_deaths=sd(new_deaths)/mean(new_deaths)*100,
                                       Min_new_deaths=min(new_deaths),
                                       Max_new_deaths=max(new_deaths))

Tasa_de_decesos_DfRUS21<-DfRUS21 %>% summarise(Promedio_total_deaths=mean(total_deaths_per_million),
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

Decesos_DfRUS22 <- DfRUS22 %>% summarise(Promedio_total_deaths=mean(total_deaths),
                                         D.Est_total_deaths=sd(total_deaths),
                                         CV_total_deaths=sd(total_deaths)/mean(total_deaths)*100,
                                         Min_total_deaths=min(total_deaths),
                                         Max_total_deaths=max(total_deaths),
                                         Promedio_new_deaths=mean(new_deaths),
                                         D.Est_new_deaths=sd(new_deaths),
                                         CV_new_deaths=sd(new_deaths)/mean(new_deaths)*100,
                                         Min_new_deaths=min(new_deaths),
                                         Max_new_deaths=max(new_deaths))

Tasa_de_decesos_DfRUS22<-DfRUS22 %>% summarise(Promedio_total_deaths=mean(total_deaths_per_million),
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

Decesos_DfRUS23<-DfRUS23 %>% summarise(Promedio_total_deaths=mean(total_deaths),
                                       D.Est_total_deaths=sd(total_deaths),
                                       CV_total_deaths=sd(total_deaths)/mean(total_deaths)*100,
                                       Min_total_deaths=min(total_deaths),
                                       Max_total_deaths=max(total_deaths),
                                       Promedio_new_deaths=mean(new_deaths),
                                       D.Est_new_deaths=sd(new_deaths),
                                       CV_new_deaths=sd(new_deaths)/mean(new_deaths)*100,
                                       Min_new_deaths=min(new_deaths),
                                       Max_new_deaths=max(new_deaths))

Tasa_de_decesos_DfRUS23<-DfRUS23 %>% summarise(Promedio_total_deaths=mean(total_deaths_per_million),
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

# Estadísticas descriptivas  y tasas de DfIRN por año
#Casos

Casos_DfIRN20 <- DfIRN20 %>% summarise(Promedio_total_cases=mean(total_cases),
                                       D.Est_total_cases=sd(total_cases),
                                       CV_total_cases=sd(total_cases)/mean(total_cases)*100,
                                       Min_total_cases=min(total_cases),
                                       Max_total_cases=max(total_cases),
                                       Promedio_new_cases=mean(new_cases),
                                       D.Est_new_cases=sd(new_cases),
                                       CV_new_cases=sd(new_cases)/mean(new_cases)*100,
                                       Min_new_cases=min(new_cases),
                                       Max_new_cases=max(new_cases))

Tasa_de_casos_DfIRN20<-DfIRN20 %>% summarise(Promedio_total_cases=mean(total_cases),
                                             D.Est_total_cases=sd(total_cases),
                                             CV_total_cases=sd(total_cases)/mean(total_cases)*100,
                                             Min_total_cases=min(total_cases),
                                             Max_total_cases=max(total_cases),
                                             Promedio_new_cases=mean(new_cases),
                                             D.Est_new_cases=sd(new_cases),
                                             CV_new_cases=sd(new_cases)/mean(new_cases)*100,
                                             Min_new_cases=min(new_cases),
                                             Max_new_cases=max(new_cases))

Casos_DfIRN21 <- DfIRN21 %>% summarise(Promedio_total_cases=mean(total_cases),
                                       D.Est_total_cases=sd(total_cases),
                                       CV_total_cases=sd(total_cases)/mean(total_cases)*100,
                                       Min_total_cases=min(total_cases),
                                       Max_total_cases=max(total_cases),
                                       Promedio_new_cases=mean(new_cases),
                                       D.Est_new_cases=sd(new_cases),
                                       CV_new_cases=sd(new_cases)/mean(new_cases)*100,
                                       Min_new_cases=min(new_cases),
                                       Max_new_cases=max(new_cases))

Tasa_de_casos_DfIRN21<-DfIRN21 %>% summarise(Promedio_total_cases=mean(total_cases),
                                             D.Est_total_cases=sd(total_cases),
                                             CV_total_cases=sd(total_cases)/mean(total_cases)*100,
                                             Min_total_cases=min(total_cases),
                                             Max_total_cases=max(total_cases),
                                             Promedio_new_cases=mean(new_cases),
                                             D.Est_new_cases=sd(new_cases),
                                             CV_new_cases=sd(new_cases)/mean(new_cases)*100,
                                             Min_new_cases=min(new_cases),
                                             Max_new_cases=max(new_cases))

Casos_DfIRN22 <- DfIRN22 %>% summarise(Promedio_total_cases=mean(total_cases),
                                       D.Est_total_cases=sd(total_cases),
                                       CV_total_cases=sd(total_cases)/mean(total_cases)*100,
                                       Min_total_cases=min(total_cases),
                                       Max_total_cases=max(total_cases),
                                       Promedio_new_cases=mean(new_cases),
                                       D.Est_new_cases=sd(new_cases),
                                       CV_new_cases=sd(new_cases)/mean(new_cases)*100,
                                       Min_new_cases=min(new_cases),
                                       Max_new_cases=max(new_cases))

Tasa_de_casos_DfIRN22<-DfIRN22 %>% summarise(Promedio_total_cases=mean(total_cases),
                                    D.Est_total_cases=sd(total_cases),
                                    CV_total_cases=sd(total_cases)/mean(total_cases)*100,
                                    Min_total_cases=min(total_cases),
                                    Max_total_cases=max(total_cases),
                                    Promedio_new_cases=mean(new_cases),
                                    D.Est_new_cases=sd(new_cases),
                                    CV_new_cases=sd(new_cases)/mean(new_cases)*100,
                                    Min_new_cases=min(new_cases),
                                    Max_new_cases=max(new_cases))

Casos_DfIRN23 <- DfIRN23 %>% summarise(Promedio_total_cases=mean(total_cases),
                                       D.Est_total_cases=sd(total_cases),
                                       CV_total_cases=sd(total_cases)/mean(total_cases)*100,
                                       Min_total_cases=min(total_cases),
                                       Max_total_cases=max(total_cases),
                                       Promedio_new_cases=mean(new_cases),
                                       D.Est_new_cases=sd(new_cases),
                                       CV_new_cases=sd(new_cases)/mean(new_cases)*100,
                                       Min_new_cases=min(new_cases),
                                       Max_new_cases=max(new_cases))

Tasa_de_casos_DfIRN23<-DfIRN23 %>% summarise(Promedio_total_cases=mean(total_cases),
                                             D.Est_total_cases=sd(total_cases),
                                             CV_total_cases=sd(total_cases)/mean(total_cases)*100,
                                             Min_total_cases=min(total_cases),
                                             Max_total_cases=max(total_cases),
                                             Promedio_new_cases=mean(new_cases),
                                             D.Est_new_cases=sd(new_cases),
                                             CV_new_cases=sd(new_cases)/mean(new_cases)*100,
                                             Min_new_cases=min(new_cases),
                                             Max_new_cases=max(new_cases))

#Vacunados

Vacunas_DfIRN20 <- DfIRN20 %>% summarise(Promedio_total_vaccinations=mean(total_vaccinations),
                                         D.Est_total_vaccinations=sd(total_vaccinations),
                                         CV_total_vaccinations=sd(total_vaccinations)/mean(total_vaccinations)*100,
                                         Min_total_vaccinations=min(total_vaccinations),
                                         Max_total_vaccinations=max(total_vaccinations),
                                         Promedio_new_vaccinations=mean(new_vaccinations),
                                         D.Est_new_vaccinations=sd(new_vaccinations),
                                         CV_new_vaccinations=sd(new_vaccinations)/mean(new_vaccinations)*100,
                                         Min_new_vaccinations=min(new_vaccinations),
                                         Max_new_vaccinations=max(new_vaccinations))

Tasa_de_vacunas_DfIRN20 <- DfIRN20 %>% summarise(Promedio_total_vaccinations=mean(total_vaccinations),
                                                 D.Est_total_vaccinations=sd(total_vaccinations),
                                                 CV_total_vaccinations=sd(total_vaccinations)/mean(total_vaccinations)*100,
                                                 Min_total_vaccinations=min(total_vaccinations),
                                                 Max_total_vaccinations=max(total_vaccinations),
                                                 Promedio_new_vaccinations=mean(new_vaccinations),
                                                 D.Est_new_vaccinations=sd(new_vaccinations),
                                                 CV_new_vaccinations=sd(new_vaccinations)/mean(new_vaccinations)*100,
                                                 Min_new_vaccinations=min(new_vaccinations),
                                                 Max_new_vaccinations=max(new_vaccinations))

Vacunas_DfIRN21 <- DfIRN21 %>% summarise(Promedio_total_vaccinations=mean(total_vaccinations),
                                         D.Est_total_vaccinations=sd(total_vaccinations),
                                         CV_total_vaccinations=sd(total_vaccinations)/mean(total_vaccinations)*100,
                                         Min_total_vaccinations=min(total_vaccinations),
                                         Max_total_vaccinations=max(total_vaccinations),
                                         Promedio_new_vaccinations=mean(new_vaccinations),
                                         D.Est_new_vaccinations=sd(new_vaccinations),
                                         CV_new_vaccinations=sd(new_vaccinations)/mean(new_vaccinations)*100,
                                         Min_new_vaccinations=min(new_vaccinations),
                                         Max_new_vaccinations=max(new_vaccinations))

Tasa_de_vacunas_DfIRN21<-DfIRN21 %>% summarise(Promedio_total_vaccinations=mean(total_vaccinations),
                                               D.Est_total_vaccinations=sd(total_vaccinations),
                                               CV_total_vaccinations=sd(total_vaccinations)/mean(total_vaccinations)*100,
                                               Min_total_vaccinations=min(total_vaccinations),
                                               Max_total_vaccinations=max(total_vaccinations),
                                               Promedio_new_vaccinations=mean(new_vaccinations),
                                               D.Est_new_vaccinations=sd(new_vaccinations),
                                               CV_new_vaccinations=sd(new_vaccinations)/mean(new_vaccinations)*100,
                                               Min_new_vaccinations=min(new_vaccinations),
                                               Max_new_vaccinations=max(new_vaccinations))

Vacunas_DfIRN22 <- DfIRN22 %>% summarise(Promedio_total_vaccinations=mean(total_vaccinations),
                                         D.Est_total_vaccinations=sd(total_vaccinations),
                                         CV_total_vaccinations=sd(total_vaccinations)/mean(total_vaccinations)*100,
                                         Min_total_vaccinations=min(total_vaccinations),
                                         Max_total_vaccinations=max(total_vaccinations),
                                         Promedio_new_vaccinations=mean(new_vaccinations),
                                         D.Est_new_vaccinations=sd(new_vaccinations),
                                         CV_new_vaccinations=sd(new_vaccinations)/mean(new_vaccinations)*100,
                                         Min_new_vaccinations=min(new_vaccinations),
                                         Max_new_vaccinations=max(new_vaccinations))

Tasa_de_vacunas_DfIRN22<-DfIRN22 %>% summarise(Promedio_total_vaccinations=mean(total_vaccinations),
                                               D.Est_total_vaccinations=sd(total_vaccinations),
                                               CV_total_vaccinations=sd(total_vaccinations)/mean(total_vaccinations)*100,
                                               Min_total_vaccinations=min(total_vaccinations),
                                               Max_total_vaccinations=max(total_vaccinations),
                                               Promedio_new_vaccinations=mean(new_vaccinations),
                                               D.Est_new_vaccinations=sd(new_vaccinations),
                                               CV_new_vaccinations=sd(new_vaccinations)/mean(new_vaccinations)*100,
                                               Min_new_vaccinations=min(new_vaccinations),
                                               Max_new_vaccinations=max(new_vaccinations))

Vacunas_DfIRN23 <- DfIRN23 %>% summarise(Promedio_total_vaccinations=mean(total_vaccinations),
                                         D.Est_total_vaccinations=sd(total_vaccinations),
                                         CV_total_vaccinations=sd(total_vaccinations)/mean(total_vaccinations)*100,
                                         Min_total_vaccinations=min(total_vaccinations),
                                         Max_total_vaccinations=max(total_vaccinations),
                                         Promedio_new_vaccinations=mean(new_vaccinations),
                                         D.Est_new_vaccinations=sd(new_vaccinations),
                                         CV_new_vaccinations=sd(new_vaccinations)/mean(new_vaccinations)*100,
                                         Min_new_vaccinations=min(new_vaccinations),
                                         Max_new_vaccinations=max(new_vaccinations))

Tasa_de_vacunas_DfIRN23<-DfIRN23 %>% summarise(Promedio_total_vaccinations=mean(total_vaccinations),
                                               D.Est_total_vaccinations=sd(total_vaccinations),
                                               CV_total_vaccinations=sd(total_vaccinations)/mean(total_vaccinations)*100,
                                               Min_total_vaccinations=min(total_vaccinations),
                                               Max_total_vaccinations=max(total_vaccinations),
                                               Promedio_new_vaccinations=mean(new_vaccinations),
                                               D.Est_new_vaccinations=sd(new_vaccinations),
                                               CV_new_vaccinations=sd(new_vaccinations)/mean(new_vaccinations)*100,
                                               Min_new_vaccinations=min(new_vaccinations),
                                               Max_new_vaccinations=max(new_vaccinations))

#Decesos

Decesos_DfIRN20 <- DfIRN20 %>% summarise(Promedio_total_deaths=mean(total_deaths),
                                         D.Est_total_deaths=sd(total_deaths),
                                         CV_total_deaths=sd(total_deaths)/mean(total_deaths)*100,
                                         Min_total_deaths=min(total_deaths),
                                         Max_total_deaths=max(total_deaths),
                                         Promedio_new_deaths=mean(new_deaths),
                                         D.Est_new_deaths=sd(new_deaths),
                                         CV_new_deaths=sd(new_deaths)/mean(new_deaths)*100,
                                         Min_new_deaths=min(new_deaths),
                                         Max_new_deaths=max(new_deaths))

Tasa_de_decesos_DfIRN20<-DfIRN20 %>% summarise(Promedio_total_deaths=mean(total_deaths_per_million),
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

Decesos_DfIRN21 <- DfIRN21 %>% summarise(Promedio_total_deaths=mean(total_deaths),
                                         D.Est_total_deaths=sd(total_deaths),
                                         CV_total_deaths=sd(total_deaths)/mean(total_deaths)*100,
                                         Min_total_deaths=min(total_deaths),
                                         Max_total_deaths=max(total_deaths),
                                         Promedio_new_deaths=mean(new_deaths),
                                         D.Est_new_deaths=sd(new_deaths),
                                         CV_new_deaths=sd(new_deaths)/mean(new_deaths)*100,
                                         Min_new_deaths=min(new_deaths),
                                         Max_new_deaths=max(new_deaths))

Tasa_de_decesos_DfIRN21<-DfIRN21 %>% summarise(Promedio_total_deaths=mean(total_deaths_per_million),
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

Decesos_DfIRN22 <- DfIRN22 %>% summarise(Promedio_total_deaths=mean(total_deaths),
                                         D.Est_total_deaths=sd(total_deaths),
                                         CV_total_deaths=sd(total_deaths)/mean(total_deaths)*100,
                                         Min_total_deaths=min(total_deaths),
                                         Max_total_deaths=max(total_deaths),
                                         Promedio_new_deaths=mean(new_deaths),
                                         D.Est_new_deaths=sd(new_deaths),
                                         CV_new_deaths=sd(new_deaths)/mean(new_deaths)*100,
                                         Min_new_deaths=min(new_deaths),
                                         Max_new_deaths=max(new_deaths))

Tasa_de_decesos_DfIRN22<-DfIRN22 %>% summarise(Promedio_total_deaths=mean(total_deaths_per_million),
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

Decesos_DfIRN23 <- DfIRN23 %>% summarise(Promedio_total_deaths=mean(total_deaths),
                                         D.Est_total_deaths=sd(total_deaths),
                                         CV_total_deaths=sd(total_deaths)/mean(total_deaths)*100,
                                         Min_total_deaths=min(total_deaths),
                                         Max_total_deaths=max(total_deaths),
                                         Promedio_new_deaths=mean(new_deaths),
                                         D.Est_new_deaths=sd(new_deaths),
                                         CV_new_deaths=sd(new_deaths)/mean(new_deaths)*100,
                                         Min_new_deaths=min(new_deaths),
                                         Max_new_deaths=max(new_deaths))

Tasa_de_decesos_DfIRN23<-DfIRN23 %>% summarise(Promedio_total_deaths=mean(total_deaths_per_million),
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

# Estadísticas descriptivas  y tasas de DfCUB por año
#Casos

Casos_DfCUB20 <- DfCUB20 %>% summarise(Promedio_total_cases=mean(total_cases),
                                       D.Est_total_cases=sd(total_cases),
                                       CV_total_cases=sd(total_cases)/mean(total_cases)*100,
                                       Min_total_cases=min(total_cases),
                                       Max_total_cases=max(total_cases),
                                       Promedio_new_cases=mean(new_cases),
                                       D.Est_new_cases=sd(new_cases),
                                       CV_new_cases=sd(new_cases)/mean(new_cases)*100,
                                       Min_new_cases=min(new_cases),
                                       Max_new_cases=max(new_cases))

Tasa_de_casos_DfCUB20<-DfCUB20 %>% summarise(Promedio_total_cases=mean(total_cases),
                                             D.Est_total_cases=sd(total_cases),
                                             CV_total_cases=sd(total_cases)/mean(total_cases)*100,
                                             Min_total_cases=min(total_cases),
                                             Max_total_cases=max(total_cases),
                                             Promedio_new_cases=mean(new_cases),
                                             D.Est_new_cases=sd(new_cases),
                                             CV_new_cases=sd(new_cases)/mean(new_cases)*100,
                                             Min_new_cases=min(new_cases),
                                             Max_new_cases=max(new_cases))

Casos_DfCUB21 <- DfCUB21 %>% summarise(Promedio_total_cases=mean(total_cases),
                                       D.Est_total_cases=sd(total_cases),
                                       CV_total_cases=sd(total_cases)/mean(total_cases)*100,
                                       Min_total_cases=min(total_cases),
                                       Max_total_cases=max(total_cases),
                                       Promedio_new_cases=mean(new_cases),
                                       D.Est_new_cases=sd(new_cases),
                                       CV_new_cases=sd(new_cases)/mean(new_cases)*100,
                                       Min_new_cases=min(new_cases),
                                       Max_new_cases=max(new_cases))

Tasa_de_casos_DfCUB21<-DfCUB21 %>% summarise(Promedio_total_cases=mean(total_cases),
                                             D.Est_total_cases=sd(total_cases),
                                             CV_total_cases=sd(total_cases)/mean(total_cases)*100,
                                             Min_total_cases=min(total_cases),
                                             Max_total_cases=max(total_cases),
                                             Promedio_new_cases=mean(new_cases),
                                             D.Est_new_cases=sd(new_cases),
                                             CV_new_cases=sd(new_cases)/mean(new_cases)*100,
                                             Min_new_cases=min(new_cases),
                                             Max_new_cases=max(new_cases))

Casos_DfCUB22 <- DfCUB22 %>% summarise(Promedio_total_cases=mean(total_cases),
                                       D.Est_total_cases=sd(total_cases),
                                       CV_total_cases=sd(total_cases)/mean(total_cases)*100,
                                       Min_total_cases=min(total_cases),
                                       Max_total_cases=max(total_cases),
                                       Promedio_new_cases=mean(new_cases),
                                       D.Est_new_cases=sd(new_cases),
                                       CV_new_cases=sd(new_cases)/mean(new_cases)*100,
                                       Min_new_cases=min(new_cases),
                                       Max_new_cases=max(new_cases))

Tasa_de_casos_DfCUB22<-DfCUB22 %>% summarise(Promedio_total_cases=mean(total_cases),
                                             D.Est_total_cases=sd(total_cases),
                                             CV_total_cases=sd(total_cases)/mean(total_cases)*100,
                                             Min_total_cases=min(total_cases),
                                             Max_total_cases=max(total_cases),
                                             Promedio_new_cases=mean(new_cases),
                                             D.Est_new_cases=sd(new_cases),
                                             CV_new_cases=sd(new_cases)/mean(new_cases)*100,
                                             Min_new_cases=min(new_cases),
                                             Max_new_cases=max(new_cases))

Casos_DfCUB23 <- DfCUB23 %>% summarise(Promedio_total_cases=mean(total_cases),
                                       D.Est_total_cases=sd(total_cases),
                                       CV_total_cases=sd(total_cases)/mean(total_cases)*100,
                                       Min_total_cases=min(total_cases),
                                       Max_total_cases=max(total_cases),
                                       Promedio_new_cases=mean(new_cases),
                                       D.Est_new_cases=sd(new_cases),
                                       CV_new_cases=sd(new_cases)/mean(new_cases)*100,
                                       Min_new_cases=min(new_cases),
                                       Max_new_cases=max(new_cases))

Tasa_de_Casos_DfCUB23<-DfCUB23 %>% summarise(Promedio_total_cases=mean(total_cases),
                                             D.Est_total_cases=sd(total_cases),
                                             CV_total_cases=sd(total_cases)/mean(total_cases)*100,
                                             Min_total_cases=min(total_cases),
                                             Max_total_cases=max(total_cases),
                                             Promedio_new_cases=mean(new_cases),
                                             D.Est_new_cases=sd(new_cases),
                                             CV_new_cases=sd(new_cases)/mean(new_cases)*100,
                                             Min_new_cases=min(new_cases),
                                             Max_new_cases=max(new_cases))

#Vacunados

Vacunas_DfCUB20 <- DfCUB20 %>% summarise(Promedio_total_vaccinations=mean(total_vaccinations),
                                         D.Est_total_vaccinations=sd(total_vaccinations),
                                         CV_total_vaccinations=sd(total_vaccinations)/mean(total_vaccinations)*100,
                                         Min_total_vaccinations=min(total_vaccinations),
                                         Max_total_vaccinations=max(total_vaccinations),
                                         Promedio_new_vaccinations=mean(new_vaccinations),
                                         D.Est_new_vaccinations=sd(new_vaccinations),
                                         CV_new_vaccinations=sd(new_vaccinations)/mean(new_vaccinations)*100,
                                         Min_new_vaccinations=min(new_vaccinations),
                                         Max_new_vaccinations=max(new_vaccinations))

Tasa_de_vacunas_DfCUB20<-DfCUB20 %>% summarise(Promedio_total_deaths=mean(total_deaths_per_million),
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

Vacunas_DfCUB21 <- DfCUB21 %>% summarise(Promedio_total_vaccinations=mean(total_vaccinations),
                                         D.Est_total_vaccinations=sd(total_vaccinations),
                                         CV_total_vaccinations=sd(total_vaccinations)/mean(total_vaccinations)*100,
                                         Min_total_vaccinations=min(total_vaccinations),
                                         Max_total_vaccinations=max(total_vaccinations),
                                         Promedio_new_vaccinations=mean(new_vaccinations),
                                         D.Est_new_vaccinations=sd(new_vaccinations),
                                         CV_new_vaccinations=sd(new_vaccinations)/mean(new_vaccinations)*100,
                                         Min_new_vaccinations=min(new_vaccinations),
                                         Max_new_vaccinations=max(new_vaccinations))

Tasa_de_vacunas_DfCUB21<-DfCUB21 %>% summarise(Promedio_total_deaths=mean(total_deaths_per_million),
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

Vacunas_DfCUB22 <- DfCUB22 %>% summarise(Promedio_total_vaccinations=mean(total_vaccinations),
                                         D.Est_total_vaccinations=sd(total_vaccinations),
                                         CV_total_vaccinations=sd(total_vaccinations)/mean(total_vaccinations)*100,
                                         Min_total_vaccinations=min(total_vaccinations),
                                         Max_total_vaccinations=max(total_vaccinations),
                                         Promedio_new_vaccinations=mean(new_vaccinations),
                                         D.Est_new_vaccinations=sd(new_vaccinations),
                                         CV_new_vaccinations=sd(new_vaccinations)/mean(new_vaccinations)*100,
                                         Min_new_vaccinations=min(new_vaccinations),
                                         Max_new_vaccinations=max(new_vaccinations))


Tasa_de_vacunas_DfCUB22<-DfCUB22 %>% summarise(Promedio_total_deaths=mean(total_deaths_per_million),
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

Vacunas_DfCUB23 <- DfCUB23 %>% summarise(Promedio_total_vaccinations=mean(total_vaccinations),
                                         D.Est_total_vaccinations=sd(total_vaccinations),
                                         CV_total_vaccinations=sd(total_vaccinations)/mean(total_vaccinations)*100,
                                         Min_total_vaccinations=min(total_vaccinations),
                                         Max_total_vaccinations=max(total_vaccinations),
                                         Promedio_new_vaccinations=mean(new_vaccinations),
                                         D.Est_new_vaccinations=sd(new_vaccinations),
                                         CV_new_vaccinations=sd(new_vaccinations)/mean(new_vaccinations)*100,
                                         Min_new_vaccinations=min(new_vaccinations),
                                         Max_new_vaccinations=max(new_vaccinations))

Tasa_de_vacunas_DfCUB23<-DfCUB23 %>% summarise(Promedio_total_deaths=mean(total_deaths_per_million),
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

#Decesos

Decesos_DfCUB20 <- DfCUB20 %>% summarise(Promedio_total_deaths=mean(total_deaths),
                                         D.Est_total_deaths=sd(total_deaths),
                                         CV_total_deaths=sd(total_deaths)/mean(total_deaths)*100,
                                         Min_total_deaths=min(total_deaths),
                                         Max_total_deaths=max(total_deaths),
                                         Promedio_new_deaths=mean(new_deaths),
                                         D.Est_new_deaths=sd(new_deaths),
                                         CV_new_deaths=sd(new_deaths)/mean(new_deaths)*100,
                                         Min_new_deaths=min(new_deaths),
                                         Max_new_deaths=max(new_deaths))

Tasa_de_decesos_DfCUB20<-DfCUB20 %>% summarise(Promedio_total_deaths=mean(total_deaths_per_million),
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

Decesos_DfCUB21 <- DfCUB21 %>% summarise(Promedio_total_deaths=mean(total_deaths),
                                         D.Est_total_deaths=sd(total_deaths),
                                         CV_total_deaths=sd(total_deaths)/mean(total_deaths)*100,
                                         Min_total_deaths=min(total_deaths),
                                         Max_total_deaths=max(total_deaths),
                                         Promedio_new_deaths=mean(new_deaths),
                                         D.Est_new_deaths=sd(new_deaths),
                                         CV_new_deaths=sd(new_deaths)/mean(new_deaths)*100,
                                         Min_new_deaths=min(new_deaths),
                                         Max_new_deaths=max(new_deaths))

Tasa_de_decesos_DfCUB21<-DfCUB21 %>% summarise(Promedio_total_deaths=mean(total_deaths_per_million),
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

Decesos_DfCUB22 <- DfCUB22 %>% summarise(Promedio_total_deaths=mean(total_deaths),
                                         D.Est_total_deaths=sd(total_deaths),
                                         CV_total_deaths=sd(total_deaths)/mean(total_deaths)*100,
                                         Min_total_deaths=min(total_deaths),
                                         Max_total_deaths=max(total_deaths),
                                         Promedio_new_deaths=mean(new_deaths),
                                         D.Est_new_deaths=sd(new_deaths),
                                         CV_new_deaths=sd(new_deaths)/mean(new_deaths)*100,
                                         Min_new_deaths=min(new_deaths),
                                         Max_new_deaths=max(new_deaths))

Tasa_de_decesos_DfCUB22<-DfCUB22 %>% summarise(Promedio_total_deaths=mean(total_deaths_per_million),
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

Decesos_DfCUB23 <- DfCUB23 %>% summarise(Promedio_total_deaths=mean(total_deaths),
                                         D.Est_total_deaths=sd(total_deaths),
                                         CV_total_deaths=sd(total_deaths)/mean(total_deaths)*100,
                                         Min_total_deaths=min(total_deaths),
                                         Max_total_deaths=max(total_deaths),
                                         Promedio_new_deaths=mean(new_deaths),
                                         D.Est_new_deaths=sd(new_deaths),
                                         CV_new_deaths=sd(new_deaths)/mean(new_deaths)*100,
                                         Min_new_deaths=min(new_deaths),
                                         Max_new_deaths=max(new_deaths))

Tasa_de_decesos_DfCUB23<-DfCUB23 %>% summarise(Promedio_total_deaths=mean(total_deaths_per_million),
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



