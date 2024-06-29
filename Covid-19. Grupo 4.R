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


DfDEU23 <- data.frame(DfDEU [DfDEU$date>"2022-12-31",])
DfDEU23 [172,6:7] <- 0

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
ESP21 [206,14] <- (54259325--53449255)
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
ESP21 [234,14] <- (63090941-63590985)
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
ESP21 [332,13] <- (76123553-76026523)
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

#Creacion de la variable new_vaccinations_per_hundred para cada pais

ESP20 <- mutate(ESP20, new_vaccinations_per_hundred=new_vaccinations/population*100)
ESP21 <- mutate(ESP21, new_vaccinations_per_hundred=new_vaccinations/population*100)

#Estadisticos por año 
#España Casos
Casos_ESP20 <- ESP20 %>% summarise(Promedio_total_cases=mean(total_cases),
                                   D.Est_total_cases=sd(total_cases),
                                   CV_total_cases=sd(total_cases)/mean(total_cases)*100,
                                   Min_total_cases=min(total_cases),
                                   Max_total_cases=max(total_cases),
                                   Promedio_new_cases=mean(new_cases),
                                   D.Est_new_cases=sd(new_cases),
                                   CV_new_cases=sd(new_cases)/mean(new_cases)*100,
                                   Min_new_cases=min(new_cases),
                                   Max_new_cases=max(new_cases))

Tasa_de_casos_ESP20 <- ESP20 %>% summarise(Promedio_total_cases=mean(total_cases_per_million),
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


Casos_ESP21 <- ESP21 %>% summarise(Promedio_total_cases=mean(total_cases),
                                   D.Est_total_cases=sd(total_cases),
                                   CV_total_cases=sd(total_cases)/mean(total_cases)*100,
                                   Min_total_cases=min(total_cases),
                                   Max_total_cases=max(total_cases),
                                   Promedio_new_cases=mean(new_cases),
                                   D.Est_new_cases=sd(new_cases),
                                   CV_new_cases=sd(new_cases)/mean(new_cases)*100,
                                   Min_new_cases=min(new_cases),
                                   Max_new_cases=max(new_cases))

Tasa_de_casos_ESP21 <- ESP21 %>% summarise(Promedio_total_cases=mean(total_cases_per_million),
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

#Vacunaciones 
Vacunas_ESP20 <-ESP20 %>% summarise(Promedio_total_vaccinations=mean(total_vaccinations),
                                    D.Est_total_vaccinations=sd(total_vaccinations),
                                    CV_total_vaccinations=sd(total_vaccinations)/mean(total_vaccinations)*100,
                                    Min_total_vaccinations=min(total_vaccinations),
                                    Max_total_vaccinations=max(total_vaccinations),
                                    Promedio_new_vaccinations=mean(new_vaccinations),
                                    D.Est_new_vaccinations=sd(new_vaccinations),
                                    CV_new_vaccinations=sd(new_vaccinations)/mean(new_vaccinations)*100,
                                    Min_new_vaccinations=min(new_vaccinations),
                                    Max_new_vaccinations=max(new_vaccinations))

Tasa_de_vacunas_ESP20 <- ESP20 %>% summarise(Promedio_total_vaccinations=mean(total_vaccinations_per_hundred),
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

Vacunas_ESP21 <- ESP21 %>% summarise(Promedio_total_vaccinations=mean(total_vaccinations),
                                     D.Est_total_vaccinations=sd(total_vaccinations),
                                     CV_total_vaccinations=sd(total_vaccinations)/mean(total_vaccinations)*100,
                                     Min_total_vaccinations=min(total_vaccinations),
                                     Max_total_vaccinations=max(total_vaccinations),
                                     Promedio_new_vaccinations=mean(new_vaccinations),
                                     D.Est_new_vaccinations=sd(new_vaccinations),
                                     CV_new_vaccinations=sd(new_vaccinations)/mean(new_vaccinations)*100,
                                     Min_new_vaccinations=min(new_vaccinations),
                                     Max_new_vaccinations=max(new_vaccinations))

Tasa_de_vacunas_ESP21 <- ESP21 %>% summarise(Promedio_total_vaccinations=mean(total_vaccinations_per_hundred),
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

#Decesos

Decesos_ESP20 <- ESP20 %>% summarise(Promedio_total_deaths=mean(total_deaths),
                                     D.Est_total_deaths=sd(total_deaths),
                                     CV_total_deaths=sd(total_deaths)/mean(total_deaths)*100,
                                     Min_total_deaths=min(total_deaths),
                                     Max_total_deaths=max(total_deaths),
                                     Promedio_new_deaths=mean(new_deaths),
                                     D.Est_new_deaths=sd(new_deaths),
                                     CV_new_deaths=sd(new_deaths)/mean(new_deaths)*100,
                                     Min_new_deaths=min(new_deaths),
                                     Max_new_deaths=max(new_deaths))

Tasa_de_decesos_ESP20 <- ESP20 %>% summarise(Promedio_total_deaths=mean(total_deaths_per_million),
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

Decesos_ESP21 <- ESP21 %>% summarise(Promedio_total_deaths=mean(total_deaths),
                                     D.Est_total_deaths=sd(total_deaths),
                                     CV_total_deaths=sd(total_deaths)/mean(total_deaths)*100,
                                     Min_total_deaths=min(total_deaths),
                                     Max_total_deaths=max(total_deaths),
                                     Promedio_new_deaths=mean(new_deaths),
                                     D.Est_new_deaths=sd(new_deaths),
                                     CV_new_deaths=sd(new_deaths)/mean(new_deaths)*100,
                                     Min_new_deaths=min(new_deaths),
                                     Max_new_deaths=max(new_deaths))

Tasa_de_decesos_ESP21 <- ESP21 %>% summarise(Promedio_total_deaths=mean(total_deaths_per_million),
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
