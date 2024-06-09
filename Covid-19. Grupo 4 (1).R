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

#Verificacion de que no existan datos NA en US20
apply(DfUS20, MARGIN = 2, function(x) sum(is.na(x))) %>% view()



DfUS21 <- data_frame (DfUS [365:729,])
#No existen valores NA en 2021


DfUS22 <-data.frame (DfUS [730:1094,])
#En el 2022-03-16 falta el valor de New Deaths quiza se pueda sacar un calculo con la info
DfUS22 <-DfUS22 %>% mutate (new_deaths = if_else
                            (is.na(new_deaths), 2437, new_deaths))
DfUS22 <-DfUS22 %>% mutate (new_deaths_per_million = if_else
                            (is.na(new_deaths_per_million), 7.203, new_deaths_per_million))
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


DfDEU22 <- data.frame(DfDEU [730:1094,])



DfDEU23 <- data.frame(DfDEU [DfDEU$date>"2022-12-31",])


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
#Falta dato de vacunacion en 2022-01-04
#A partir de 2022-05-31 falta datos de tests
#Faltan algunos datos de vacunaciones que podrian o no ser ceros


DfTUR23 <- data.frame(DfTUR [DfTUR$date>"2022-12-31",])
#No hay informacion sobre vacunas en 2023, podrian o no ser ceros

#Estadisticas del total de casos y nuevos casos de US, DEU y TUR por año

#Estados Unidos
Casos_DfUS20 <- DfUS20 %>% summarise(Promedio_total_cases=mean(total_cases),
                                     Media_total_cases=median(total_cases),
                                     D.Est_total_cases=sd(total_cases),
                                     CV_total_cases=sd(total_cases)/mean(total_cases)*100,
                                     Min_total_cases=min(total_cases),
                                     Max_total_cases=max(total_cases),
                                     Promedio_new_cases=mean(new_cases),
                                     Media_new_cases=median(new_cases),
                                     D.Est_new_cases=sd(new_cases),
                                     CV_new_cases=sd(new_cases)/mean(new_cases)*100,
                                     Min_new_cases=min(new_cases),
                                     Max_new_cases=max(new_cases))


Casos_DfUS21 <- DfUS21 %>% summarise(Promedio_total_cases=mean(total_cases),
                                     Media_total_cases=median(total_cases),
                                     D.Est_total_cases=sd(total_cases),
                                     CV_total_cases=sd(total_cases)/mean(total_cases)*100,
                                     Min_total_cases=min(total_cases),
                                     Max_total_cases=max(total_cases),
                                     Promedio_new_cases=mean(new_cases),
                                     Media_new_cases=median(new_cases),
                                     D.Est_new_cases=sd(new_cases),
                                     CV_new_cases=sd(new_cases)/mean(new_cases)*100,
                                     Min_new_cases=min(new_cases),
                                     Max_new_cases=max(new_cases))

Casos_DfUS22 <- DfUS22 %>% summarise(Promedio_total_cases=mean(total_cases),
                                     Media_total_cases=median(total_cases),
                                     D.Est_total_cases=sd(total_cases),
                                     CV_total_cases=sd(total_cases)/mean(total_cases)*100,
                                     Min_total_cases=min(total_cases),
                                     Max_total_cases=max(total_cases),
                                     Promedio_new_cases=mean(new_cases),
                                     Media_new_cases=median(new_cases),
                                     D.Est_new_cases=sd(new_cases),
                                     CV_new_cases=sd(new_cases)/mean(new_cases)*100,
                                     Min_new_cases=min(new_cases),
                                     Max_new_cases=max(new_cases))

Casos_DfUS23 <- DfUS23 %>% summarise(Promedio_total_cases=mean(total_cases),
                                     Media_total_cases=median(total_cases),
                                     D.Est_total_cases=sd(total_cases),
                                     CV_total_cases=sd(total_cases)/mean(total_cases)*100,
                                     Min_total_cases=min(total_cases),
                                     Max_total_cases=max(total_cases),
                                     Promedio_new_cases=mean(new_cases),
                                     Media_new_cases=median(new_cases),
                                     D.Est_new_cases=sd(new_cases),
                                     CV_new_cases=sd(new_cases)/mean(new_cases)*100,
                                     Min_new_cases=min(new_cases),
                                     Max_new_cases=max(new_cases))

#Alemania

Casos_DfDEU20 <- DfDEU20 %>% summarise(Promedio_total_cases=mean(total_cases),
                                       Media_total_cases=median(total_cases),
                                       D.Est_total_cases=sd(total_cases),
                                       CV_total_cases=sd(total_cases)/mean(total_cases)*100,
                                       Min_total_cases=min(total_cases),
                                       Max_total_cases=max(total_cases),
                                       Promedio_new_cases=mean(new_cases),
                                       Media_new_cases=median(new_cases),
                                       D.Est_new_cases=sd(new_cases),
                                       CV_new_cases=sd(new_cases)/mean(new_cases)*100,
                                       Min_new_cases=min(new_cases),
                                       Max_new_cases=max(new_cases))


Casos_DfDEU21 <- DfDEU21 %>% summarise(Promedio_total_cases=mean(total_cases),
                                       Media_total_cases=median(total_cases),
                                       D.Est_total_cases=sd(total_cases),
                                       CV_total_cases=sd(total_cases)/mean(total_cases)*100,
                                       Min_total_cases=min(total_cases),
                                       Max_total_cases=max(total_cases),
                                       Promedio_new_cases=mean(new_cases),
                                       Media_new_cases=median(new_cases),
                                       D.Est_new_cases=sd(new_cases),
                                       CV_new_cases=sd(new_cases)/mean(new_cases)*100,
                                       Min_new_cases=min(new_cases),
                                       Max_new_cases=max(new_cases))

Casos_DfDEU22 <- DfDEU22 %>% summarise(Promedio_total_cases=mean(total_cases),
                                       Media_total_cases=median(total_cases),
                                       D.Est_total_cases=sd(total_cases),
                                       CV_total_cases=sd(total_cases)/mean(total_cases)*100,
                                       Min_total_cases=min(total_cases),
                                       Max_total_cases=max(total_cases),
                                       Promedio_new_cases=mean(new_cases),
                                       Media_new_cases=median(new_cases),
                                       D.Est_new_cases=sd(new_cases),
                                       CV_new_cases=sd(new_cases)/mean(new_cases)*100,
                                       Min_new_cases=min(new_cases),
                                       Max_new_cases=max(new_cases))

Casos_DfDEU23 <- DfDEU23 %>% summarise(Promedio_total_cases=mean(total_cases),
                                       Media_total_cases=median(total_cases),
                                       D.Est_total_cases=sd(total_cases),
                                       CV_total_cases=sd(total_cases)/mean(total_cases)*100,
                                       Min_total_cases=min(total_cases),
                                       Max_total_cases=max(total_cases),
                                       Promedio_new_cases=mean(new_cases),
                                       Media_new_cases=median(new_cases),
                                       D.Est_new_cases=sd(new_cases),
                                       CV_new_cases=sd(new_cases)/mean(new_cases)*100,
                                       Min_new_cases=min(new_cases),
                                       Max_new_cases=max(new_cases))

#Turquia

Casos_DfTUR20 <- DfTUR20 %>% summarise(Promedio_total_cases=mean(total_cases),
                                       Media_total_cases=median(total_cases),
                                       D.Est_total_cases=sd(total_cases),
                                       CV_total_cases=sd(total_cases)/mean(total_cases)*100,
                                       Min_total_cases=min(total_cases),
                                       Max_total_cases=max(total_cases),
                                       Promedio_new_cases=mean(new_cases),
                                       Media_new_cases=median(new_cases),
                                       D.Est_new_cases=sd(new_cases),
                                       CV_new_cases=sd(new_cases)/mean(new_cases)*100,
                                       Min_new_cases=min(new_cases),
                                       Max_new_cases=max(new_cases))


Casos_DfTUR21 <- DfTUR21 %>% summarise(Promedio_total_cases=mean(total_cases),
                                       Media_total_cases=median(total_cases),
                                       D.Est_total_cases=sd(total_cases),
                                       CV_total_cases=sd(total_cases)/mean(total_cases)*100,
                                       Min_total_cases=min(total_cases),
                                       Max_total_cases=max(total_cases),
                                       Promedio_new_cases=mean(new_cases),
                                       Media_new_cases=median(new_cases),
                                       D.Est_new_cases=sd(new_cases),
                                       CV_new_cases=sd(new_cases)/mean(new_cases)*100,
                                       Min_new_cases=min(new_cases),
                                       Max_new_cases=max(new_cases))

Casos_DfTUR22 <- DfTUR22 %>% summarise(Promedio_total_cases=mean(total_cases),
                                       Media_total_cases=median(total_cases),
                                       D.Est_total_cases=sd(total_cases),
                                       CV_total_cases=sd(total_cases)/mean(total_cases)*100,
                                       Min_total_cases=min(total_cases),
                                       Max_total_cases=max(total_cases),
                                       Promedio_new_cases=mean(new_cases),
                                       Media_new_cases=median(new_cases),
                                       D.Est_new_cases=sd(new_cases),
                                       CV_new_cases=sd(new_cases)/mean(new_cases)*100,
                                       Min_new_cases=min(new_cases),
                                       Max_new_cases=max(new_cases))

Casos_DfTUR23 <- DfTUR23 %>% summarise(Promedio_total_cases=mean(total_cases),
                                       Media_total_cases=median(total_cases),
                                       D.Est_total_cases=sd(total_cases),
                                       CV_total_cases=sd(total_cases)/mean(total_cases)*100,
                                       Min_total_cases=min(total_cases),
                                       Max_total_cases=max(total_cases),
                                       Promedio_new_cases=mean(new_cases),
                                       Media_new_cases=median(new_cases),
                                       D.Est_new_cases=sd(new_cases),
                                       CV_new_cases=sd(new_cases)/mean(new_cases)*100,
                                       Min_new_cases=min(new_cases),
                                       Max_new_cases=max(new_cases))





#Llamado a la librería
library(dplyr)
#Se muestran los NA en el primer año de Covid en CHN
apply(CHN[CHN$date<"2021-01-01",], MARGIN = 2, function(x) sum(is.na(x))) %>% View()

#Eliminación de variables no necesarias para el trabajo
CHN <- CHN %>% select(index, iso_code, continent, location, date, 
                      total_cases, new_cases, new_cases_per_million,
                      total_cases_per_million, 
                      total_deaths, new_deaths, new_deaths_per_million, 
                      total_deaths_per_million, 
                      total_vaccinations, new_vaccinations, 
                      total_vaccinations_per_hundred, people_vaccinated, 
                      people_fully_vaccinated, people_vaccinated_per_hundred, 
                      people_fully_vaccinated_per_hundred, 
                      population_density, population, median_age)
# Cambio de valores NA en China en el primer año
CHN20 <- data.frame (CHN [CHN$date<"2021-01-01",], na.rm = TRUE)
CHN20 <- CHN20 %>% mutate(total_cases =if_else(is.na(total_cases), 0, total_cases))
CHN20 <- CHN20 %>% mutate(total_cases_per_million =if_else(is.na(total_cases_per_million), 0, total_cases_per_million))
CHN20 <- CHN20 %>% mutate(total_deaths =if_else(is.na(total_deaths), 0, total_deaths))
CHN20 <- CHN20 %>% mutate(total_deaths_per_million =if_else(is.na(total_deaths_per_million), 0, total_deaths_per_million))
CHN20 <- CHN20 %>% mutate(total_vaccinations =if_else(is.na(total_vaccinations), 0, total_vaccinations))
CHN20 <- CHN20 %>% mutate(total_vaccinations_per_hundred =if_else(is.na(total_vaccinations_per_hundred), 0, total_vaccinations_per_hundred))
CHN20 <- CHN20 %>% mutate(new_vaccinations =if_else(is.na(new_vaccinations), 0, new_vaccinations))
CHN20 <- CHN20 %>% mutate(people_vaccinated =if_else(is.na(people_vaccinated), 0, people_vaccinated))
CHN20 <- CHN20 %>% mutate(people_fully_vaccinated =if_else(is.na(people_fully_vaccinated), 0, people_fully_vaccinated))
CHN20 <- CHN20 %>% mutate(people_vaccinated_per_hundred =if_else(is.na(people_vaccinated_per_hundred), 0, people_vaccinated_per_hundred))
CHN20 <- CHN20 %>% mutate(people_fully_vaccinated_per_hundred =if_else(is.na(people_fully_vaccinated_per_hundred), 0, people_fully_vaccinated_per_hundred))

#Se muestran los NA del segundo año en CHN
apply(CHN[365:729,], MARGIN = 2, function(x) sum(is.na(x))) %>% View()
#Eliminación de los mismos en el segundo año
CHN21 <- data.frame(CHN [365:729,])
CHN21 <- CHN21 %>% mutate(total_vaccinations =if_else(is.na(total_vaccinations), 0, total_vaccinations))
CHN21 <- CHN21 %>% mutate(new_vaccinations =if_else(is.na(new_vaccinations), 0, new_vaccinations))
CHN21 <- CHN21 %>% mutate(total_vaccinations_per_hundred =if_else(is.na(total_vaccinations_per_hundred), 0, total_vaccinations_per_hundred))
CHN21 <- CHN21 %>% mutate(people_vaccinated =if_else(is.na(people_vaccinated), 0, people_vaccinated))
CHN21 <- CHN21 %>% mutate(people_fully_vaccinated =if_else(is.na(people_fully_vaccinated), 0, people_fully_vaccinated))
CHN21 <- CHN21 %>% mutate(people_vaccinated_per_hundred =if_else(is.na(people_vaccinated_per_hundred), 0, people_vaccinated_per_hundred))
CHN21 <- CHN21 %>% mutate(people_fully_vaccinated_per_hundred =if_else(is.na(people_fully_vaccinated_per_hundred), 0, people_fully_vaccinated_per_hundred))
#Se muestran los NA del tercer año en CHN
apply(CHN[730:1094,], MARGIN = 2, function(x) sum(is.na(x))) %>% View()
#Se eliminan los NA del mismo
CHN22 <- data.frame(CHN [730:1094,])
CHN22 <- CHN22 %>% mutate(people_vaccinated =if_else(is.na(people_vaccinated), 0, people_vaccinated))
CHN22 <- CHN22 %>% mutate(people_fully_vaccinated =if_else(is.na(people_fully_vaccinated), 0, people_fully_vaccinated))
CHN22 <- CHN22 %>% mutate(people_vaccinated_per_hundred =if_else(is.na(people_vaccinated_per_hundred), 0, people_vaccinated_per_hundred))
CHN22 <- CHN22 %>% mutate(people_fully_vaccinated_per_hundred =if_else(is.na(people_fully_vaccinated_per_hundred), 0, people_fully_vaccinated_per_hundred))
#Se muestran los NA del cuarto año de CHN
apply(CHN[CHN$date>"2021-12-31",], MARGIN = 2, function(x) sum(is.na(x))) %>% View()
#Se eleminan los NA del mismo
CHN23 <- data.frame(CHN[CHN$date>"2022-12-31",])
CHN23 <- CHN23 %>% mutate(new_cases =if_else(is.na(new_cases), 0, new_cases))
CHN23 <- CHN23 %>% mutate(new_cases_per_million =if_else(is.na(new_cases_per_million), 0, new_cases_per_million))
CHN23 <- CHN23 %>% mutate(total_vaccinations =if_else(is.na(total_vaccinations), 0, total_vaccinations))
CHN23 <- CHN23 %>% mutate(new_vaccinations =if_else(is.na(new_vaccinations), 0, new_vaccinations))
CHN23 <- CHN23 %>% mutate(total_vaccinations_per_hundred =if_else(is.na(total_vaccinations_per_hundred), 0, total_vaccinations_per_hundred))
CHN23 <- CHN23 %>% mutate(people_vaccinated =if_else(is.na(people_vaccinated), 0, people_vaccinated))
CHN23 <- CHN23 %>% mutate(people_fully_vaccinated =if_else(is.na(people_fully_vaccinated), 0, people_fully_vaccinated))
CHN23 <- CHN23 %>% mutate(people_vaccinated_per_hundred =if_else(is.na(people_vaccinated_per_hundred), 0, people_vaccinated_per_hundred))
CHN23 <- CHN23 %>% mutate(people_fully_vaccinated_per_hundred =if_else(is.na(people_fully_vaccinated_per_hundred), 0, people_fully_vaccinated_per_hundred))


