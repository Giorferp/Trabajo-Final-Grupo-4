#Llamado a tidyverse 

library(tidyverse)

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

CHN <- CHN %>% select(index, iso_code, continent, location, date, 
                      total_cases, new_cases, new_cases_per_million,
                      total_cases_per_million, 
                      total_deaths, new_deaths, new_deaths_per_million, 
                      total_deaths_per_million, 
                      excess_mortality, excess_mortality_cumulative, 
                      excess_mortality_cumulative_absolute, 
                      excess_mortality_cumulative_per_million, 
                      total_tests, new_tests, new_tests_per_thousand, 
                      total_tests_per_thousand, positive_rate, 
                      tests_per_case, tests_units, 
                      total_vaccinations, new_vaccinations, 
                      total_vaccinations_per_hundred, people_vaccinated, 
                      people_fully_vaccinated, people_vaccinated_per_hundred, 
                      people_fully_vaccinated_per_hundred, 
                      population_density, population, median_age)

CUB <- CUB %>% select(index, iso_code, continent, location, date, 
                      total_cases, new_cases, new_cases_per_million,
                      total_cases_per_million, 
                      total_deaths, new_deaths, new_deaths_per_million, 
                      total_deaths_per_million, 
                      excess_mortality, excess_mortality_cumulative, 
                      excess_mortality_cumulative_absolute, 
                      excess_mortality_cumulative_per_million, 
                      total_tests, new_tests, new_tests_per_thousand, 
                      total_tests_per_thousand, positive_rate, 
                      tests_per_case, tests_units, 
                      total_vaccinations, new_vaccinations, 
                      total_vaccinations_per_hundred, people_vaccinated, 
                      people_fully_vaccinated, people_vaccinated_per_hundred, 
                      people_fully_vaccinated_per_hundred, 
                      population_density, population, median_age)

DEU <- DEU %>% select(index, iso_code, continent, location, date, 
                      total_cases, new_cases, new_cases_per_million,
                      total_cases_per_million, 
                      total_deaths, new_deaths, new_deaths_per_million, 
                      total_deaths_per_million, 
                      excess_mortality, excess_mortality_cumulative, 
                      excess_mortality_cumulative_absolute, 
                      excess_mortality_cumulative_per_million, 
                      total_tests, new_tests, new_tests_per_thousand, 
                      total_tests_per_thousand, positive_rate, 
                      tests_per_case, tests_units, 
                      total_vaccinations, new_vaccinations, 
                      total_vaccinations_per_hundred, people_vaccinated, 
                      people_fully_vaccinated, people_vaccinated_per_hundred, 
                      people_fully_vaccinated_per_hundred, 
                      population_density, population, median_age)

ESP <- ESP %>% select(index, iso_code, continent, location, date, 
                      total_cases, new_cases, new_cases_per_million,
                      total_cases_per_million, 
                      total_deaths, new_deaths, new_deaths_per_million, 
                      total_deaths_per_million, 
                      excess_mortality, excess_mortality_cumulative, 
                      excess_mortality_cumulative_absolute, 
                      excess_mortality_cumulative_per_million, 
                      total_tests, new_tests, new_tests_per_thousand, 
                      total_tests_per_thousand, positive_rate, 
                      tests_per_case, tests_units, 
                      total_vaccinations, new_vaccinations, 
                      total_vaccinations_per_hundred, people_vaccinated, 
                      people_fully_vaccinated, people_vaccinated_per_hundred, 
                      people_fully_vaccinated_per_hundred, 
                      population_density, population, median_age)

GBR <- GBR %>% select(index, iso_code, continent, location, date, 
                      total_cases, new_cases, new_cases_per_million,
                      total_cases_per_million, 
                      total_deaths, new_deaths, new_deaths_per_million, 
                      total_deaths_per_million, 
                      excess_mortality, excess_mortality_cumulative, 
                      excess_mortality_cumulative_absolute, 
                      excess_mortality_cumulative_per_million, 
                      total_tests, new_tests, new_tests_per_thousand, 
                      total_tests_per_thousand, positive_rate, 
                      tests_per_case, tests_units, 
                      total_vaccinations, new_vaccinations, 
                      total_vaccinations_per_hundred, people_vaccinated, 
                      people_fully_vaccinated, people_vaccinated_per_hundred, 
                      people_fully_vaccinated_per_hundred, 
                      population_density, population, median_age)

IND <- IND %>% select(index, iso_code, continent, location, date, 
                      total_cases, new_cases, new_cases_per_million,
                      total_cases_per_million, 
                      total_deaths, new_deaths, new_deaths_per_million, 
                      total_deaths_per_million, 
                      excess_mortality, excess_mortality_cumulative, 
                      excess_mortality_cumulative_absolute, 
                      excess_mortality_cumulative_per_million, 
                      total_tests, new_tests, new_tests_per_thousand, 
                      total_tests_per_thousand, positive_rate, 
                      tests_per_case, tests_units, 
                      total_vaccinations, new_vaccinations, 
                      total_vaccinations_per_hundred, people_vaccinated, 
                      people_fully_vaccinated, people_vaccinated_per_hundred, 
                      people_fully_vaccinated_per_hundred, 
                      population_density, population, median_age)

IRN <- IRN %>% select(index, iso_code, continent, location, date, 
                      total_cases, new_cases, new_cases_per_million,
                      total_cases_per_million, 
                      total_deaths, new_deaths, new_deaths_per_million, 
                      total_deaths_per_million, 
                      excess_mortality, excess_mortality_cumulative, 
                      excess_mortality_cumulative_absolute, 
                      excess_mortality_cumulative_per_million, 
                      total_tests, new_tests, new_tests_per_thousand, 
                      total_tests_per_thousand, positive_rate, 
                      tests_per_case, tests_units, 
                      total_vaccinations, new_vaccinations, 
                      total_vaccinations_per_hundred, people_vaccinated, 
                      people_fully_vaccinated, people_vaccinated_per_hundred, 
                      people_fully_vaccinated_per_hundred, 
                      population_density, population, median_age)

RUS <- RUS %>% select(index, iso_code, continent, location, date, 
                      total_cases, new_cases, new_cases_per_million,
                      total_cases_per_million, 
                      total_deaths, new_deaths, new_deaths_per_million, 
                      total_deaths_per_million, 
                      excess_mortality, excess_mortality_cumulative, 
                      excess_mortality_cumulative_absolute, 
                      excess_mortality_cumulative_per_million, 
                      total_tests, new_tests, new_tests_per_thousand, 
                      total_tests_per_thousand, positive_rate, 
                      tests_per_case, tests_units, 
                      total_vaccinations, new_vaccinations, 
                      total_vaccinations_per_hundred, people_vaccinated, 
                      people_fully_vaccinated, people_vaccinated_per_hundred, 
                      people_fully_vaccinated_per_hundred, 
                      population_density, population, median_age)

TUR <- TUR %>% select(index, iso_code, continent, location, date, 
                      total_cases, new_cases, new_cases_per_million,
                      total_cases_per_million, 
                      total_deaths, new_deaths, new_deaths_per_million, 
                      total_deaths_per_million, 
                      excess_mortality, excess_mortality_cumulative, 
                      excess_mortality_cumulative_absolute, 
                      excess_mortality_cumulative_per_million, 
                      total_tests, new_tests, new_tests_per_thousand, 
                      total_tests_per_thousand, positive_rate, 
                      tests_per_case, tests_units, 
                      total_vaccinations, new_vaccinations, 
                      total_vaccinations_per_hundred, people_vaccinated, 
                      people_fully_vaccinated, people_vaccinated_per_hundred, 
                      people_fully_vaccinated_per_hundred, 
                      population_density, population, median_age)

US <- US %>% select(index, iso_code, continent, location, date, 
                    total_cases, new_cases, new_cases_per_million,
                    total_cases_per_million, 
                    total_deaths, new_deaths, new_deaths_per_million, 
                    total_deaths_per_million, 
                    excess_mortality, excess_mortality_cumulative, 
                    excess_mortality_cumulative_absolute, 
                    excess_mortality_cumulative_per_million, 
                    total_tests, new_tests, new_tests_per_thousand, 
                    total_tests_per_thousand, positive_rate, 
                    tests_per_case, tests_units, 
                    total_vaccinations, new_vaccinations, 
                    total_vaccinations_per_hundred, people_vaccinated, 
                    people_fully_vaccinated, people_vaccinated_per_hundred, 
                    people_fully_vaccinated_per_hundred, 
                    population_density, population, median_age)


#La variable de "nuevos vacunados por cien" no existe


#Uso de "summary" para visualizar informacion estadistica de cada pais


summary(CHN, na.rm = TRUE)
summary(CUB, na.rm = TRUE)
summary(DEU, na.rm = TRUE)
summary(ESP, na.rm = TRUE)
summary(GBR, na.rm = TRUE)
summary(IND, na.rm = TRUE)
summary(IRN, na.rm = TRUE)
summary(RUS, na.rm = TRUE)
summary(TUR, na.rm = TRUE)
summary(US, na.rm = TRUE)



