# Libraries
library(readr)
library(dplyr)
library(readxl)
library(stringi)
library(lubridate)
library(plotly)

# Import Data
com2013 <- read_delim(paste0(path,"Data/france2013.txt")) %>% mutate(insee13=13)
com2014 <- read_delim(paste0(path,"Data/france2014.txt")) %>% mutate(insee14=14)
com2015 <- read_delim(paste0(path,"Data/comsimp2015.txt")) %>% mutate(insee15=15)
com2021 <- read_csv(paste0(path,"Data/cog_ensemble_2021_csv/commune2021.csv")) %>% mutate(insee21=21)

infos_com <- read_csv2(paste0(path,"Data/correspondance-code-insee-code-postal.csv"))

com_bound <- sf::st_read("../Data_GIS/communes-version-simplifiee.geojson")
com_bound <- com_bound %>% mutate(x="contour")


names(com2013) <- names(com2013) %>% stri_trans_tolower() 
names(com2014) <- names(com2014) %>% stri_trans_tolower() 
names(com2015) <- names(com2015) %>% stri_trans_tolower() 
names(com2021) <- names(com2021) %>% stri_trans_tolower() 

com2013 <- com2013 %>% 
  mutate(com = paste0(com2013$dep, com2013$com)) %>% 
  select(com, ncc, insee13)
com2014 <- com2014 %>% 
  mutate(com = paste0(com2014$dep, com2014$com)) %>% 
  select(com, ncc, insee14)
com2015 <- com2015 %>% 
  mutate(com = paste0(com2015$dep, com2015$com)) %>%
  select(com, ncc, insee15)
com2021 <- com2021 %>% select(com, ncc, insee21)


# Test merge avec infos communes (altitude, pop...)
infos_com$infos = 1
infos_com13 <- merge(infos_com, com2013, by.x="Code INSEE", by.y="com", all = T)
infos_com14 <- merge(infos_com, com2014, by.x="Code INSEE", by.y="com", all = T)
infos_com15 <- merge(infos_com, com2015, by.x="Code INSEE", by.y="com", all = T)
infos_com21 <- merge(infos_com, com2021, by.x="Code INSEE", by.y="com", all = T)




# Test de merge insee vs contours
com_bound_2014 <- merge(com_bound, com2014 %>% mutate(y=1), by.x = "code", by.y = "com", all = T)
com_bound_2015 <- merge(com_bound, com2015 %>% mutate(y=1), by.x = "code", by.y = "com", all = T)
com_bound_2021 <- merge(com_bound, insee_com %>% mutate(y=1), by.x = "code", by.y = "com", all = T)


table(com_bound_2014$x, com_bound_2014$y, useNA='ifany')
table(com_bound_2015$x, com_bound_2015$y, useNA='ifany')
table(com_bound_2021$x, com_bound_2021$y, useNA='ifany')

nrow(com_bound)
nrow(com_bound_2015)
nrow(com_bound_2021)


# Test de merge insee vs data catnat
data <- data <- read_rds("../Data/arrete_catnat.rds")

data_2014 <- merge(data, com2014 %>% mutate(y=1), by.x = "insee", by.y = "com", all.x = T)
data_2015 <- merge(data, com2015 %>% mutate(y=1), by.x = "insee", by.y = "com", all.x = T)
data_2021 <- merge(data, insee_com %>% mutate(y=1), by.x = "insee", by.y = "com", all.x = T)

table(data_2014$y,useNA='ifany') # toutes les communes dans datasont dans insee2014
table(data_2015$y,useNA='ifany') 
table(data_2021$y,useNA='ifany')


nrow(data)
nrow(data_2015)
nrow(data_2021)


# Test de merge contours vs data catnat
data_bound <- merge(com_bound, data %>% mutate(y=1), by.x = "code", by.y = "insee", all = T)

table(data_bound$x, data_bound$y, useNA='ifany')

nrow(unique(data_bound%>%filter(is.na(x))%>%select(code)))
