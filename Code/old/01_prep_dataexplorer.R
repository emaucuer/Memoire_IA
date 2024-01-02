# --------------------------------------------- #
# Date : 05/07/2022
# Auteur : E. Maucuer
# Description : Première découverte des données
# d'arrétés de CATNAT FR metropolitaine et
# nettoyage
# --------------------------------------------- #

# Libraries
library(readr)
library(dplyr)
library(readxl)
library(stringi)
library(lubridate)
library(tidyr)

# Data partie 1
data <- read_excel("../Data/CATNAT/dataexplorer/Arretes_de_catastrophe_naturelles.xlsx")

# Data partie 2
insee_com2014 <- read_delim("../Data/france2014.txt")
insee_com2015 <- read_delim("../Data/comsimp2015.txt")
insee_com <- read_csv("../Data/cog_ensemble_2021_csv/commune2021.csv")
insee_dep <- read_csv("../Data/cog_ensemble_2021_csv/departement2021.csv")
insee_reg <- read_csv( "../Data/cog_ensemble_2021_csv/region2021.csv")


# ----------------------------------------------------------------------------- #
# PARTIE 1 : TRAVAIL SUR LES DONNEES CATNAT ----------------------------------- 
# ----------------------------------------------------------------------------- #

## Traitement des noms de variables -----------------------------------
names(data) <- names(data) %>%
  stri_trans_tolower() %>%
  stri_trans_general(id = "Latin-ASCII") 
names(data) <- gsub(" ", "_", names(data)) 



## Gestion des valeurs manquantes ----------------------------------------
summary(data)
sapply(data, function(x) sum(is.na(x)))
data <- data %>% filter(!is.na(perils))



## Traitement codes insee et departements --------------------------------
data <- data %>% 
  mutate(insee = sub("\\.(.+?)$", "", insee)) %>% 
  mutate(insee = ifelse(nchar(insee)==4, paste0("0", insee), insee),
         departement = substr(insee,1,2)) 

# Cas de Paris et ses arrondissements
data <- data %>%
  mutate(insee = ifelse(departement == "75", "75056", insee)) %>%
  mutate(commune = ifelse(departement == "75", "Paris", commune))



## Variables temporelles ---------------------------------------------
data <- data %>%
  mutate(date_fin = ymd(date_fin),
         date_debut = ymd(date_debut),
         an_debut = as.integer(year(date_debut)),
         an_fin = as.integer(year(date_fin)),
         duree = date_fin-date_debut +1)



## Variable perils -------------------------------------------------------
table(data$perils)
data$inondation <- stri_detect_regex(data$perils, "inondation", case_insensitive = T)
data$secheresse <- stri_detect_regex(data$perils, "sécheresse", case_insensitive = T)



## Analyse des doublons --------------------------------------------------
data <- data %>% mutate(dup = duplicated(data))
table(data$dup)
data <- data %>% select(-dup) %>% distinct()

# Meme date de debut pour une meme commune et un meme peril
data <- data %>% group_by(insee, date_debut, perils) %>% mutate(dup = n()) %>% ungroup()
table(data$dup)
data <- data %>% 
  arrange(insee, date_debut, desc(duree)) %>% 
  distinct(insee, date_debut, perils, .keep_all = T) %>%
  select(-dup)

# Meme date de fin pour une meme commune et un meme peril
data <- data %>% group_by(insee, date_fin, perils) %>% mutate(dup2 = n()) %>% ungroup()
table(data$dup2)
data <- data %>% 
  arrange(insee, date_debut, desc(duree)) %>% 
  distinct(insee, date_debut, perils, .keep_all = T) %>%
  select(-dup2)



## Gestion des perils pluri-annuels ----------------------------------------
an_min = min(data$an_debut)
an_max = max(data$an_fin)

for (an in (an_min:(an_max+1))){
  varname = paste0("y",an)
  data[[varname]] <- with(data, an >= an_debut & an <= an_fin)
}

data <- data %>% 
  pivot_longer(paste0("y", 1982:2016), names_to="annee") %>%
  filter(value==T) %>%
  select(-value) %>%
  mutate(annee = substr(annee,2, 5))



## Enregistrement des donnees ----------------------------------------
write_rds(data, "../Data/arrete_catnat.rds")





# ----------------------------------------------------------------------------- #
# PARTIE 2 : TRAVAIL SUR LES DONNEES INSEE ET VAR GEOGRAPHIQUES ---------------
# ----------------------------------------------------------------------------- #

## 1) Travail sur les donnees INSEE-----
### Traitement des noms de variables -----
names(insee_com2015) <- names(insee_com2015) %>% stri_trans_tolower() 
names(insee_com) <- names(insee_com) %>% stri_trans_tolower() 
names(insee_dep) <- names(insee_dep) %>% stri_trans_tolower() 
names(insee_reg) <- names(insee_reg) %>% stri_trans_tolower() 



### Variables géographiques -----
# Données insee (CATNAT sur la metropole uniquement)
insee_com2015 <- insee_com2015 %>%  
  mutate(com = paste0(insee_com2015$dep, insee_com2015$com)) %>% 
  filter(nchar(dep)<=2) %>%
  select(com, ncc)
insee_com <- insee_com %>% filter(nchar(dep)<=2) 
insee_dep <- insee_dep %>% 
  filter(nchar(dep)<=2) %>% select(dep, reg, ncc)
insee_reg <- insee_reg %>% filter(! reg %in% c("01","02","03","04","05","06")) %>% 
  select(reg, ncc)

data <- data %>% 
  left_join(insee_dep, by=c("departement"="dep")) %>%
  left_join(insee_reg, by="reg", suffix=c("_dep", "_reg"))



## 2) Bases maille geographiques x annees -----------------------------------------
an_min = min(data$an_debut)
an_max = max(data$an_fin)
delta_an = an_max-an_min+1

# Tous les dep x tous les ans 
dep_an <- insee_dep %>% select("dep", "ncc")
dep_an <- dep_an[rep(c(1:nrow(dep_an)), delta_an),]
dep_an <- dep_an %>% 
  arrange(dep) %>% 
  mutate(an = rep(an_min:an_max, length(unique(dep_an$dep))))
# Toutes les reg x tous les ans
reg_an <- insee_reg %>% select("reg", "ncc")
reg_an <- reg_an[rep(c(1:nrow(reg_an)), delta_an),]
reg_an <- reg_an %>% 
  arrange(reg) %>% 
  mutate(an = rep(an_min:an_max, length(unique(reg_an$reg))))
# Toutes les communes x tous les ans
com_an <- insee_com2015 %>% select("com", "ncc")
com_an <- com_an[rep(c(1:nrow(com_an)), delta_an),]
com_an <- com_an %>% 
  arrange(com) %>% 
  mutate(an = rep(an_min:an_max, length(com_an$com)/(delta_an)))



## 3) Préparation des données ----------------------------------------------------

### Data par region et par an -----
### V1 deduplication des lignes par région
data_reg_an_dedup <- data %>% 
  select(-c(insee, commune, departement, grep("_dep", names(data)))) %>%
  distinct() %>%
  group_by(reg, annee, ncc_reg) %>%
  summarise(nb_inondation = sum(inondation),
            nb_secheresse = sum(secheresse)) %>%
  merge(reg_an, by.x=c("annee","reg","ncc_reg"), by.y=c("an","reg","ncc"), all = T) %>%
  replace(is.na(.), 0)

### V2 sans deduplication
data_reg_an <- data %>% 
  group_by(reg, ncc_reg, annee) %>%
  summarise(nb_inondation = sum(inondation),
            nb_secheresse = sum(secheresse)) %>%
  merge(reg_an, by.x=c("annee","reg","ncc_reg"), by.y=c("an","reg","ncc"), all = T) %>%
  replace(is.na(.), 0)



### Data par departement et par an -----
### V1 deduplication des lignes par departement
data_dep_an_dedup <- data %>% 
  select(-c(insee, commune)) %>%
  distinct() %>%
  group_by(departement, annee, ncc_dep) %>%
  summarise(nb_inondation = sum(inondation),
            nb_secheresse = sum(secheresse)) %>%
  merge(dep_an %>% mutate(y=1), by.x=c("annee","departement","ncc_dep"), by.y=c("an","dep","ncc"), all = T) %>%
  replace(is.na(.), 0)

### V2 sans deduplication
data_dep_an <- data %>% 
  group_by(departement, annee, ncc_dep) %>%
  summarise(nb_inondation = sum(inondation),
            nb_secheresse = sum(secheresse)) %>%
  merge(dep_an, by.x=c("annee","departement","ncc_dep"), by.y=c("an","dep","ncc"), all = T) %>%
  replace(is.na(.), 0)



### Data par commune et par an -----
data_com_an <- data %>% 
  group_by(insee, commune, annee) %>%
  summarise(nb_inondation = sum(inondation),
            nb_secheresse = sum(secheresse)) %>%
  mutate(x=1) %>%
  merge(com_an %>% mutate(y=1), by.x=c("annee","insee"), by.y=c("an","com"), all = T) %>%
  replace(is.na(.), 0)
table(data_com_an$x, data_com_an$y, useNA='ifany')



## 4) Enregistrement des donnees -----
write_rds(data_reg_an_dedup, "../Data/CATNAT/dataexplorer/data_reg_an_dedup.rds")
write_rds(data_reg_an, "../Data/CATNAT/dataexplorer/data_reg_an.rds")

write_rds(data_dep_an_dedup, "../Data/CATNAT/dataexplorer/data_dep_an_dedup.rds")
write_rds(data_dep_an, "../Data/CATNAT/dataexplorer/data_dep_an.rds")

write_rds(data_com_an, "../Data/CATNAT/dataexplorer/data_com_an.rds")
