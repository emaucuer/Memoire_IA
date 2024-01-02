

# Libraries
library(readr)
library(dplyr)
library(readxl)
library(stringi)
library(lubridate)
library(tidyr)
library(data.table)
library(stats)
library(tidyverse)



gaspar <- read_rds("../../Data/Gaspar/gaspar.rds")
code_catnat <- read_csv2("../../Data/Gaspar/code_jo_catnat.csv")
coord_com <- read_rds("../../Data/Administratif/coordCom2018.rds") %>% 
  filter(code < 96000) %>% arrange(code)

# gestion des donnees gaspar
gaspar <- gaspar %>%
  merge(code_catnat %>% select(Nom, nom_simple, Code), by.x = "num_risque_jo", by.y = "Code")


# Agregation par annee et peril
gaspar_test <- gaspar %>% 
  group_by(an_debut,cod_commune, nom_simple) %>%
  summarise(nb_cat_nat = n())

catnat_an_peril <- as.data.frame(table(gaspar_test$an_debut, gaspar_test$nom_simple)) %>%
  mutate(Freq = as.integer(Freq)) %>%
  spread(key = "Var2",value = "Freq") %>%
  rename("annee"="Var1") %>%
  mutate(annee = as.integer(as.character(annee))) %>%
  mutate(secheresse = ifelse(annee < 1988, NA, secheresse))


# Agregation par an peril commune
gaspar_ag_fin <- gaspar %>% 
  group_by(an_debut,cod_commune, nom_simple) %>%
  summarise(nb_cat_nat = n()) %>%
  spread(key = "nom_simple",value = "nb_cat_nat")


gaspar_ag_gros <- gaspar %>% 
  mutate(lib_risque_jo2 = str_replace(stri_trans_general(tolower(lib_risque_jo2),"Latin-ASCII"), " ", "_")) %>%
  group_by(an_debut,cod_commune, lib_risque_jo2) %>% 
  summarise(nb_cat_nat = n()) %>%
  spread(key = "lib_risque_jo2",value = "nb_cat_nat")



# Base avec toutes les communes pour toutes les annees
cod_commune <- rep(coord_com$code, length(1982:2021))
annee <- sort(rep(1982:2021,length(coord_com$code)))

data_com_an <- data.table(annee, cod_commune)


# Bases finales par commune
catnat_com_1982_2021_details <- data_com_an %>%
  merge(gaspar_ag_fin, by.x = c("annee","cod_commune"),
        by.y = c("an_debut", "cod_commune"), all = T) %>%
  replace(is.na(.), 0) %>%
  mutate(secheresse = ifelse(annee < 1988, NA, secheresse))

catnat_com_1982_2021_ag <- data_com_an %>%
  merge(gaspar_ag_gros, by.x = c("annee","cod_commune"), 
        by.y = c("an_debut", "cod_commune"), all = T) %>%
  replace(is.na(.), 0) %>%
  mutate(secheresse = ifelse(annee < 1988, NA, secheresse))



# Enregistrement
write_csv2(catnat_an_peril, "../../Data/Gaspar/output/catnat_an_peril.csv")
write_csv2(catnat_com_1982_2021_details, "../../Data/Gaspar/output/catnat_com_1982_2021_details.csv")
write_csv2(catnat_com_1982_2021_ag, "../../Data/Gaspar/output/catnat_com_1982_2021_ag.csv")



