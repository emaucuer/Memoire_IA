# --------------------------------------------- #
# Date : 21/07/2022
# Auteur : E. Maucuer
# Description : Préparation de la base de 
# modélisation à la maille département à partir
# de celle à la maille commune
# --------------------------------------------- #

library(dplyr)
library(readr)
library(sf)


# Chargement des donnees -------------------------------------------------------
dep_bound <- sf::st_read("../Data_GIS/AdminExpress/DEPARTEMENT.shp")

dataFinaleCom <- read_rds("../Data/modelisation/data_modelisation_1982_2021_all_V2.rds")
sapply(dataFinaleCom, function(x) sum(is.na(x)))



# Liste des idicateurs ---------------------------------------------------------
list_var <- setdiff(names(dataFinaleCom), 
                    c("id","annee","longM","latM","inondation", "secheresse",
                      "commune", "indic_in", "code_dep",
                      "part_alea_faible_commune",       
                      "part_alea_moyen_fort_commune","part_alea_faible_commune_cat",  
                      "part_alea_moyen_fort_commune_cat"))


# Agregation par departement ---------------------------------------------------
dataFinaleDep <- dataFinaleCom %>%
  group_by(code_dep, annee) %>%
  summarise_at(list_var, mean) %>%
  merge(dataFinaleCom %>% 
              group_by(code_dep, annee) %>%
              summarise(nb_commune = n(),
                        inondation = sum(inondation),
                        secheresse = sum(secheresse)),
        by = c("code_dep", "annee"))
 
# Ajout des indicateurs d'exposition -------------------------------------------
rga2019Dep <- read_rds("../Data/CATNAT/rga2019Dep.rds")

rga2019Dep <- rga2019Dep %>% 
  select(departement, part_alea_faible_dep, part_alea_moyen_fort_dep,
         part_alea_faible_dep_cat, part_alea_moyen_fort_dep_cat, 
         part_alea)

dataFinaleDep <- dataFinaleDep %>% 
  merge(rga2019Dep, by.x = "code_dep", by.y = "departement") 
  
# Enregistrement ---------------------------------------------------------------
write_rds(dataFinaleDep, "../Data/modelisation/data_modelisation_dep_1982_2021_all_V2.rds")
