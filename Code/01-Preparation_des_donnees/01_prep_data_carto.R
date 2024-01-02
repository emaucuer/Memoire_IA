# --------------------------------------------- #
# Date : 28/08/2022
# Auteur : E. Maucuer
# Description : Nettoyage et creation de fchiers
# rds des données de cartographie par commune et 
# par département
# --------------------------------------------- #


library(dplyr)
library(data.table)
library(sf)
library(readr)


# FICHIERS GEOJSON -------------------------------------------------------------
## Départements ----------------------------------------------------------------
dep_bound <- sf::st_read("../../Data/Data_GIS/geojson/departements-version-simplifiee.geojson")
dep_om <- sf::st_read("../../Data/Data_GIS/geojson/departements-avec-outre-mer.geojson")

dep_om <- dep_om %>% filter(nchar(code)==3)
dep_bound <- rbind(dep_bound, dep_om)

write_rds(dep_bound, "../../Data/Data_GIS/geojson/departements-vs-om.rds")


## Communes --------------------------------------------------------------------
com_bound <- sf::st_read("../../Data/Data_GIS/geojson/communes-version-simplifiee.geojson")
com_detail <- sf::st_read("../../Data/Data_GIS/geojson/communes-avec-outre-mer.geojson")

# lat et long moyenne par commune
newCoord <- sf::st_cast(com_detail, to = "POINT")
newCoord <- sf::st_coordinates(newCoord$geometry) %>%
  bind_cols(newCoord)

setDT(newCoord)
newCoord <- newCoord[,.(longM = mean(X),
                        latM = mean(Y)),
                     by = code]

write_rds(newCoord, "../../Data/Administratif/coordCom2018.rds")

# Version détaillee
com_detail <- com_detail %>% merge(newCoord, by = c("code"))
write_rds(com_detail, "../../Data/Data_GIS/geojson/communes-om.rds")


# version simplifiée
com_om <- com_detail %>% filter(code >= 96000) %>% select(-latM, -longM)
com_bound <- rbind(com_bound, com_om)
com_bound <- com_bound %>% merge(newCoord, by = c("code"))

com_bound_wo_om <- com_bound %>% filter(code < 96000)

write_rds(com_bound, "../../Data/Data_GIS/geojson/communes-vs-om.rds")
write_rds(com_bound_wo_om, "../../Data/Data_GIS/geojson/communes-vs.rds")




