# --------------------------------------------- #
# Auteur : E. Maucuer
# Description : Fonction de lissage
# --------------------------------------------- #


library(dplyr)
library(readr)
library(data.table)


# Lissage -----------------------------------------------------------------

# P1 : parametre de lissage
# dfvars : dataframe des valeurs à lisser pour chaque année
# dfcoord : dataframe des coordonnées finales (longM, latM) que l'on souhaite
# dataVar1Y contient les indicateurs pour une annee
# dataFinale1Y contient les indicateurs pour une annee pour les points souhaités

fct_lissage <- function(P1, dfcoord, dfvars, list_an, list_indic){
  dataFinale = data.frame()
  for (year in list_an) {
    print(year)
    dataVar1Y <- dfvars %>% filter(annee==year)
    dataFinale1Y <- dfcoord %>% mutate(annee = year)
    
    for (var in list_indic){
      dataVar1Y <- dataVar1Y %>% dplyr::rename(indicateur = var)
      
      dataFinale1Y$indicateur = 
        sapply(1:dim(dataFinale1Y)[1], function(i) sum(dataVar1Y$indicateur*exp(-sqrt((dataFinale1Y$long[i] - dataVar1Y$longitude)^2 + 
                                                                                        (dataFinale1Y$lat[i] - dataVar1Y$latitude)^2)*P1)) / 
                 sum(exp(-(sqrt((dataFinale1Y$long[i] - dataVar1Y$longitude)^2 + (dataFinale1Y$lat[i] - dataVar1Y$latitude)^2))*P1)))
      
      dataFinale1Y <- dataFinale1Y %>% dplyr::rename(!!var := indicateur)
      dataVar1Y <- dataVar1Y %>% dplyr::rename(!!var := indicateur)
    }
    
    dataFinale <- dataFinale %>% bind_rows(dataFinale1Y)
  }
  return(dataFinale)
}