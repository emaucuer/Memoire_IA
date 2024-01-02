# --------------------------------------------- #
# Date : 21/07/2022
# Auteur : E. Maucuer
# Description : Préparation de la base de 
# modélisation à la maille commune :
# Lissage des données aladin à la maille com
# --------------------------------------------- #

library(data.table)
library(dplyr)
library(stringi)
library(readr)
library(tidyr)
library(sf)


# Lissage des indicateurs pour toutes les années et communes--------------------

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
      dataVar1Y <- dataVar1Y %>% rename(indicateur = var)
      
      dataFinale1Y$indicateur = 
        sapply(1:dim(dataFinale1Y)[1], function(i) sum(dataVar1Y$indicateur*exp(-sqrt((dataFinale1Y$long[i] - dataVar1Y$longitude)^2 + 
                                                                                        (dataFinale1Y$lat[i] - dataVar1Y$latitude)^2)*P1)*dataVar1Y$expo) / 
                 sum(exp(-(sqrt((dataFinale1Y$long[i] - dataVar1Y$longitude)^2 + (dataFinale1Y$lat[i] - dataVar1Y$latitude)^2))*P1)*dataVar1Y$expo))
      
      dataFinale1Y <- dataFinale1Y %>% rename(!!var := indicateur)
      dataVar1Y <- dataVar1Y %>% rename(!!var := indicateur)
    }
    
    dataFinale <- dataFinale %>% bind_rows(dataFinale1Y)
  }
  return(dataFinale)
}


#------------------------------------------------------------------------------#
# Récupération des données de mapping ------------------------------------------
#------------------------------------------------------------------------------#
dataCoord <- read_rds(("../Data/coordCom2018.rds"))
dataCoord <- dataCoord %>% 
  rename(id = code) %>% 
  filter(id < 96000) %>%
  arrange(id)


#------------------------------------------------------------------------------#
# I) Data aladin 1982 2021 -----------------------------------------------------
#------------------------------------------------------------------------------#

## 1) Data historique (1982-2005) ----------------------------------------------
dataVar <- read_delim("../Data/aladin/indicesALADIN63_CNRM-CM5_1982_2005_all.txt",
                      delim = ";", skip = 44) # skip = 27


dataVar <- dataVar %>% 
  rename_with(~ tolower(gsub("#| ", "",.x))) %>%
  filter(! is.na(longitude)) %>%
  select(-starts_with("..")) %>%
  mutate(expo=1)



## 2) Data projection (2006 2021) ----------------------------------------------
dataVar2006 <- read_delim("../Data/aladin/indicesALADIN63_CNRM-CM5_2006_2021_RCP45_all.txt",
                          delim = ";", skip = 44) # skip = 27)

dataVar2006 <- dataVar2006 %>% 
  rename_with(~ tolower(gsub("#| ", "",.x))) %>%
  filter(! is.na(longitude)) %>%
  select(-starts_with("..")) %>%
  mutate(expo=1)

data1982_2021 = bind_rows(dataVar, dataVar2006)



## 3) Définition des paramètres ------------------------------------------------
list_indicateurs = setdiff(names(data1982_2021), c("point","latitude","longitude","contexte","annee", "expo"))
list_annee = min(data1982_2021$annee):max(data1982_2021$annee)



## 4) Lissage ------------------------------------------------------------------
#### a) 1er lissage à 1e4 ------------------------------------------------------
dataFinale1 <- fct_lissage(1e4, dataCoord, data1982_2021, 1982:1983, list_indicateurs)

#### b) 2eme lissage pour les communes problématiques à 1e3 --------------------
dataCoord_relance <- unique(dataFinale1 %>% 
                              filter(is.na(tav)) %>% 
                              select(id, longM, latM))

dataFinale2 <- fct_lissage(1e3, dataCoord_relance, data1982_2021, 1982:1983, list_indicateurs)

#### c) Fusion des 2 lissages --------------------------------------------------
dataFinale <- dataFinale1 %>% 
  filter(!is.na(tav)) %>%
  bind_rows(dataFinale2)



## 5) Enregistrement -----------------------------------------------------------
# write_rds(dataFinale, "../Data/indic_aladin_1982_2021_all_V2.rds")



#------------------------------------------------------------------------------#
# II) Data aladin projection RCP 2.6 -------------------------------------------
#------------------------------------------------------------------------------#



## 1) Chargement des données ---------------------------------------------------
dataInitiale26 <- read_delim("../Data/aladin/indicesALADIN63_CNRM-CM5_2006_2050_RCP26.txt",
                      delim = ";", skip = 44) # skip = 27
dataInitiale26 <- dataInitiale26 %>% 
  rename_with(~ tolower(gsub("#| ", "",.x))) %>%
  filter(! is.na(longitude)) %>%
  select(-starts_with("..")) %>%
  mutate(expo=1)

## PARAMETRES --------------------------------------------------------
list_annee_proj = 2022:2050
list_indicateurs = setdiff(names(dataInitiale26), c("point","latitude","longitude","contexte","annee", "expo"))


## 2) Lissage ------------------------------------------------------------------
#### a) 1er lissage à 1e4 ------------------------------------------------------
dataFinale26_1 <- fct_lissage(1e4, dataCoord, dataInitiale26, list_annee_proj, list_indicateurs)

#### b) 2eme lissage pour les communes problématiques à 1e3 --------------------
dataCoord_relance26 <- unique(dataFinale26_1 %>% 
                              filter(is.na(tav)) %>% 
                              select(id, longM, latM))

dataFinale26_2 <- fct_lissage(1e3, dataCoord_relance26, dataInitiale26, list_annee_proj, list_indicateurs)

#### c) Fusion des 2 lissages --------------------------------------------------
dataFinale26 <- dataFinale26_1 %>% 
  filter(!is.na(tav)) %>%
  bind_rows(dataFinale26_2)



## 3) Enregistrement -----------------------------------------------------------
write_rds(dataFinale26, "../Data/indic_aladin_2022_2050_RCP26.rds")




#------------------------------------------------------------------------------#
# III) Data aladin projection RCP 4.5 ------------------------------------------
#------------------------------------------------------------------------------#

## 1) Chargement des données ---------------------------------------------------
dataInitiale45 <- read_delim("../Data/aladin/indicesALADIN63_CNRM-CM5_2006_2050_RCP45.txt",
                             delim = ";", skip = 44) # skip = 27
dataInitiale45 <- dataInitiale45 %>% 
  rename_with(~ tolower(gsub("#| ", "",.x))) %>%
  filter(! is.na(longitude)) %>%
  select(-starts_with("..")) %>%
  mutate(expo=1)



## PARAMETRES --------------------------------------------------------
list_annee_proj = 2022:2050
list_indicateurs = setdiff(names(dataInitiale45), c("point","latitude","longitude","contexte","annee", "expo"))



## 2) Lissage ------------------------------------------------------------------
#### a) 1er lissage à 1e4 ------------------------------------------------------
dataFinale45_1 <- fct_lissage(1e4, dataCoord, dataInitiale45, list_annee_proj, list_indicateurs)

#### b) 2eme lissage pour les communes problématiques à 1e3 --------------------
dataCoord_relance45 <- unique(dataFinale45_1 %>% 
                                filter(is.na(tav)) %>% 
                                select(id, longM, latM))

dataFinale45_2 <- fct_lissage(1e3, dataCoord_relance45, dataInitiale45, list_annee_proj, list_indicateurs)

#### c) Fusion des 2 lissages --------------------------------------------------
dataFinale45 <- dataFinale45_1 %>% 
  filter(!is.na(tav)) %>%
  bind_rows(dataFinale45_2)



## 3) Enregistrement -----------------------------------------------------------
write_rds(dataFinale45, "../Data/indic_aladin_2022_2050_RCP45.rds")



#------------------------------------------------------------------------------#
# IV) Data aladin projection RCP 8.5 -------------------------------------------
#------------------------------------------------------------------------------#

## 1) Chargement des données ---------------------------------------------------
dataInitiale85 <- read_delim("../Data/aladin/indicesALADIN63_CNRM-CM5_2006_2050_RCP85.txt",
                             delim = ";", skip = 44) # skip = 27
dataInitiale85 <- dataInitiale85 %>% 
  rename_with(~ tolower(gsub("#| ", "",.x))) %>%
  filter(! is.na(longitude)) %>%
  select(-starts_with("..")) %>%
  mutate(expo=1)



## PARAMETRES --------------------------------------------------------
list_annee_proj = 2022:2050
list_indicateurs = setdiff(names(dataInitiale85), c("point","latitude","longitude","contexte","annee", "expo"))



## 2) Lissage ------------------------------------------------------------------
#### a) 1er lissage à 1e4 ------------------------------------------------------
dataFinale85_1 <- fct_lissage(1e4, dataCoord, dataInitiale85, list_annee_proj, list_indicateurs)

#### b) 2eme lissage pour les communes problématiques à 1e3 --------------------
dataCoord_relance85 <- unique(dataFinale85_1 %>% 
                                filter(is.na(tav)) %>% 
                                select(id, longM, latM))

dataFinale85_2 <- fct_lissage(1e3, dataCoord_relance85, dataInitiale85, list_annee_proj, list_indicateurs)

#### c) Fusion des 2 lissages --------------------------------------------------
dataFinale85 <- dataFinale85_1 %>% 
  filter(!is.na(tav)) %>%
  bind_rows(dataFinale85_2)



## 3) Enregistrement -----------------------------------------------------------
write_rds(dataFinale85, "../Data/indic_aladin_2022_2050_RCP85.rds")






#------------------------------------------------------------------------------#
# V) Data Climsec historique (non testé)        --------------------------------------------
#------------------------------------------------------------------------------#

## 1) Chargement des données ---------------------------------------------------
dataInitialeClimsec <- read_delim("../Data/aladin/indicesARPEGE_RETIC_1982_1999.txt",
                             delim = ";", skip = 18) # skip = 27
dataInitialeClimsec <- dataInitialeClimsec %>% 
  rename_with(~ tolower(gsub("#| ", "",.x))) %>%
  filter(! is.na(longitude)) %>%
  select(-starts_with("..")) %>%
  mutate(expo=1)



## PARAMETRES --------------------------------------------------------
list_anneeC = min(dataInitialeClimsec$annee):max(dataInitialeClimsec$annee)
list_indicateursC = setdiff(names(dataInitialeClimsec), c("point","latitude","longitude","contexte","annee", "expo"))



## 2) Lissage ------------------------------------------------------------------
#### a) 1er lissage à 1e4 ------------------------------------------------------
dataFinaleC_1 <- fct_lissage(1e4, dataCoord, dataInitialeClimsec, list_anneeC, list_indicateursC)

#### b) 2eme lissage pour les communes problématiques à 1e3 --------------------
dataCoord_relanceC <- unique(dataFinaleC_1 %>% 
                                filter(is.na(tav)) %>% 
                                select(id, longM, latM))

dataFinaleC_2 <- fct_lissage(1e3, dataCoord_relanceC, dataInitialeClimsec, list_anneeC, list_indicateursC)

#### c) Fusion des 2 lissages --------------------------------------------------
dataFinaleC <- dataFinaleC_1 %>% 
  filter(!is.na(tav)) %>%
  bind_rows(dataFinaleC_2)



## 3) Enregistrement -----------------------------------------------------------
write_rds(dataFinaleC, "../Data/indic_climsec_1982_1999.rds")
