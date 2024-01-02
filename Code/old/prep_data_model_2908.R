# --------------------------------------------- #
# Date : 21/07/2022
# Auteur : E. Maucuer
# Description : Préparation de la base de 
# modélisation à la maille commune :
# Lissage des données aladin à la maille com
# + ajout des données catnat GASPAR
# + altitude moyenne des communes
# --------------------------------------------- #

library(data.table)
library(dplyr)
library(stringi)
library(readr)
library(tidyr)
library(sf)

# Récupération des données -----------------------------------------------------
## Data Map --------------------------------------------------------------------
# dataCarte=readRDS("../Data/dataCarte2")
# dataCoord1=dataCarte[,.(longM=mean(long),latM=mean(lat)),by=id]

# com_bound_wo_om <- read_rds("../Data_GIS/geojson/communes-vs.rds")
# dataCoord=com_bound_wo_om %>% select(code, longM, latM, -geometry) %>%
#   rename(id=code) %>%
#   arrange(id)

dataCoord <- read_rds(("../Data/coordCom2018.rds"))
dataCoord <- dataCoord %>% 
  rename(id = code) %>% 
  filter(id < 96000) %>%
  arrange(id)


# Data aladin 1982 2005
dataVar <- read_delim("../Data/aladin/indicesALADIN63_CNRM-CM5_1982_2005_all.txt",
                          delim = ";", skip = 44) # skip = 27


dataVar <- dataVar %>% 
  rename_with(~ tolower(gsub("#| ", "",.x))) %>%
  filter(! is.na(longitude)) %>%
  select(-starts_with("..")) %>%
  mutate(expo=1)

## Data aladin 2006 2021 -------------------------------------------------------
dataVar2006 <- read_delim("../Data/aladin/indicesALADIN63_CNRM-CM5_2006_2021_RCP45_all.txt",
                      delim = ";", skip = 44) # skip = 27)

dataVar2006 <- dataVar2006 %>% 
  rename_with(~ tolower(gsub("#| ", "",.x))) %>%
  filter(! is.na(longitude)) %>%
  select(-starts_with("..")) %>%
  mutate(expo=1)

dataVar = bind_rows(dataVar, dataVar2006)

# 
list_indicateurs = setdiff(names(dataVar), c("point","latitude","longitude","contexte","annee", "expo"))
list_annee = min(dataVar$annee):max(dataVar$annee)



# Lissage des indicateurs pour toutes les années et communes--------------------

## Fonction de lissage ---------------------------------------------------------
fct_lissage <- function(P1, coord){
  dataFinale = data.frame()
  for (year in list_annee) {
    print(year)
    dataVar1Y <- dataVar %>% filter(annee==year)
    dataFinale1Y <- coord %>% mutate(annee = year)
    
    for (var in list_indicateurs){
      dataVar1Y <- dataVar1Y %>% rename(indicateur = var)
      
      dataFinale1Y$indicateur = 
        sapply(1:dim(dataFinale1Y)[1], function(i) sum(dataVar1Y$indicateur*exp(-sqrt((dataFinale1Y$long[i] - dataVar1Y$longitude)^2 + 
                                                    (dataFinale1Y$lat[i] - dataVar1Y$latitude)^2)*P1)*dataVar1Y$expo) / sum(exp(-(sqrt((dataFinale1Y$long[i] - dataVar1Y$longitude)^2 + (dataFinale1Y$lat[i] - dataVar1Y$latitude)^2))*P1)*dataVar1Y$expo))
      
      dataFinale1Y <- dataFinale1Y %>% rename(!!var := indicateur)
      dataVar1Y <- dataVar1Y %>% rename(!!var := indicateur)
    }
    
    dataFinale <- dataFinale %>% bind_rows(dataFinale1Y)
  }
  return(dataFinale)
}

## Premier lissage à 1e4 -------------------------------------------------------
dataFinale1 <- fct_lissage(1e4, dataCoord)

## Deuxieme lissage pour les communes problématiques à 1e3 ---------------------
dataCoord_relance <- unique(dataFinale1 %>% 
                        filter(is.na(tav)) %>% 
                        select(id, longM, latM))

dataFinale2 <- fct_lissage(1e3, dataCoord_relance)

## Fusion des 2 lissages -------------------------------------------------------
dataFinale <- dataFinale1 %>% 
  filter(!is.na(txav)) %>%
  bind_rows(dataFinale2)

## Gestion de Paris, Marseille et Lyon -----------------------------------------
paris <- dataFinale %>% 
  filter(substr(id,1,2)==75) %>%
  select(-id) %>%
  group_by(annee) %>%
  summarise_all(mean) %>%
  mutate(id = "75056")
 

id_marseille = 13201:13216
marseille <- dataFinale %>% 
  filter(id %in% id_marseille) %>%
  select(-id) %>%
  group_by(annee) %>%
  summarise_all(mean) %>%
  mutate(id = "13055")


id_lyon = 69381:69389
lyon <- dataFinale %>% 
  filter(id %in% id_lyon) %>%
  select(-id) %>%
  group_by(annee) %>%
  summarise_all(mean) %>%
  mutate(id = "69123")
  


dataFinale <- dataFinale %>% 
  filter(!id %in% id_lyon & 
           !id %in% id_marseille & 
           substr(id,1,2)!=75) %>%
  bind_rows(paris) %>%
  bind_rows(marseille) %>%
  bind_rows(lyon)


write_rds(dataFinale, "../Data/indic_aladin_1982_2021_all_V2.rds")




# Ajout des CatNat GASPAR ------------------------------------------------------
gaspar <- readRDS("../Data/gaspar.rds")

gaspar_an <- gaspar %>%
  group_by(cod_commune, an_debut) %>%
  summarise(inondation = sum(inondation),
            secheresse = sum(secheresse))
gaspar_sum <- gaspar %>%
  group_by(cod_commune) %>%
  summarise(count =n())

# # Test de merge all 
# dataFinale <- read_rds("../Data/indic_aladin_1982_2021.rds")
# dataFinale <- dataFinale %>%
#   mutate(aladin=1) %>%
#   merge(gaspar_an %>% mutate(catnat=1),
#         by.x=c("id", "annee"), by.y=c("cod_commune", "an_debut"),
#         all=T)

# # Verifications des communes
# data_com <- dataFinale %>% group_by(id) %>%
#   summarize(aladin = max(aladin, na.rm=T),
#             catnat = max(catnat, na.rm=T)) %>%
#   mutate(dep = ifelse(substr(id,1,2) < 96,
#                       substr(id,1,2), substr(id,1,3)))
# 
# table(data_com %>% filter(aladin == -Inf) %>% select(dep))
# dep
# 971 972 973 974 976 
# 32  34  16  24  17 




## Ajout des infos gaspar dans la base finale ----------------------------------
dataFinale <- read_rds("../Data/indic_aladin_1982_2021_all.rds")
dataFinale <- dataFinale %>%
  merge(gaspar_an, by.x=c("id", "annee"), 
        by.y=c("cod_commune", "an_debut"),
        all.x=T) %>%
  mutate(inondation = replace_na(inondation,0),
         secheresse = ifelse(annee>=1988, replace_na(secheresse,0), NA))


## Historique 3 ans ------------------------------------------------------------
catnat3ans <- dataFinale %>%
  left_join(dataFinale %>% mutate(annee=annee+1) %>% select(id, annee, inondation, secheresse), 
            by = c("id", "annee"), suffix = c("", "1")) %>%
  left_join(dataFinale %>% mutate(annee=annee+2) %>% select(id, annee, inondation, secheresse), 
            by = c("id", "annee"), suffix = c("", "2")) %>%
  left_join(dataFinale %>% mutate(annee=annee+3) %>% select(id, annee, inondation, secheresse), 
            by = c("id", "annee"), suffix = c("", "3")) %>%
  mutate(inondationH3 = inondation1+inondation2+inondation3,
         secheresseH3 = secheresse1+secheresse2+secheresse3) %>%
  select(id, annee, inondationH3, secheresseH3)


dataFinale <- dataFinale %>% merge(catnat3ans, by = c("id", "annee"))


# Ajout des altitudes ----------------------------------------------------------
infos_com <- readRDS("../Data/infos_communes.rds")

dataFinale <- dataFinale %>%
  merge(infos_com %>% select(code_insee, commune, altitude) ,
        by.x=c("id"), by.y=c("code_insee"),
        all.x=T)

table(is.na(dataFinale$altitude))
table(dataFinale %>% filter(is.na(altitude)) %>% select(id))
# id
# 55138 76095 
# 40    40 

dataFinale <- dataFinale %>% 
  mutate(altitude = ifelse(id=="55138", 251,
                                   ifelse(id=="76095", 53, altitude)))


# Ajout du code département ----------------------------------------------------
dataFinale <- dataFinale %>% mutate(dep = ifelse(substr(id,1,2) < 96,
                                                       substr(id,1,2), substr(id,1,3)))


  
# Enregistrement ---------------------------------------------------------------
write_rds(dataFinale, "../Data/data_modelisation_1982_2021_all.rds")


