# --------------------------------------------- #
# Date : 21/07/2022
# Auteur : E. Maucuer
# Description : Création de la base de 
# modélisation à la maille commune :
# Merge data aladin lissées avec
# + les données catnat GASPAR
# + altitude moyenne des communes
# + data RGA et PPRN
# + création de l'historique 3 ans
# --------------------------------------------- #

library(data.table)
library(dplyr)
library(stringi)
library(readr)
library(tidyr)
library(sf)

#------------------------------------------------------------------------------#
# Récupération des données -----------------------------------------------------
#------------------------------------------------------------------------------#
dataAladin <- read_rds("../../Data/DRIAS_Aladin/lissage/indic_aladin_1982_2021_all_V2.rds")
climsecA1B <- read_rds("../../Data/DRIAS_Climsec/lissage/indic_climsec_2000_2050_A1B.rds")
climsecA2 <- read_rds("../../Data/DRIAS_Climsec/lissage/indic_climsec_2000_2050_A2.rds")
climsecB1 <- read_rds("../../Data/DRIAS_Climsec/lissage/indic_climsec_2000_2050_B1.rds")
climsecHist <- read_rds("../../Data/DRIAS_Climsec/lissage/indic_climsec_1982_1999.rds")
new_sswi <- read_rds("../../Data/sswi/final_sswi_1982_2021.rds")
gaspar_ag_an <- read_rds("../../Data/Gaspar/gaspar_ag.rds")

#------------------------------------------------------------------------------#
# Ajout des CatNat GASPAR ------------------------------------------------------
#------------------------------------------------------------------------------#
setDT(dataAladin)
setDT(gaspar_ag_an)
dataFinale <- merge(dataAladin, gaspar_ag_an, 
                    by.x=c("id", "annee"), 
                    by.y=c("cod_commune", "annee"),
                    all.x=T) 

dataFinale <- dataFinale %>%  
  mutate(inondation = replace_na(inondation,0),
         inondation_pl = replace_na(inondation_pl,0),
         secheresse = ifelse(annee>=1988, replace_na(secheresse,0), NA),
         secheresse_pl = ifelse(annee>=1988, replace_na(secheresse_pl,0), NA))


## Historique 3 ans ------------------------------------------------------------
list_var <- c("id", "annee", "inondation", "secheresse", "inondation_pl", "secheresse_pl")

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

#------------------------------------------------------------------------------#
# Ajout des altitudes + departement --------------------------------------------
#------------------------------------------------------------------------------#
infos_com <- readRDS("../../Data/Administratif/infos_communes.rds")

dataFinale <- dataFinale %>%
  merge(infos_com %>% select(code_insee, commune, altitude, code_departement) ,
        by.x=c("id"), by.y=c("code_insee"),
        all.x=T) %>%
  rename(code_dep = code_departement)

table(is.na(dataFinale$altitude)) #aucune altitude manquante
table(is.na(dataFinale$dep)) #aucun departement manquant



#------------------------------------------------------------------------------#
# Ajout des expositions aux risques (RGA et PPRI) ------------------------------
#------------------------------------------------------------------------------#
rga2019Finale <- read_rds("../../Data/RGA/rga2019.rds")
ppriCom <- read_rds("../../Data/Gaspar/ppriCom.rds")

rga2019Finale <- rga2019Finale %>% 
  select(code_insee, part_alea_faible_commune, part_alea_moyen_fort_commune,
         part_alea_faible_commune_cat, part_alea_moyen_fort_commune_cat, 
         part_alea)

dataFinale <- dataFinale %>% 
  merge(rga2019Finale, by.x = "id", by.y = "code_insee") %>%
  merge(ppriCom, by.x = "id", by.y = "code_insee") %>%
  relocate(c(commune, code_dep, inondation, secheresse), .after=id)


#------------------------------------------------------------------------------#
# Ajout des indicateurs de secheresse ------------------------------------------
#------------------------------------------------------------------------------#

## Donnees CLIMSEC
climsecA1B <- bind_rows(climsecA1B, climsecHist) %>% 
  rename(spi_A1B = spi, sswi_A1B = sswi) %>%
  select(-longM, -latM)
climsecA2 <- bind_rows(climsecA2, climsecHist) %>% 
  rename(spi_A2 = spi, sswi_A2 = sswi) %>%
  select(-longM, -latM)
climsecB1 <- bind_rows(climsecB1, climsecHist) %>% 
  rename(spi_B1 = spi, sswi_B1 = sswi) %>%
  select(-longM, -latM)

setDT(climsecA1B)
setDT(climsecB1)
setDT(climsecA2)

climsec_all <- climsecA1B %>% 
  merge(climsecB1, by = c("id", "annee")) %>%
  merge(climsecA2 , by = c("id", "annee"))

dataFinaleClimsec <- merge(dataFinale, climsec_all, by = c("id", "annee"), all.x=T)


## Donnees Meteo France retraitees
dataFinale_sswi <- merge(dataFinaleClimsec, new_sswi %>% select(-longM,-latM), 
                         by = c("id", "annee"), all.x=T)


# Enregistrement ---------------------------------------------------------------
write_rds(dataFinale_sswi, "../Data/modelisation/data_modelisation_1982_2021_f.rds")


