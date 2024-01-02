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
dataFinale <- read_rds("../../Data/indic_aladin_1982_2021_all_V2.rds")
climsecA1B <- read_rds("../../Data/indic_climsec_2000_2050_A1B.rds")
climsecA2 <- read_rds("../../Data/indic_climsec_2000_2050_A2.rds")
climsecB1 <- read_rds("../../Data/indic_climsec_2000_2050_B1.rds")
climsecHist <- read_rds("../../Data/indic_climsec_1982_1999.rds")
new_sswi <- read_rds("../../Data/sswi/final_sswi_1982_2021.rds")


#------------------------------------------------------------------------------#
# Ajout des CatNat GASPAR ------------------------------------------------------
#------------------------------------------------------------------------------#
## Load data gaspar et aggrégation annuelle ------------------------------------
gaspar_an <- read_rds("../../Data/Gaspar/gaspar_ag.rds")


## Ajout des infos gaspar dans la base finale ----------------------------------
setDT(dataFinale)
setDT(gaspar_an)
dataFinale <- merge(dataFinale, gaspar_an, 
                     by.x=c("id", "annee"), by.y=c("cod_commune", "an_debut"), # remplacer par annee pour pluri annuel
                     all.x=T) 
dataFinale <- dataFinale %>%  
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


# Ajout des altitudes + departement --------------------------------------------
infos_com <- readRDS("../Data/infos_communes.rds")

dataFinale <- dataFinale %>%
  merge(infos_com %>% select(code_insee, commune, altitude, code_departement) ,
        by.x=c("id"), by.y=c("code_insee"),
        all.x=T) %>%
  rename(code_dep = code_departement)

table(is.na(dataFinale$altitude)) #aucune altitude manquante
table(is.na(dataFinale$dep)) #aucun departement manquant



# Ajout des expositions aux risques (RGA et PPRI) ------------------------------
rga2019Finale <- read_rds("../Data/CATNAT/rga2019.rds")
ppriCom <- read_rds("../Data/CATNAT/ppriCom.rds")

rga2019Finale <- rga2019Finale %>% 
  select(code_insee, part_alea_faible_commune, part_alea_moyen_fort_commune,
         part_alea_faible_commune_cat, part_alea_moyen_fort_commune_cat, 
         part_alea)

dataFinale <- dataFinale %>% 
  merge(rga2019Finale, by.x = "id", by.y = "code_insee") %>%
  merge(ppriCom, by.x = "id", by.y = "code_insee") %>%
  relocate(c(commune, code_dep, inondation, secheresse), .after=id)



# Ajout des indicateurs de secheresse ------------------------------------------
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
dataFinale_sswi <- merge(dataFinale, new_sswi %>% select(-longM,-latM), 
                         by = c("id", "annee"), all.x=T)


# Enregistrement ---------------------------------------------------------------
write_rds(dataFinale, "../Data/modelisation/data_modelisation_1982_2021_all_V2.rds")
# write_rds(dataFinaleA1B, "../Data/modelisation/data_modelisation_1982_2021_all_V2_A1B.rds")
# write_rds(dataFinaleB1, "../Data/modelisation/data_modelisation_1982_2021_all_V2_B1.rds")
# write_rds(dataFinaleA2, "../Data/modelisation/data_modelisation_1982_2021_all_V2_A2.rds")
write_rds(dataFinaleClimsec, "../Data/modelisation/data_modelisation_1982_2021_all_v2_climsec.rds")
write_rds(dataFinale_sswi, "../Data/modelisation/data_modelisation_1982_2021_f.rds")


