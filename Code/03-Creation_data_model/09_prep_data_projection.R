# -----------------------------------------------------------------------------#
# Préparation des données de projections pour les 3 scenarios du GIEC
# -----------------------------------------------------------------------------#


library(dplyr)
library(readr)
library(tidyr)
library(data.table)



# -----------------------------------------------------------------------------#
# data historique --------------------------------------------------------------
# -----------------------------------------------------------------------------#
dataFinaleCom <- read_rds("../../Data/Modelisation/data_modelisation_1982_2021_f.rds") %>% 
  filter(annee == 2020 | annee == 2021)


# -----------------------------------------------------------------------------#
# data projection 2050 ---------------------------------------------------------
# -----------------------------------------------------------------------------#
data26 <- read_rds("../../Data/DRIAS_Aladin/lissage/indic_aladin_2022_2050_RCP26.rds") 
data45 <- read_rds("../../Data/DRIAS_Aladin/lissage/indic_aladin_2022_2050_RCP45.rds")
data85 <- read_rds("../../Data/DRIAS_Aladin/lissage/indic_aladin_2022_2050_RCP85.rds")

# pour les données de spi
dataA2 <- read_rds("../../Data/DRIAS_Climsec/lissage/indic_climsec_2000_2050_A2.rds") 
dataB1 <- read_rds("../../Data/DRIAS_Climsec/lissage/indic_climsec_2000_2050_B1.rds") 
# Pour les données de swi
dataA2_new <- read_rds("../../Data/DRIAS_Climsec/lissage/indic_climsec_2020_2050_A2_new.rds") 
dataB1_new <- read_rds("../../Data/DRIAS_Climsec/lissage/indic_climsec_2020_2050_B1_new.rds") 

setDT(data26)
setDT(data45)
setDT(data85)
setDT(dataA2)
setDT(dataB1)
setDT(dataA2_new)
setDT(dataB1_new)

data85 <- data85 %>% merge(dataA2, by=c("annee","id","longM","latM")) %>%
  merge(dataA2_new, by=c("annee","id","longM","latM"))
data45 <- data45 %>% merge(dataB1, by=c("annee","id","longM","latM")) %>%
  merge(dataB1_new, by=c("annee","id","longM","latM"))
data26 <- data26 %>% merge(dataB1, by=c("annee","id","longM","latM")) %>%
  merge(dataB1_new, by=c("annee","id","longM","latM"))


# -----------------------------------------------------------------------------#
# Ajout de la variable historique 3 ans de 2021/2020 ------------------
# -----------------------------------------------------------------------------#
historique_3 <- dataFinaleCom %>% 
  filter(annee==2021) %>%
  select(id, inondationH3) %>%
  merge(dataFinaleCom %>% 
          filter(annee==2020) %>%
          select(id, secheresseH3), by = "id")


data85 <- data85 %>% merge(historique_3, by="id")
data45 <- data45 %>% merge(historique_3, by="id")
data26 <- data26 %>% merge(historique_3, by="id")


# -----------------------------------------------------------------------------#
# Ajout des expositions aux risques (RGA et PPRI) + altitudes ------------------
# -----------------------------------------------------------------------------#
rga2019Finale <- read_rds("../../Data/RGA/rga2019.rds")
ppriCom <- read_rds("../../Data/Gaspar/ppriCom.rds")
infos_com <- read_rds("../../Data/Administratif/infos_communes.rds")


rga2019Finale <- rga2019Finale %>% 
  select(code_insee, part_alea_faible_commune, part_alea_moyen_fort_commune,
         part_alea_faible_commune_cat, part_alea_moyen_fort_commune_cat, 
         part_alea)
data_altitudes <- infos_com %>% select(code_insee, altitude)

data_expo_com <- rga2019Finale %>%
  merge(ppriCom, by = "code_insee") %>%
  merge(data_altitudes, by=c("code_insee"), all.x=T)

data85 <- data85 %>% 
  merge(data_expo_com, by.x = "id", by.y = "code_insee") 

data45 <- data45 %>% 
  merge(data_expo_com, by.x = "id", by.y = "code_insee") 

data26 <- data26 %>% 
  merge(data_expo_com, by.x = "id", by.y = "code_insee") 



# -----------------------------------------------------------------------------#
# Changement de législation                                   ------------------
# -----------------------------------------------------------------------------#
data85 <- data85 %>%
  mutate(old_periode_sec = annee < 1999)
data45 <- data45 %>%
  mutate(old_periode_sec = annee < 1999)
data26 <- data26 %>%
  mutate(old_periode_sec = annee < 1999)


dir.create(file.path("../../Data/", "Projection"), showWarnings = FALSE)

write_rds(data85, "../../Data/projection/data_proj_2022_2050_RCP85_A2.rds")
write_rds(data45, "../../Data/projection/data_proj_2022_2050_RCP45_B1.rds")
write_rds(data26, "../../Data/projection/data_proj_2022_2050_RCP26_B1.rds")

