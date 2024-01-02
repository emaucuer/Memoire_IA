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



#------------------------------------------------------------------------------#
# Récupération des données de mapping ------------------------------------------
#------------------------------------------------------------------------------#
dataCoord <- read_rds("../../Data/Administratif/coordCom2018.rds")
dataCoord <- dataCoord %>% 
  dplyr::rename(id = code) %>% 
  filter(id < 96000) %>%
  arrange(id)


#------------------------------------------------------------------------------#
# Fct de lissage pour tous les indicateurs, années et communes -----------------
#------------------------------------------------------------------------------#

source("../00_lissage.R")

# P1 : parametre de lissage
# dfvars : dataframe des valeurs à lisser pour chaque année
# dfcoord : dataframe des coordonnées finales (longM, latM) que l'on souhaite
# dataVar1Y contient les indicateurs pour une annee
# dataFinale1Y contient les indicateurs pour une annee pour les points souhaités




#------------------------------------------------------------------------------#
# Fonction de lissage global ---------------------------------------------------
#------------------------------------------------------------------------------#
# dfvars : dataframe des valeurs à lisser pour chaque année
# dfcoord : dataframe des coordonnées finales (longM, latM) que l'on souhaite
# list_an : list des années à lisser
# name_save : nom du fichier à enregistrer


preproc_data <- function(dfcoord, dfvars, list_an, name_save, save = F){
  
  ## 1) Nettoyage des données --------------------------------------------------
  dfvars <- dfvars %>% 
    rename_with(~ tolower(gsub("#| ", "",.x))) %>%
    filter(! is.na(longitude)) %>%
    select(-starts_with("..")) %>%
    mutate(expo=1)
  
  
  ## 2) Paramètres -------------------------------------------------------------
  list_indicateurs = setdiff(names(dfvars), c("point","latitude","longitude","contexte","annee", "expo"))

  ## 3) Lissage ------------------------------------------------------------------
  #### a) 1er lissage à 1e4 ------------------------------------------------------
  dataF_1 <- fct_lissage(1e4, dfcoord, dfvars, list_an, list_indicateurs)
  
  #### b) 2eme lissage pour les communes problématiques à 1e3 --------------------
  dfCoord_relance <- unique(dataF_1 %>% 
                              filter_all(any_vars(is.na(.))) %>% 
                              select(id, longM, latM))

  dataF_2 <- fct_lissage(1e3, dfCoord_relance, dfvars, list_an, list_indicateurs)
  
  #### c) Fusion des 2 lissages --------------------------------------------------
  dataF <- dataF_1 %>% 
    filter_all(all_vars(!is.na(.))) %>%
    bind_rows(dataF_2)


  ## 4) Enregistrement -----------------------------------------------------------
  if (save==T) {write_rds(dataF, paste0("../../Data/", name_save,".rds"))}
  
  return(dataF)
}




#------------------------------------------------------------------------------#
# I) Dnnées ALADIN -------------------------------------------------------------
#------------------------------------------------------------------------------#
dir.create(file.path("../../Data/DRIAS_Aladin/", "lissage"), showWarnings = FALSE)

## A) Periode historique -------------------------------------------------------
#### 1) Chargement des données -------------------------------------------------
# a) Data historique (1982-2005) 
dataVar <- read_delim(unz("../../Data/DRIAS_Aladin/input.zip","indicesALADIN63_CNRM-CM5_1982_2005.txt"),
                      delim = ";", skip = 44) # skip = 27

# b) Data projection avec scenario RCP4.5 (2006 2021) 
dataVar2006 <- read_delim(unz("../../Data/DRIAS_Aladin/input.zip","indicesALADIN63_CNRM-CM5_2006_2050_RCP45.txt"),
                          delim = ";", skip = 44) # skip = 27)

data1982_2021 = bind_rows(dataVar, dataVar2006)

#### 2) Preprocesing -----------------------------------------------------------
dataF_Hist <- preproc_data(dataCoord, data1982_2021, 1982:2021, 
                           "DRIAS_Aladin/lissage/indic_aladin_1982_2021", save = F)

#dataF_Hist <- preproc_data(dataCoord %>% filter(id == "29155"), data1982_2021 %>% select(Latitude, Longitude, ANNEE, TAV), 1982:2021, "indic_aladin_1982_2021")




## B) Data aladin projection RCP 2.6 -------------------------------------------
#### 1) Chargement des données -------------------------------------------------
dataInitiale26 <- read_delim(unz("../../Data/DRIAS_Aladin/input.zip","indicesALADIN63_CNRM-CM5_2006_2050_RCP26.txt"),
                             delim = ";", skip = 44) # skip = 27
#### 2) Preprocesing -----------------------------------------------------------
dataF26 <- preproc_data(dataCoord, dataInitiale26, 2022:2050, 
                        "DRIAS_Aladin/lissage/indic_aladin_2022_2050_RCP26", save = F)



## C) Data aladin projection RCP 4.5 -------------------------------------------
#### 1) Chargement des données -------------------------------------------------
dataInitiale45 <- read_delim(unz("../../Data/DRIAS_Aladin/input.zip","indicesALADIN63_CNRM-CM5_2006_2050_RCP45.txt"),
                             delim = ";", skip = 44) # skip = 27
#### 2) Preprocesing -----------------------------------------------------------
dataF45 <- preproc_data(dataCoord, dataInitiale45, 2022:2050, 
                        "DRIAS_Aladin/lissage/indic_aladin_2022_2050_RCP45", save = F)



## D) Data aladin projection RCP 8.5 -------------------------------------------
#### 1) Chargement des données -------------------------------------------------
dataInitiale85 <- read_delim(unz("../../Data/DRIAS_Aladin/input.zip","indicesALADIN63_CNRM-CM5_2006_2050_RCP85.txt"),
                             delim = ";", skip = 44) # skip = 27
#### 2) Preprocesing -----------------------------------------------------------
dataF85 <- preproc_data(dataCoord, dataInitiale85, 2022:2050, 
                        "DRIAS_Aladin/lissage/indic_aladin_2022_2050_RCP85", save = F)





#------------------------------------------------------------------------------#
# II) Data Climsec annuelles ---------------------------------------------------
#------------------------------------------------------------------------------#
dir.create(file.path("../../Data/DRIAS_Climsec/", "lissage"), showWarnings = FALSE)

## A) Periode historique -------------------------------------------------------
#### 1) Chargement des données -------------------------------------------------
dataInitialeClimsec <- read_delim(unz("../../Data/DRIAS_Climsec/input.zip","indicesARPEGE_RETIC_1982_1999.txt"),
                                  delim = ";", skip = 18) # skip = 27
#### 2) Preprocesing -----------------------------------------------------------
dataFClimsec <- preproc_data(dataCoord, dataInitialeClimsec, 1982:1999, 
                             "DRIAS_Climsec/lissage/indic_climsec_1982_1999", save = F)



## B) Data Climsec A1B ---------------------------------------------------------
#### 1) Chargement des données -------------------------------------------------
dataInitiale_A1B <- read_delim(unz("../../Data/DRIAS_Climsec/input.zip","indicesARPEGE_RETIC_2000_2050_A1B.txt"),
                                  delim = ";", skip = 18) # skip = 27
#### 2) Preprocesing -----------------------------------------------------------
dataFA1B <- preproc_data(dataCoord, dataInitiale_A1B, 2000:2050, 
                         "DRIAS_Climsec/lissage/indic_climsec_2000_2050_A1B", save = F)



## C) Data Climsec A2 ----------------------------------------------------------
#### 1) Chargement des données -------------------------------------------------
dataInitiale_A2 <- read_delim(unz("../../Data/DRIAS_Climsec/input.zip","indicesARPEGE_RETIC_2000_2050_A2.txt"),
                               delim = ";", skip = 18) # skip = 27
#### 2) Preprocesing -----------------------------------------------------------
dataFA2 <- preproc_data(dataCoord, dataInitiale_A2, 2000:2050, 
                        "DRIAS_Climsec/lissage/indic_climsec_2000_2050_A2", save = F)



## D) Data Climsec B1 ----------------------------------------------------------
#### 1) Chargement des données -------------------------------------------------
dataInitiale_B1 <- read_delim(unz("../../Data/DRIAS_Climsec/input.zip","indicesARPEGE_RETIC_2000_2050_B1.txt"),
                              delim = ";", skip = 18) # skip = 27
#### 2) Preprocesing -----------------------------------------------------------
dataFB1 <- preproc_data(dataCoord, dataInitiale_B1, 2000:2050, 
                        "DRIAS_Climsec/lissage/indic_climsec_2000_2050_B1", save = F)





#------------------------------------------------------------------------------#
# III) Data Climsec mensuelles  ------------------------------------------------
#------------------------------------------------------------------------------#

## A) Climsec mensuel A2 -------------------------------------------------------
#### 1) Chargement des données -------------------------------------------------
dataInitiale_A2_month <- read_delim(unz("../../Data/DRIAS_Climsec/input.zip","indicesARPEGE_RETIC_2020_2050_A2_monthly.txt"),
                              delim = ";", skip = 17) # skip = 27
dataInitiale_A2_y <- dataInitiale_A2_month %>%
  rename_with(~ tolower(gsub("#| ", "",.x))) %>%
  group_by(annee, point, longitude, latitude) %>%
  summarise(sswi_min = min(sswi), 
            sswi_max = max(sswi),
            sswi_moy = mean(sswi))

#### 2) Preprocesing -----------------------------------------------------------
dataFA2_y <- preproc_data(dataCoord, dataInitiale_A2_y, 2020:2050, 
                          "DRIAS_Climsec/lissage/indic_climsec_2020_2050_A2_new", save = F)
#write_rds(dataFA2_y, "../../Data/DRIAS_Climsec/lissage/indic_climsec_2020_2050_A2_new.rds")



## A) Climsec mensuel B1 -------------------------------------------------------
#### 1) Chargement des données -------------------------------------------------
dataInitiale_B1_month <- read_delim(unz("../../Data/DRIAS_Climsec/input.zip","indicesARPEGE_RETIC_2020_2050_B1_monthly.txt"),
                                    delim = ";", skip = 17) # skip = 27
dataInitiale_B1_y <- dataInitiale_B1_month %>%
  rename_with(~ tolower(gsub("#| ", "",.x))) %>%
  group_by(annee, point, longitude, latitude) %>%
  summarise(sswi_min = min(sswi), 
            sswi_max = max(sswi),
            sswi_moy = mean(sswi))

#### 2) Preprocesing -----------------------------------------------------------
dataFB1_y <- preproc_data(dataCoord, dataInitiale_B1_y, 2020:2050, 
                          "DRIAS_Climsec/lissage/indic_climsec_2020_2050_B1_new", save = F)




#------------------------------------------------------------------------------#
# IX) Data aladin ecart à horizon 2050 -----------------------------------------
#------------------------------------------------------------------------------#
# 
# 
# ######## RCP26
# ## 1) Chargement des données -------------------------------------------------
# dataH2_26 <- read_delim("../Data/aladin/indicesALADIN63_CNRM-CM5_ecart_H2_RCP26.txt",
#                              delim = ";", skip = 55) # skip = 27
# 
# ## 2) Preprocesing -----------------------------------------------------------
# dataH2_26 <- preproc_data(dataCoord, dataH2_26, 2022:2050, "indic_aladin_ecart_RCP26", T)
# 
# 
# ######## RCP45
# ## 1) Chargement des données -------------------------------------------------
# dataH2_45 <- read_delim("../Data/aladin/indicesALADIN63_CNRM-CM5_ecart_H2_RCP45.txt",
#                         delim = ";", skip = 14) # skip = 27
# 
# ## 2) Preprocesing -----------------------------------------------------------
# dataH2_45 <- preproc_data(dataCoord, dataH2_45, 2022:2050, "indic_aladin_ecart_RCP26", T)
# 
# 
# ######## RCP85
# ## 1) Chargement des données -------------------------------------------------
# dataH2_26 <- read_delim("../Data/aladin/indicesALADIN63_CNRM-CM5_ecart_H2_RCP26.txt",
#                         delim = ";", skip = 14) # skip = 27
# 
# ## 2) Preprocesing -----------------------------------------------------------
# dataH2_26 <- preproc_data(dataCoord, dataH2_26, 2022:2050, "indic_aladin_ecart_RCP26", T)
