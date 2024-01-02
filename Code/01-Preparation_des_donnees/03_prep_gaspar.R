# --------------------------------------------- #
# Date : 12/07/2022
# Auteur : E. Maucuer
# Description : Nettoyage des
# données catnat GASPAR
# --------------------------------------------- #

# Libraries
library(readr)
library(dplyr)
library(readxl)
library(stringi)
library(lubridate)
library(tidyr)
library(data.table)

# ---------------------------------------------------------------------------- #
# 1) Chargement des donnees ----------------------------------------------------
# ---------------------------------------------------------------------------- #

# Data insee
insee_dep <- read_csv("../../Data/Administratif/cog_ensemble_2021_csv/departement2021.csv")
insee_reg <- read_csv("../../Data/Administratif/cog_ensemble_2021_csv/region2021.csv")
infos_com <- read_rds("../../Data/Administratif/infos_communes.rds")

names(insee_dep) <- names(insee_dep) %>% stri_trans_tolower() 
names(insee_reg) <- names(insee_reg) %>% stri_trans_tolower() 


# Data gaspar
gaspar <- read_csv2("../../Data/Gaspar/input/catnat_gaspar.csv")
code_catnat <- read_csv2("../../Data/Gaspar/code_jo_catnat.csv")

# Valeurs manquantes
sapply(gaspar, function(x) sum(is.na(x)))
summary(gaspar)

# ---------------------------------------------------------------------------- #
# 2) Prétraitement des données -------------------------------------------------
# ---------------------------------------------------------------------------- #

## Variables temporelles -------------------------------------------------------
gaspar <- gaspar %>%
  mutate(dat_deb = ymd(dat_deb),
         dat_fin = ymd(dat_fin),
         an_debut = as.integer(year(dat_deb)),
         an_fin = as.integer(year(dat_fin)),
         duree = dat_fin-dat_deb +1,
         dat_pub_arrete = ymd(dat_pub_arrete)) %>%
  rename(date_debut = dat_deb, date_fin = dat_fin)


## Variables géographiques -----------------------------------------------------
# Ajout du code département
gaspar$dep <- substr(gaspar$cod_commune,1,2)
gaspar$dep <- ifelse(as.integer(gaspar$dep>95), substr(gaspar$cod_commune,1,3), gaspar$dep) #cas des dom-tom

# Ajout infos insee dep et reg
gaspar <- gaspar %>% 
  merge(insee_dep %>% select(-cheflieu), by="dep") %>%
  merge(insee_reg %>% select(-cheflieu), by="reg", suffixes = c("_dep","_reg"))


## Variable perils --------------------------------------------------------------
gaspar <- gaspar %>%
  merge(code_catnat %>% select(Code, lib_risque_jo2), by.x = "num_risque_jo", by.y = "Code") %>%
  mutate(inondation = num_risque_jo == 54 | num_risque_jo == 61 ,
         secheresse = ifelse(an_debut >= 1988, num_risque_jo == 59, NA),
         inondation_all = lib_risque_jo2 == "Inondation",
         lib_risque = ifelse(lib_risque_jo2 %in% c("Inondation", "Sécheresse"), lib_risque_jo2, "Autre"))



# ---------------------------------------------------------------------------- #
# 3) Test de merge avec la base de carto ---------------------------------------
# & correction si necessaire --------------------------------------------------#
# ---------------------------------------------------------------------------- #

com_bound_wo_om <- read_rds("../../Data/Data_GIS/geojson/communes-vs.rds")

gaspar_com <- gaspar %>% #1 ligne par commune en metropole
  filter(cod_commune < 96000) %>%
  group_by(cod_commune) %>%
  summarise(count = n())

setDT(com_bound_wo_om)
setDT(gaspar_com)
testG <- merge(com_bound_wo_om, gaspar_com, by.x="code", by.y="cod_commune", all=T)

table(is.na(testG$latM)) # que 2 ligne gaspar non attribuees
# View(testG %>% filter(is.na(latM)))

gaspar <- gaspar %>%
  mutate(cod_commune = ifelse(cod_commune=="14666","14712",
                              ifelse(cod_commune=="27058", "27676", cod_commune)),
         lib_commune = ifelse(cod_commune == "14666", "Troarn", lib_commune))



# ---------------------------------------------------------------------------- #
# 4) Analyse des doublons ------------------------------------------------------
# ---------------------------------------------------------------------------- #

# En cas de doublon (meme commune, meme peril et date de debut et/ou fin identique)
# La règle est que l'arrete le plus récent fait fois
# en cas de dates d'arrete identiques, on garde la déclaration ayant la duree la 
# plus longue


gaspar <- gaspar %>% mutate(dup = duplicated(gaspar))
table(gaspar$dup) # aucune ligne en double

### Date debut ET fin identiques -----------------------------------------------
gaspar <- gaspar %>% 
  select(-dup) %>%
  group_by(cod_commune, date_debut, date_fin, num_risque_jo) %>% 
  mutate(dup = n()) %>% 
  ungroup()
table(gaspar$dup)

gaspar <- gaspar %>% 
  arrange(cod_commune, date_debut, date_fin, desc(dat_pub_arrete)) %>% 
  distinct(cod_commune, date_debut, date_fin, num_risque_jo, .keep_all = T) 

### Date de debut -------------------------------------------------------------- 
gaspar <- gaspar %>% 
  select(-dup) %>%
  group_by(cod_commune, date_debut, num_risque_jo) %>% 
  mutate(dup = n()) %>% 
  ungroup()
table(gaspar$dup)

gaspar <- gaspar %>% 
  arrange(cod_commune, date_debut, desc(dat_pub_arrete), desc(date_fin)) %>% 
  distinct(cod_commune, date_debut, num_risque_jo, .keep_all = T) 

### Date de fin ----------------------------------------------------------------
gaspar <- gaspar %>% 
  select(-dup) %>%
  group_by(cod_commune, date_fin, num_risque_jo) %>% 
  mutate(dup = n()) %>% 
  ungroup()
table(gaspar$dup)

gaspar <- gaspar %>% 
  arrange(cod_commune, date_fin, desc(dat_pub_arrete), date_debut) %>% 
  distinct(cod_commune, date_fin, num_risque_jo, .keep_all = T) %>%
  select(-dup)


# ---------------------------------------------------------------------------- #
# 4) Selection de variables             --------------------------------------
# ---------------------------------------------------------------------------- #

gaspar <- gaspar %>%
  select(reg, ncc_reg, dep, ncc_dep, cod_commune, lib_commune, 
         lib_risque_jo, lib_risque_jo2, inondation, secheresse, inondation_all, lib_risque, num_risque_jo, 
         duree, an_debut, an_fin)
# ---------------------------------------------------------------------------- #
# 5) Gestion des evenements pluri-annuels --------------------------------------
# ---------------------------------------------------------------------------- #
an_min = min(gaspar$an_debut)
an_max = max(gaspar$an_fin)

gaspar_pl <- gaspar

for (an in (an_min:(an_max+1))){
  varname = paste0("y",an)
  gaspar_pl[[varname]] <- with(gaspar_pl, an >= an_debut & an <= an_fin)
}

gaspar_pl <- gaspar_pl %>%
  pivot_longer(paste0("y", an_min:an_max), names_to="annee") %>%
  filter(value==T) %>%
  select(-value) %>%
  mutate(annee = substr(annee,2, 5))



# ---------------------------------------------------------------------------- #
# 6) Agregation annuelle des donnees -------------------------------------------
# ---------------------------------------------------------------------------- #

gaspar_an <- gaspar %>%
  group_by(cod_commune, an_debut) %>%
  summarise(inondation = sum(inondation),
            secheresse = sum(secheresse)) %>%
  rename(annee = an_debut)

gaspar_an_pl <- gaspar_pl %>%
  group_by(cod_commune, annee) %>%
  mutate(annee = as.integer(annee)) %>%
  summarise(inondation_pl = sum(inondation),
            secheresse_pl = sum(secheresse))

gaspar_agregation_annuelle <- merge(gaspar_an, gaspar_an_pl, 
                                    by = c("cod_commune", "annee"), all = T)




# ---------------------------------------------------------------------------- #
# 7) Enregistrement des donnees ------------------------------------------------
# ---------------------------------------------------------------------------- #
dir.create(file.path("../../Data/Gaspar", "output"), showWarnings = FALSE)
write_rds(gaspar, "../../Data/Gaspar/output/gaspar.rds")
write_rds(gaspar_pl, "../../Data/Gaspar/output/gaspar_pluri_annuel.rds")
write_rds(gaspar_agregation_annuelle, "../../Data/Gaspar/output/gaspar_ag.rds")



