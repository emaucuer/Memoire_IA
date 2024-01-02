# --------------------------------------------- #
# Date : 28/08/2022
# Auteur : E. Maucuer
# Description : Nettoyage et travail sur les 
# données administratives + exposition aux
# risques (TRI, PPRI, retrait gonflement des 
# argiles)
# --------------------------------------------- #

library(reshape)
library(dplyr)
library(readr)
library(stringi)
library(data.table)
library(readxl)
library(sf)
library(lubridate)


com_bound_wo_om <- read_rds("../../Data/Data_GIS/geojson/communes-vs.rds")

# -----------------------------------------------------------------------------#
# DONEES COMMUNES/DEPARTEMETS --------------------------------------------------
# -----------------------------------------------------------------------------#
infos_com <- read_csv2("../../Data/Administratif/correspondance-code-insee-code-postal.csv")


names(infos_com) <- names(infos_com) %>%
  stri_trans_tolower() %>%
  stri_trans_general(id = "Latin-ASCII") 
names(infos_com) <- gsub(" ", "_", names(infos_com)) 

## Gestion des décimaux : ------------------------------------------------------
infos_com <- infos_com %>% 
  mutate(altitude_moyenne = as.integer(altitude_moyenne)/10 ,
         superficie = as.integer(superficie)/1000, # conversion en km2
         population = as.double(population))

## Gestion de paris, lyon et marseille -----------------------------------------
paris <- infos_com %>% 
  filter(grepl("^PARIS-[0-9]", commune)) %>%
  summarize(altitude_moyenne = round(weighted.mean(altitude_moyenne,superficie)),
            superficie = sum(superficie),
            population = sum(population),
            departement= max(departement),
            region = max(region),
            statut = max(statut),
            code_departement = max(code_departement),
            code_region = max(code_region),
            code_arrondissement = max(code_arrondissement)) %>%
  mutate(code_insee = "75056",
         code_postal = "75000",
         commune = "PARIS",
         code_commune = "056")


lyon <- infos_com %>% 
  filter(grepl("^LYON-*[0-9]", commune)) %>%
  summarize(altitude_moyenne = round(weighted.mean(x=altitude_moyenne,w=superficie)),
            superficie = sum(superficie),
            population = sum(population),
            departement= max(departement),
            region = max(region),
            statut = max(statut),
            code_departement = max(code_departement),
            code_region = max(code_region),
            code_arrondissement = max(code_arrondissement)) %>%
  mutate(code_insee = "69123",
         code_postal = "69000",
         commune = "LYON",
         code_commune = "123")

marseille <- infos_com %>% 
  filter(grepl("^MARSEILLE-*[0-9]", commune)) %>%
  summarize(altitude_moyenne = round(weighted.mean(x=altitude_moyenne,w=superficie)),
            superficie = sum(superficie),
            population = sum(population),
            departement= max(departement),
            region = max(region),
            statut = max(statut),
            code_departement = max(code_departement),
            code_region = max(code_region),
            code_arrondissement = max(code_arrondissement)) %>%
  mutate(code_insee = "13055",
         code_postal = "13000",
         commune = "MARSEILLE",
         code_commune = "055")


infos_com <- infos_com %>%
  filter(grepl("^(LYON|PARIS|MARSEILLE)-*[0-9]", commune) == F) %>%
  bind_rows(paris) %>%
  bind_rows(lyon) %>% 
  bind_rows(marseille) %>%
  rename(altitude = altitude_moyenne)


## Test de merge avec la base de carto -----------------------------------------
setDT(infos_com)
setDT(com_bound_wo_om)
testM <- merge(com_bound_wo_om %>% mutate(bound = 1), 
               infos_com %>% mutate(infos=1), 
               by.x="code", by.y="code_insee", all=T)
View(testM %>% filter(is.na(infos)))

# 3 communes pour lesquelles les infos manquent -> A completer avec internet
# Culey
# Bihorel
# Saint-Lucien

infos_com <- infos_com %>% 
  add_row(code_insee = "55138", code_postal = "55000", commune = "CULEY",
        departement = "MEUSE", altitude = 306, superficie = 11.02, 
        population = 0.1, code_commune = "138", code_departement = "55") %>%
  add_row(code_insee = "76095", code_postal = "76420", commune = "BIHOREL",
          departement = "SEINE-MARITIME", altitude = 114, superficie = 2.51, 
          population = 8.3, code_commune = "095", code_departement = "76") %>%
  add_row(code_insee = "76601", code_postal = "76780", commune = "SAINT-LUCIEN",
          departement = "SEINE-MARITIME", altitude = 140, superficie = 8.71, 
          population = 0.2, code_commune = "601", code_departement = "76")

## Save ------------------------------------------------------------------------
write_rds(infos_com, "../../Data/Administratif/infos_communes.rds")




# -----------------------------------------------------------------------------#
# Exposition RGA 2019 ----------------------------------------------------------
# -----------------------------------------------------------------------------#

## Importation des données -----------------------------------------------------
rga2019 <- read_xlsx("../../Data/RGA/indicateurs_rga_2019_communes_departements.xlsx",
                     sheet = "RGA_2019_commune")

rga2019 <- rga2019 %>% rename(code_insee = insee_com)


colsDep <- c("departement", "nom", "nb_logement", "nb_logement_alea_moyen_fort", 
             "nb_logement_alea_faible", "nb_logement_sans_alea",
             "surface_dep", "surface_alea_moyen_fort_dep", 
             "surface_alea_faible_dep", "surface_sans_alea", 
             "nb_logement_alea_moyen_fort_avant_1920", "nb_logement_alea_moyen_fort_1920_1945",
             "nb_logement_alea_moyen_fort_1945_1975", "nb_logement_alea_moyen_fort_apres_1975",
             "nb_logement_alea_faible_avant_1920", "nb_logement_alea_faible_1920_1945",
             "nb_logement_alea_faible_1945_1975", "nb_logement_alea_faible_apres_1975")
             
rga2019Dep <- read_xlsx("../../Data/RGA/indicateurs_rga_2019_communes_departements.xlsx",
                     sheet = "RGA_2019_departement", skip = 2,
                     col_names = colsDep)
summary(rga2019) # part en % sur 100, possible erreur sur les parts de surface 

rga2019Dep <- rga2019Dep %>%
  mutate(part_logement_alea_moyen_fort_avant_1920 = 100*nb_logement_alea_moyen_fort_avant_1920 / nb_logement,
         part_logement_alea_moyen_fort_1920_1945 = 100*nb_logement_alea_moyen_fort_1920_1945 / nb_logement,
         part_logement_alea_moyen_fort_1945_1975 = 100*nb_logement_alea_moyen_fort_1945_1975 / nb_logement,
         part_logement_alea_moyen_fort_apres_1975 = 100*nb_logement_alea_moyen_fort_apres_1975 / nb_logement,
         part_logement_alea_faible_avant_1920 = 100*nb_logement_alea_faible_avant_1920 / nb_logement,
         part_logement_alea_faible_1920_1945 = 100*nb_logement_alea_faible_1920_1945 / nb_logement,
         part_logement_alea_faible_1920_1945 = 100*nb_logement_alea_faible_1920_1945 / nb_logement,
         part_logement_alea_faible_1920_1945 = 100*nb_logement_alea_faible_1920_1945 / nb_logement,
         part_alea_faible_dep = 100*surface_alea_faible_dep/surface_dep,
         part_alea_moyen_fort_dep = 100*surface_alea_moyen_fort_dep/surface_dep,
         part_alea = part_alea_faible_dep + part_alea_moyen_fort_dep)


## Correction pour les certaines communes --------------------------------------
# 9 communes pour lesquels surface tot < surface faible + surface moyen-fort 
rga2019$part_alea <- rga2019$part_alea_moyen_fort_commune + rga2019$part_alea_faible_commune
summary(rga2019$part_alea) 

com_rga_pb <- rga2019 %>% 
  filter(part_alea > 100.01)%>%
  mutate(surface_commune = surface_alea_faible_commune+surface_alea_moyen_fort_commune) %>%
  mutate(part_alea_faible_commune = 100*surface_alea_faible_commune/surface_commune,
         part_alea_moyen_fort_commune = 100*surface_alea_moyen_fort_commune/surface_commune) %>%
  mutate(part_alea = part_alea_moyen_fort_commune + part_alea_faible_commune)
  
rga2019 <- rga2019 %>% 
  filter(part_alea < 100.01) %>%
  bind_rows(com_rga_pb)

## Test merge avec la base de carto --------------------------------------------
testRGA <- merge(rga2019, com_bound_wo_om,
                 by.x="code_insee", by.y="code", all=T)

table(is.na(testRGA$surface_commune), useNA = 'ifany') # 390 communes sans exposition RGA
View(testRGA %>% filter(is.na(testRGA$surface_commune)))
table(is.na(testRGA$latM), useNA = 'ifany')

# Pour les communes sans exposition RGA, on utilise la moyenne du dep
com_wo_rga <- testRGA %>% 
  filter(is.na(testRGA$surface_commune)) %>%
  select(code_insee) %>%
  mutate(dep = substr(code_insee,1,2)) %>%
  merge(rga2019Dep, by.x="dep", by.y="departement") %>%
  select(-starts_with("nb_logement"), -starts_with("surface_"), -nom, -dep) %>%
  rename(part_alea_faible_commune = part_alea_faible_dep,
         part_alea_moyen_fort_commune = part_alea_moyen_fort_dep)


# Création de la base finale
rga2019Finale <- rga2019 %>%
  bind_rows(com_wo_rga) 



## Création de categories pour certaines variables -----------------------------
rga2019Finale <- rga2019Finale %>% 
  mutate_at(.vars = vars(starts_with("part_")), 
            .funs= list(cat =~cut(.,breaks = c(0,1,25,50,75,max(max(.), 100)),
                                  include.lowest = T)))
rga2019Dep <- rga2019Dep %>% 
  mutate_at(.vars = vars(starts_with("part_")), 
            .funs= list(cat =~cut(.,breaks = c(0,1,25,50,75,max(max(.), 100)),
                                  include.lowest = T)))

## Save ------------------------------------------------------------------------
write_rds(rga2019Finale, "../../Data/RGA/rga2019.rds")
write_rds(rga2019Dep, "../../Data/RGA/rga2019Dep.rds")



# -----------------------------------------------------------------------------#
# PPRN Inondation --------------------------------------------------------------
# -----------------------------------------------------------------------------#
pprn <- read.csv2("../../Data/Gaspar/input/pprn_gaspar.csv", na.strings="")
pprn <- pprn %>% rename(code_insee = cod_commune)

## Identification des PPR inondation -------------------------------------------
cod_risque <- pprn %>% select(lib_risque, num_risque) %>% distinct() %>% arrange(lib_risque)
cod_risque 
# write.table(cod_risque, file="cod_risque_pprn.txt", row.names=F, quote=F)

cod_inondation <- c(140, 131, 285, 130, 286, 180, 183)
ppri <- pprn %>% filter(num_risque %in% cod_inondation)
ppri <- ppri %>% mutate_at(vars(starts_with("dat")), funs(ymd_hms))

# # suppression des ppr qui ne sont plus d actualité (ie dat_abroge < date ajd)
# table(is.na(ppri$dat_abrog))
# table(year(ppri$dat_abrog))
table(ppri$lib_risque,ppri$num_risque)

## Creation de la bdd finale (1 ligne par code insee)

ppriCom <- ppri %>%
  mutate(value = 1,
         lib_risque_short = ifelse(num_risque == 131, "ppri_lave_torrent",
                                   ifelse(num_risque == 285, "ppri_nappe",
                                          ifelse(num_risque == 130, "ppri_ruiss_coulee",
                                                 ifelse(num_risque == 286,"ppri_submersion",
                                                        ifelse(num_risque == 180,"ppri_crue_lent",
                                                               ifelse(num_risque == 183,"ppri_crue_rapide","ppri_inondation"))))))) %>% 
  select(code_insee, lib_risque_short, value) %>%
  distinct() %>%
  cast(code_insee ~ lib_risque_short) %>%
  merge(com_bound_wo_om %>% select(code), by.x = "code_insee", by.y = "code", all =T) %>%
  replace(is.na(.), 0) %>%
  mutate(ppri_sum = rowSums(across(starts_with("ppri_"))),
         ppri = as.integer(ppri_sum>0))
         

## PPR RGA ---------------------------------------------------------------------
pprrga <- pprn %>% 
  filter(cod_ppr == "Plan de Prévention des Risques Naturels Retrait Gonflement Argile")

## Save ------------------------------------------------------------------------
write_rds(ppriCom, "../../Data/Gaspar/ppriCom.rds")
write_rds(pprrga, "../../Data/Gaspar/pprRGA.rds")


