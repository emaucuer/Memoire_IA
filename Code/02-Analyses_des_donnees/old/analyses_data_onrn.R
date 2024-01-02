setwd("G:/Drive partagés/BU_ACT_FR/4 - Mémoires/Mémoires 2022/Mémoire de MAUCUER Eurydice/Code")


# Libraries --------------------------------------------------------------------
library(dplyr)
library(readxl)
library(readr)
library(stringr)
library(ggplot2)
library(leaflet)
library(RColorBrewer)

# -----------------------------------------------------------------------------#
# Données carto ----------------------------------------------------------------
# -----------------------------------------------------------------------------#
com_bound_wo_om <- read_rds("../Data_GIS/geojson/communes-vs.rds")
#com_bound_om <- read_rds("../Data_GIS/geojson/communes-vs_-om.rds")
#com_bound2 <- sf::st_read("../Data_GIS/OpenStreetMap/communes-20220101.shp")
dep_bound <- read_rds("../Data_GIS/geojson/departements-vs-om.rds") %>%
  filter(code<=95)

# -----------------------------------------------------------------------------#
# Données ONRN -----------------------------------------------------------------
# -----------------------------------------------------------------------------#
cumul_ino <- read_xlsx("../Data/ONRN/ONRN_CoutCommune_Inondation/ONRN_CoutCum_Inon_9518.xlsx")
cumul_sec <- read_xlsx("../Data/ONRN/ONRN_CoutCommune_Sech/ONRN_CoutCum_Sech_9518.xlsx")

names(cumul_ino) <- c("code_insee","commune","cout")
names(cumul_sec) <- c("code_insee","commune","cout")



# -----------------------------------------------------------------------------#
# INONDATION - Préparation des données -----------------------------------------
# -----------------------------------------------------------------------------#
cumul_ino <- cumul_ino %>%
  mutate(min_cout = as.integer(str_extract(cout, "[0-9]+")),
         max_cout = as.integer(str_extract(str_extract(cout, "et [0-9]+"),"[0-9]+")),
         unite_1 = str_extract(cout,"[a-z-A-Z]€"),
         unite_2 = str_extract(cout,"[a-z-A-Z]€$")) %>%
  mutate(min_cout_k = ifelse(unite_1 == "M€", min_cout*1000,min_cout),
         max_cout_k = ifelse(unite_2 == "M€", max_cout*1000,max_cout),
         min_cout_M = ifelse(unite_1 == "k€", min_cout/1000,min_cout),
         max_cout_M = ifelse(unite_2 == "k€", max_cout/1000,max_cout)) %>%
  mutate(max_cout_k = ifelse(min_cout_k==100000, Inf, max_cout_k),
         max_cout_M = ifelse(min_cout_k==100000, Inf, max_cout_M)) %>%
  select(-min_cout,-max_cout,-unite_1,-unite_2) %>%
  mutate(cout_cat = ifelse(max_cout_k <= 100, 1,
                           ifelse(max_cout_k <= 500, 2,
                                  ifelse(max_cout_k <= 5000, 4,
                                         ifelse(min_cout_k >= 5000, 5, 0))))) %>%
  mutate(cout_cat = ifelse(is.na(cout_cat),0, cout_cat))



# -----------------------------------------------------------------------------#
# Cartographie -----------------------------------------------------------------
# -----------------------------------------------------------------------------#
cumul_ino_plot <- cumul_ino %>%
  merge(com_bound_wo_om, by.x = "code_insee", by.y="code", all.y=T) %>%
  mutate(cout_cat = ifelse(is.na(cout_cat),0,cout_cat))

cumul_ino_plot$cout_cat <- ordered(cumul_ino_plot$cout_cat,
                              levels = c(0,1,2,4,5),
                              labels = c("Pas de sinistre", 
                                         "< 100k", "100k à 500k", 
                                         "500K à 5M",
                                         "> 5M"))

ggplot(data=cumul_ino_plot) +
  geom_sf(aes(geometry = geometry, fill = cout_cat), color = NA) +
  scale_fill_manual(values = c("white", "#c6dbef", "#6baed6", "#2171b5","#08306b")) +
  geom_sf(data = dep_bound, aes(geometry = geometry), color="grey70", fill = NA) +
  theme_void() +
  guides(fill=guide_legend(title="Coûts cumulés inonation en €"))



# -----------------------------------------------------------------------------#
# Secheresse - Préparation des données -----------------------------------------
# -----------------------------------------------------------------------------#
cumul_sec <- cumul_sec %>%
  mutate(min_cout = as.integer(str_extract(cout, "[0-9]+")),
         max_cout = as.integer(str_extract(str_extract(cout, "et [0-9]+"),"[0-9]+")),
         unite_1 = str_extract(cout,"[a-z-A-Z]€"),
         unite_2 = str_extract(cout,"[a-z-A-Z]€$")) %>%
  mutate(min_cout_k = ifelse(unite_1 == "M€", min_cout*1000,min_cout),
         max_cout_k = ifelse(unite_2 == "M€", max_cout*1000,max_cout),
         min_cout_M = ifelse(unite_1 == "k€", min_cout/1000,min_cout),
         max_cout_M = ifelse(unite_2 == "k€", max_cout/1000,max_cout)) %>%
  mutate(max_cout_k = ifelse(min_cout_k==50000, Inf, max_cout_k),
         max_cout_M = ifelse(min_cout_k==50000, Inf, max_cout_M)) %>%
  select(-min_cout,-max_cout,-unite_1,-unite_2) %>%
  mutate(cout_cat = ifelse(max_cout_k <= 100, 1,
                           ifelse(max_cout_k <= 500, 2,
                                  ifelse(max_cout_k <= 5000, 4,
                                         ifelse(min_cout_k >= 5000, 5, 0))))) %>%
  mutate(cout_cat = ifelse(is.na(cout_cat),0, cout_cat))


# -----------------------------------------------------------------------------#
# Cartographie -----------------------------------------------------------------
# -----------------------------------------------------------------------------#
cumul_sec_plot <- cumul_sec %>%
  merge(com_bound_wo_om, by.x = "code_insee", by.y="code", all.y=T) %>%
  mutate(cout_cat = ifelse(is.na(cout_cat),0,cout_cat))

cumul_sec_plot$cout_cat <- ordered(cumul_sec_plot$cout_cat,
                                   levels = c(0,1,2,4,5),
                                   labels = c("Pas de sinistre", 
                                              "< 100k", "100k à 500k", 
                                              "500K à 5M",
                                              "> 5M"))

ggplot(data=cumul_sec_plot) +
  geom_sf(aes(geometry = geometry, fill = cout_cat), color = NA) +
  scale_fill_manual(values = c("white", "#fff5f0", "#fcbba1", "#fb6a4a","#cb181d")) +
  geom_sf(data = dep_bound, aes(geometry = geometry), color="grey70", fill = NA) +
  theme_void() +
  guides(fill=guide_legend(title="Coûts cumulés sécheresse en €"))




# -----------------------------------------------------------------------------#
# EAIP -------------------------------------------------------------------------
# -----------------------------------------------------------------------------#
eaip <- read_csv2("../Data/ONRN/ONRN_Population_EAIP_CE/ONRN_Population_EAIP_CE.csv")

names(eaip) <- tolower(names(eaip))
eaip <- eaip %>% rename(pop = "population dans eaip ce")

eaip_dep <- eaip %>% 
  group_by(code_dept,nom_dept) %>% 
  summarize(pop= sum(pop)) %>%
  mutate(pop_cat = ifelse(pop <= 50000, 1,
                        ifelse(pop <= 75000, 2,
                               ifelse(pop <= 150000, 3,
                                      ifelse(pop <= 250000, 4, 5)))))

eaip_dep$pop_cat <- ordered(eaip_dep$pop_cat,
                                   levels = c(1,2,3,4,5),
                                   labels = c("< 50k", 
                                              "50k à 75k", 
                                              "75k à 150k", 
                                              "150k à 250k",
                                              "> 250k"))


eaip_dep_plot <- eaip_dep %>%
  merge(dep_bound, by.x = "code_dept", by.y = "code")

ggplot(data=eaip_dep_plot) +
  geom_sf(aes(geometry = geometry, fill = pop), color = NA) +
  geom_sf(data = dep_bound, aes(geometry = geometry), color="white", fill = NA) +
  scale_fill_gradient(low = "white", high = "#B90000") +
  theme_void() 


ggplot(data=eaip_dep_plot) +
  geom_sf(aes(geometry = geometry, fill = pop_cat), color = NA) +
  scale_fill_manual(values = c('white','#deebf7','#9ecae1', '#4292c6','#08519c')) +
  geom_sf(data = dep_bound, aes(geometry = geometry), color="grey70", fill = NA) +
  theme_void() +
  guides(fill=guide_legend(title="Population dans l'EAIP \ncours d'eau"))
