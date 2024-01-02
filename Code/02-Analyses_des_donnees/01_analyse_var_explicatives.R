# --------------------------------------------- #
# Date : quelque part en 2022
# Auteur : E. Maucuer
# Description : Analyses des données d'exposition
# aux risques (PPRI, AZI, EAIP, cout des 
# cummules des sinistres, expo au RGA)
# --------------------------------------------- #


library(dplyr)
library(readr)
library(readxl)
library(ggplot2)
library(plotly)
library(leaflet)
library(RColorBrewer)
library(sf)
library(stringr)

source("../00_lib_externe.R")


# ---------------------------------------------------------------------------- #
# 1) Importation et preparation des donnees ------------------------------------
# ---------------------------------------------------------------------------- #

## Donnees de cartographie -----------------------------------------------------
infos_com <- read_rds("../../Data/Administratif/infos_communes.rds")
com_bound_wo_om <- read_rds("../../Data/Data_GIS/geojson/communes-vs.rds")
dep_bound <- readRDS("../../Data/Data_GIS/geojson/departements-vs-om.rds") %>%
  filter(code<=95)



## Donees d'exposition ---------------------------------------------------------
rga2019Finale <- read_rds("../../Data/RGA/rga2019.rds")
ppriCom <- read_rds("../../Data/Gaspar/ppriCom.rds")

dataPlot <- com_bound_wo_om %>% 
  merge(infos_com, by.x = "code", by.y = "code_insee")  %>% 
  merge(rga2019Finale, by.x = "code", by.y = "code_insee") %>%
  merge(ppriCom, by.x = "code", by.y = "code_insee") 

names(dataPlot)


## Data SSWI -------------------------------------------------------------------
final_sswi <- read_rds("../../Data/sswi/final_sswi_1982_2021.rds")



## Data gaspar AZI -------------------------------------------------------------
azi <- read_csv2("../../Data/Gaspar/input/azi_gaspar.csv")

azi <- azi %>% 
  arrange(cod_commune, desc(dat_diffusion)) %>%
  mutate(indic_azi = "Oui") %>%
  distinct(cod_commune, .keep_all = T) 

aziPlot <- com_bound_wo_om %>% merge(azi, by.y="cod_commune", by.x = "code", all.x=T)
aziPlot <- aziPlot %>% mutate(indic_azi = ifelse(is.na(indic_azi), "Non", indic_azi))



## Données ONRN -----------------------------------------------------------------
cumul_ino <- read_xlsx("../../Data/ONRN/ONRN_CoutCommune_Inondation/ONRN_CoutCum_Inon_9518.xlsx")
cumul_sec <- read_xlsx("../../Data/ONRN/ONRN_CoutCommune_Sech/ONRN_CoutCum_Sech_9518.xlsx")

names(cumul_ino) <- c("code_insee","commune","cout")
names(cumul_sec) <- c("code_insee","commune","cout")



### Préparation des données inondations
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



### Préparation des données sécheresses
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



## Donnees EAIP ----------------------------------------------------------------
eaip <- read_csv2("../../Data/ONRN/ONRN_Population_EAIP_CE/ONRN_Population_EAIP_CE.csv")

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

eaip_dep_plot <- merge(eaip_dep, dep_bound, by.x = "code_dept", by.y = "code")




# ---------------------------------------------------------------------------- #
# 2) Analyses de variables -----------------------------------------------------
# ---------------------------------------------------------------------------- #

## Altitude --------------------------------------------------------------------
plot_carto_gradient(dataPlot,"altitude",color_high="brown",title="Altitude")


## Population ------------------------------------------------------------------
plot_carto_gradient(dataPlot,"population",color_low="grey90",color_high="#1DE9B6",title="Population")

# ---------------------------------------------------------------------------- #
# 3) SSWI ---------------------------------------------------------------------
# ---------------------------------------------------------------------------- #

my_annee = 2003

dataPlot_indicateur <- final_sswi %>%
  filter(annee==my_annee) %>% 
  merge(com_bound_wo_om, by.x="id", by.y="code", all.y = T) 


ggplot(data=dataPlot_indicateur) +
  geom_sf(aes(geometry = geometry, fill = sswi_moy), colour = NA) +
  geom_sf(data = dep_bound, aes(geometry = geometry), color="grey40", fill = NA) +
  scale_fill_gradient2(low = "#99000D", mid = "white", high = "#084594", midpoint= 0, limits = c(-2.5,2.5)) +
  theme_void() +
  ggtitle("")

#plot_carto_gradient2(dataPlot_indicateur, "sswi_max", color_low="#99000D", color_high="#084594",
#                         my_limits = c(-2.5,2.5))


# ---------------------------------------------------------------------------- #
# 4) Analyses des secheresses --------------------------------------------------
# ---------------------------------------------------------------------------- #

## RGA Alea moyen fort ----------------------------------------------------------
plot_carto_manual(dataPlot, "part_alea_moyen_fort_commune_cat", colors_red_5, legend_title="Part de la commune en aléa\nmoyen fort (en %)")

pal <- colorFactor("YlOrRd", domain = dataPlot$part_alea_moyen_fort_commune_cat)
hl_opt <- highlightOptions(
  weight = 2,
  color = "#666",
  dashArray = "",
  bringToFront = TRUE)

leaflet(dataPlot) %>%
  setView(lng = 3, lat = 47, zoom = 4) %>%
  addPolygons(weight = 0.5,
              fillOpacity = 1,
              color = NA,
              fillColor = ~pal(part_alea_moyen_fort_commune_cat),
              label = paste("<strong>", dataPlot$code,"</strong><br>", dataPlot$nom) %>%
                lapply(htmltools::HTML),
              dashArray = "1",
              highlightOptions = hl_opt)




## RGA Alea faible -------------------------------------------------------------
plot_carto_manual(dataPlot, "part_alea_faible_commune_cat", colors_vert_sia, legend_title="Part de la commune en aléa\nfaible (en %)")


## RGA alea --------------------------------------------------------------------
plot_carto_manual(dataPlot, "part_alea_cat", colors_bg, legend_title="Part de la commune exposée \nau RGA (en %)")


## ONRN : cout cumule ----------------------------------------------------------
cumul_sec_plot <- cumul_sec %>%
  merge(com_bound_wo_om, by.x = "code_insee", by.y="code", all.y=T) %>%
  mutate(cout_cat = ifelse(is.na(cout_cat),0,cout_cat))

cumul_sec_plot$cout_cat <- ordered(cumul_sec_plot$cout_cat,
                                   levels = c(0,1,2,4,5),
                                   labels = c("Pas de sinistre", 
                                              "< 100k", "100k à 500k", 
                                              "500K à 5M",
                                              "> 5M"))

plot_carto_manual(cumul_sec_plot,"cout_cat",colors_red,color_contour="grey70",legend_title="Coûts cumulés sécheresse en €")



# ---------------------------------------------------------------------------- #
# 5) Analyses des inondations --------------------------------------------------
# ---------------------------------------------------------------------------- #
colors_ppri <- c("white", "darkblue")

## PPRI ------------------------------------------------------------------------
#### Submersions marines -------------------------------------------------------
plot_carto_manual(dataPlot, "ppri_submersion", colors_ppri, title="PPRI Submersions marines", legend="none")


#### ppri_crue_lent ------------------------------------------------------------
plot_carto_manual(dataPlot, "ppri_crue_lent", colors_ppri, title="PPRI Crues lentes", legend="none")


#### ppri_crue_rapide ----------------------------------------------------------
plot_carto_manual(dataPlot, "ppri_crue_rapide", colors_ppri, title="PPRI Crues rapides", legend="none")



#### ppri_ruiss_coulee ---------------------------------------------------------
plot_carto_manual(dataPlot, "ppri_ruiss_coulee", colors_ppri, title="PPRI Ruissellements et coulées", legend="none")


#### ppri_nappe ----------------------------------------------------------------
plot_carto_manual(dataPlot, "ppri_nappe", colors_ppri, title="PPRI Remontées de nappe", legend="none")


#### ppri_lave_torrent ---------------------------------------------------------
plot_carto_manual(dataPlot, "ppri_lave_torrent", colors_ppri, title="PPRI Lave torrentielle", legend="none")


#### somme ppri ----------------------------------------------------------------
plot_carto_manual(dataPlot, "ppri_sum", colors_ppri, title="PPRI somme", legend="none")


#### ppri  ---------------------------------------------------------------------
plot_carto_manual(dataPlot, "ppri_inondation", colors_ppri, title="PPRI avant traitement", legend="none")


#### ppri retraite -------------------------------------------------------------
plot_carto_manual(dataPlot, "ppri", colors_ppri, title="PPRI après traitement", legend="none")



## AZI -------------------------------------------------------------------------
#png("../../Plots&Resultats/exposition_risques/map_azi.png", units="in", width=8.5, height=5, res=300)
plot_carto_manual(aziPlot, "indic_azi", c("white", "#4292c6"), legend_title="Commune concernée par \nun Atlas de Zone Inondable")
dev.off()



## ONRN : cout cumule ----------------------------------------------------------
cumul_ino_plot <- cumul_ino %>%
  merge(com_bound_wo_om, by.x = "code_insee", by.y="code", all.y=T) %>%
  mutate(cout_cat = ifelse(is.na(cout_cat),0,cout_cat))

cumul_ino_plot$cout_cat <- ordered(cumul_ino_plot$cout_cat,
                                   levels = c(0,1,2,4,5),
                                   labels = c("Pas de sinistre", 
                                              "< 100k", "100k à 500k", 
                                              "500K à 5M",
                                              "> 5M"))


plot_carto_manual(cumul_ino_plot, "cout_cat", colors_test, legend_title="Coûts cumulés inonation en €", color_contour="grey70")



## EAIP ------------------------------------------------------------------------
plot_carto_gradient(eaip_dep_plot,"pop", color_high="#08519c",color_contour="white")

plot_carto_manual(eaip_dep_plot, "pop_cat", colors_eaip, legend_title="Population dans l'EAIP \ncours d'eau", 
            color_contour="grey70")


