# -----------------------------------------------------------------------------#
#                   Analyse des données de modélisation NEW SSWI
# -----------------------------------------------------------------------------#


library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(data.table)
library(corrplot)
library(plotly)

# Chargement des données de modélisation et de cartographie
dataFinaleCom <- read_rds("../../Data/modelisation/data_modelisation_1982_2021_f.rds")

com_bound <- read_rds("../../Data/Data_GIS/geojson/communes-vs.rds")
dep_bound <- read_rds("../../DAta/Data_GIS/geojson/departements-vs-om.rds") %>%
  dplyr::filter(code<=95)



# Création d indicateurs de secheresse et d inondation
dataFinaleCom <- dataFinaleCom %>%
  mutate(indic_in = ifelse(inondation==0,0,1),
         indic_sec = ifelse(secheresse==0,0,1),
         indic_in_pl = ifelse(inondation_pl==0,0,1),
         indic_sec_pl = ifelse(secheresse_pl==0,0,1))



# Liste de variables
list_CN <- c("inondation", "secheresse", "indic_in", "indic_sec")
list_CN_pl <- c("inondation_pl", "secheresse_pl", "indic_in_pl", "indic_sec_pl")

list_var <- setdiff(names(dataFinaleCom), 
                    c("id","annee","longM","latM","code_dep","commune",
                      "inondation", "secheresse", "indic_in", "indic_sec", 
                      "inondation_pl", "secheresse_pl", "indic_in_pl", "indic_sec_pl"))



list_var_continuous <- setdiff(list_var, c(names(dataFinaleCom)[startsWith(names(dataFinaleCom),"ppri")],
                                           "part_alea_faible_commune_cat",
                                           "part_alea_moyen_fort_commune_cat"))



# -----------------------------------------------------------------------------#
# 1) Stat globales des bases de modelisation                --------------------
# -----------------------------------------------------------------------------#

data_modelisation_ino <- dataFinaleCom %>% filter(annee > 1984)
sum(data_modelisation_ino$inondation_pl)
mean(data_modelisation_ino$inondation_pl)

data_modelisation_ino <- data_modelisation_ino %>% group_by(id) %>% summarise(nb_ino = sum(inondation_pl))
table(data_modelisation_ino$nb_ino)
1-1858/35228


data_modelisation_sec <- dataFinaleCom %>% filter(annee > 1990 & annee < 2021)
sum(data_modelisation_sec$secheresse_pl)
mean(data_modelisation_sec$secheresse_pl)

data_modelisation_sec <- data_modelisation_sec %>% group_by(id) %>% summarise(nb_sec = sum(secheresse_pl))
table(data_modelisation_sec$nb_sec)
1-23023/35228




## Communes remarquables ---------------------------------------
sum_catnat <- dataFinaleCom %>%
  group_by(id, commune) %>%
  summarise(nb_ino = sum(inondation_pl),
            nb_sec = sum(secheresse_pl, na.rm = T))



sum_catnat %>% filter(nb_ino > 20) %>% arrange(-nb_ino)

plot_ly(dataFinaleCom %>% filter(id == "13055"), colors = "Blues") %>%
  add_bars(x =~ annee, y =~ inondation)

plot_ly(dataFinaleCom %>% filter(id == "13055"), colors = "Blues") %>%
  add_bars(x =~ annee, y =~ pfl90)



# -----------------------------------------------------------------------------#
# 2) Etude des correlations ----------------------------------------------------
# -----------------------------------------------------------------------------#

# -----------------------------------------------------------------------------#
## Correlation avec la variable réponse ----------------------------------------
# -----------------------------------------------------------------------------#
dataCor <- data.frame()
for (catnat in list_CN){
  print(catnat)
  corCN <- data.frame(val = 0)
  val_catnat <- dataFinaleCom %>% select(catnat) 
  
  correlation_value <- as.data.frame(cor(val_catnat, 
                                         dataFinaleCom %>% select(list_var_continuous),
                                         use="complete.obs", method="spearman"))
  dataCor <- rbind(dataCor, correlation_value)
}
dataCor <- as.data.frame(t(dataCor))
#write.csv2(dataCor, "../../Plots&Resultats/correlations/corrSpearman_new.csv")


dataCor_pl <- data.frame()
for (catnat in list_CN_pl){
  print(catnat)
  val_catnat <- dataFinaleCom %>% select(catnat) 
  
  correlation_value <- as.data.frame(cor(val_catnat, 
                                         dataFinaleCom %>% select(list_var_continuous),
                                         use="complete.obs", method="spearman"))
  dataCor_pl <- rbind(dataCor_pl, correlation_value)
}
dataCor_pl <- as.data.frame(t(dataCor_pl))
#write.csv2(dataCor_pl, "../../Plots&Resultats/correlations/corrSpearman_new_pl.csv")


# -----------------------------------------------------------------------------#
## Correlations entre variables explicatves ------------------------------------
# -----------------------------------------------------------------------------#

### Sécheresse -----------------------------------------------------------------
dataSec <- dataFinaleCom %>% 
  dplyr::select(list_var_continuous, 
                -starts_with("ppri"),
                -inondationH3,
                -inondation_pl) %>%
  rename(part_alea_moyen_fort = part_alea_moyen_fort_commune,
         part_alea_faible = part_alea_faible_commune)

dataCorVarSec = cor(dataSec, use = "complete.obs",
                    method = "spearman")

corrplot(dataCorVarSec, 
         method = "square",
         order = 'hclust',
         type = "upper",
         tl.col="black", 
         tl.cex = 1.5)

corrplot(dataCorVarSec, 
         method = "color",
         order = 'hclust', 
         addrect = 3, rect.lwd = 3,
         tl.col="black")

#write.csv2(as.data.frame(dataCorVar), "../../Plots&Resultats/correlations/dataCorVar_sec.csv")


### Inondations ----------------------------------------------------------------

dataCorVar = cor(dataFinaleCom %>% 
                   select(list_var_continuous, 
                          -starts_with("part_"),
                          -secheresseH3,
                          -secheresses_pl), use = "complete.obs")

View(as.data.frame(dataCorVar))
#write.csv2(as.data.frame(dataCorVar), "../../Plots&Resultats/correlations/dataCorVar_inondation.csv")


corrplot(dataCorVar, 
         method = "color",
         order = 'hclust', 
         addrect = 3, rect.lwd = 3,
         tl.col="black", tl.cex = 2)

corrplot(dataCorVar, 
         method = "square",
         order = 'hclust',
         type = "upper",
         tl.col="black", 
         tl.cex = 1.5)





# -----------------------------------------------------------------------------#
# 3) Carte pour un indicateur et une annéee donnée  ----------------------------
# -----------------------------------------------------------------------------#
# 2001, 2003 2018 2013
# 1993 1989
my_annee = 1993

dataPlot_indicateur <- dataFinaleCom %>%
  filter(annee==my_annee) %>% 
  merge(com_bound, by.x="id", by.y="code", all.y = T) 


ggplot(data=dataPlot_indicateur) +
  geom_sf(aes(geometry = geometry, fill = sswi_moy), colour = NA) +
  geom_sf(data = dep_bound, aes(geometry = geometry), color="grey40", fill = NA) +
  scale_fill_gradient2(low = "#99000D", mid = "white", high = "#084594", midpoint= 0, limits = c(-2.5,2.5)) +
  theme_void() +
  ggtitle("")


ggplot(data=dataPlot_indicateur) +
  geom_sf(aes(geometry = geometry, fill = rr1mm), colour = NA) +
  geom_sf(data = dep_bound, aes(geometry = geometry), color="grey40", fill = NA) +
  scale_fill_gradient2(low = "white", high = "#084594") +
  theme_void() +
  ggtitle("")


# -----------------------------------------------------------------------------#
# 4) Carte des indicateurs moyens sur la période historique --------------------
# -----------------------------------------------------------------------------#

dataMoyHist <- dataFinaleCom %>%
  select(-starts_with("ppri"), -starts_with("part_"),
         -altitude, -secheresse, -inondation,
         -secheresseH3, -inondationH3,
         -longM, -latM, -annee) %>%
  group_by(id, commune, code_dep) %>%
  summarise_all(mean, na.rm = T)


dataPlot <- dataMoyHist %>%
  merge(com_bound, by.x="id", by.y="code", all.y = T) %>%
  filter(id < 96000) 


ggplot(data=dataPlot) +
  geom_sf(aes(geometry = geometry, fill = pfl90), colour = NA) +
  geom_sf(data = dep_bound, aes(geometry = geometry), color="grey40", fill = NA) +
  scale_fill_gradient(low = "white", high = "#084594") +
  theme_void() +
  ggtitle("")


ggplot(data=dataPlot) +
  geom_sf(aes(geometry = geometry, fill = tx35), colour = NA) +
  geom_sf(data = dep_bound, aes(geometry = geometry), color="grey40", fill = NA) +
  scale_fill_gradient(low = "white", high = "#99000D") +
  theme_void() +
  ggtitle("")



