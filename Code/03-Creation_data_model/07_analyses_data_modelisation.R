# -----------------------------------------------------------------------------#
#                   Analyse des données de modélisation
# -----------------------------------------------------------------------------#


library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(data.table)
library(corrplot)
library(plotly)
library(sf)

# Chargement des données de modélisation et de cartographie
dataFinaleCom <- read_rds("../../Data/modelisation/data_modelisation_1982_2021_all_V2.rds")
dataFinaleClimsec <- read_rds("../../Data/modelisation/data_modelisation_1982_2021_all_V2_climsec.rds")
dataFinaleClimsec_pl <- read_rds("../../Data/modelisation/data_modelisation_1982_2021_all_V2_climsec_pl.rds")

com_bound <- read_rds("../../Data/Data_GIS/geojson/communes-vs.rds")
dep_bound <- read_rds("../../Data/Data_GIS/geojson/departements-vs-om.rds") %>%
  dplyr::filter(code<=95)



# Création d indicateurs de secheresse et d inondation
dataFinaleCom <- dataFinaleCom %>%
  mutate(indic_in = ifelse(inondation==0,0,1),
         indic_sec = ifelse(secheresse==0,0,1))
dataFinaleClimsec <- dataFinaleClimsec %>%
  mutate(indic_in = ifelse(inondation==0,0,1),
         indic_sec = ifelse(secheresse==0,0,1))
dataFinaleClimsec_pl <- dataFinaleClimsec_pl %>%
  mutate(indic_in = ifelse(inondation==0,0,1),
         indic_sec = ifelse(secheresse==0,0,1))

  

# Liste de variables
list_CN <- c("inondation", "secheresse", "indic_in", "indic_sec")
list_var <- setdiff(names(dataFinaleClimsec), 
                    c("id","annee","longM","latM","inondation", "secheresse",
                      "commune", "indic_in", "indic_sec", "code_dep"))

list_var_continuous <- setdiff(list_var, 
                               c(names(dataFinaleClimsec)[startsWith(names(dataFinaleClimsec),"ppri")],
                                           "part_alea_faible_commune_cat",
                                           "part_alea_moyen_fort_commune_cat"))


# tav+tnav+txav+sd+tx35+txnd+            
# tnht+tr+tnfd+tnnd+txfd+tncwd+           
# txhwd+trav+txq90+txq10+tnq10+tnq90+
# pav+rr+rr1mm+pn20mm+pfl90+pxcwd+
# pxcdd+pint+pq90+pq99+altitude


# -----------------------------------------------------------------------------#
# Etude des correlations ------------------------------------------------------
# -----------------------------------------------------------------------------#

# -----------------------------------------------------------------------------#
## Correlation avec la variable réponse ----------------------------------------
# -----------------------------------------------------------------------------#
dataCor <- data.frame()
for (catnat in list_CN){
  print(catnat)
  val_catnat <- dataFinaleClimsec %>% select(catnat) 
  
  correlation_value <- as.data.frame(cor(val_catnat, 
                                         dataFinaleClimsec %>% select(list_var_continuous),
                                         use="complete.obs", method="spearman"))
  dataCor <- rbind(dataCor, correlation_value)
}
dataCor <- as.data.frame(t(dataCor))
write.csv2(dataCor, "../Plots&Resultats/correlations/corrSpearmanClimsec.csv")


dataCor_pl <- data.frame()
for (catnat in list_CN){
  print(catnat)
  val_catnat <- dataFinaleClimsec_pl %>% select(catnat) 
  
  correlation_value <- as.data.frame(cor(val_catnat, 
                                         dataFinaleClimsec_pl %>% select(list_var_continuous),
                                         use="complete.obs", method="spearman"))
  dataCor_pl <- rbind(dataCor_pl, correlation_value)
}
dataCor_pl <- as.data.frame(t(dataCor_pl))
write.csv2(dataCor_pl, "../Plots&Resultats/correlations/corrSpearmanClimsec_pl.csv")


# -----------------------------------------------------------------------------#
## Correlations entre variables explicatves ------------------------------------
# -----------------------------------------------------------------------------#

### Sécheresse -----------------------------------------------------------------
dataSec <- dataFinaleClimsec %>% 
  select(-inondationH3, -starts_with("ppri")) %>%
  filter(annee >= 1988) %>%
  rename(part_alea_moyen_fort = part_alea_moyen_fort_commune)

dataCorVarSec = cor(dataFinaleClimsec %>% 
                   select(list_var_continuous, 
                          -starts_with("ppri"),
                          -inondationH3) %>%
                   rename(part_alea_moyen_fort = part_alea_moyen_fort_commune,
                          part_alea_faible = part_alea_faible_commune), use = "complete.obs",
                   method = "spearman")

corrplot(dataCorVarSec, 
         method = "square",
         order = 'hclust',
         type = "upper",
         tl.col="black")

corrplot(dataCorVarSec, 
         method = "square",
         order = 'hclust',
         type = "upper",
         tl.col="black")



### Inondations ----------------------------------------------------------------

dataCorVar = cor(dataFinaleClimsec %>% 
                   select(list_var_continuous, 
                          -starts_with("part_"),
                          -secheresseH3), use = "complete.obs")

View(as.data.frame(dataCorVar))
write.csv2(as.data.frame(dataCorVar), "dataCorVar_inondation.csv")


corrplot(dataCorVar, 
         method = "color",
         order = 'hclust', 
         addrect = 3, rect.lwd = 3,
         tl.col="black")

corrplot(dataCorVar, 
         method = "square",
         order = 'hclust',
         type = "upper",
         tl.col="black")



# -----------------------------------------------------------------------------#
# Stat par groupe --------------------------------------------------------------
# -----------------------------------------------------------------------------#

## Inondation ------------------------------------------------------------------
dataSummaryFlood <- dataFinaleCom %>% 
  select(list_var, inondation) %>%
  group_by(inondation) %>%
  summarise_at(list_var, funs(min,
                              q25=quantile(.,probs = c(0.25), na.rm=T), 
                              mean, median, 
                              q75=quantile(., probs = 0.75, na.rm=T),
                              max))

dataSummaryFlood <- dataSummaryFlood %>%
  pivot_longer(!inondation, names_to="indicateur") %>%
  separate(indicateur, into=c("var", "stat")) %>%
  pivot_wider(names_from = stat, values_from = value)
 
write.csv2(dataSummaryFlood, "dataSummaryFlood.csv")                                                



dataSummaryFloodi <- dataFinaleCom %>% 
  select(list_var, indic_in) %>%
  group_by(indic_in) %>%
  summarise_at(list_var, funs(min,
                              q25=quantile(.,probs = c(0.25), na.rm=T), 
                              mean, median, 
                              q75=quantile(., probs = 0.75, na.rm=T),
                              max)) 

dataSummaryFloodi <- dataSummaryFloodi %>%
  pivot_longer(!indic_in, names_to="indicateur") %>%
  separate(indicateur, into=c("var", "stat")) %>%
  pivot_wider(names_from = stat, values_from = value)

write.csv2(dataSummaryFloodi, "dataSummaryFloodi.csv")  



boxplotFlood <- function(yvar){
  ggplot(dataFinaleCom, aes_(x=~as.factor(inondation),y=as.name(yvar))) +
    geom_boxplot() }

lapply(list_var, boxplotFlood) # toutes les var explicatives d'un coup



## Secheresse ------------------------------------------------------------------
dataSummaryDrought <- dataFinaleClimsec %>% 
  select(list_var, secheresse) %>%
  group_by(secheresse) %>%
  summarise_at(list_var, funs(min,
                              q25=quantile(.,probs = c(0.25), na.rm=T), 
                              mean, median, 
                              q75=quantile(., probs = 0.75, na.rm=T),
                              max)) 

dataSummaryDrought <- dataSummaryDrought %>%
  pivot_longer(!secheresse, names_to="indicateur") %>%
  separate(indicateur, into=c("var", "stat")) %>%
  pivot_wider(names_from = stat, values_from = value)

write.csv2(dataSummaryDrought, "dataSummaryDrought.csv") 




# -----------------------------------------------------------------------------#
# Nuages de points -------------------------------------------------------------
# -----------------------------------------------------------------------------#

## Inondation ------------------------------------------------------------------
plotseriesFlood <- function(yvar){
  ggplot(dataFinaleCom, aes_(x=~inondation,y=as.name(yvar))) +
    geom_bin2d()
}
lapply(list_var, plotseriesFlood) # toutes les var explicatives d'un coup


ggplot(dataFinaleCom, aes(x=inondation, y=tav)) + geom_bin2d() # Une seule var



## Secheresse ------------------------------------------------------------------
plotseriesDrought <- function(yvar){
  ggplot(dataFinaleCom, aes_(x=~secheresse,y=as.name(yvar))) +
    geom_bin2d()
}
lapply(list_var, plotseriesDrought) # toutes les var explicatives d'un coup


ggplot(dataFinaleCom, aes(x=secheresse, y=tav)) + geom_bin2d() # Une seule var


dataFinale_ag <- dataFinaleCom %>% 
  group_by(id) %>%
  summarise(nb_secheresse = sum(secheresse, na.rm = T),
            part_alea_faible_commune = max(part_alea_faible_commune),
            part_alea_moyen_fort_commune = max(part_alea_moyen_fort_commune)) %>%
  mutate(part_avec_alea = part_alea_faible_commune+ part_alea_moyen_fort_commune)

ggplot(dataFinale_ag,  aes_(x=~part_sans_alea, y=~nb_secheresse)) +
  geom_point()







# -----------------------------------------------------------------------------#
# Carte pour un indicateur et une annéee donnée  -----------------------
# -----------------------------------------------------------------------------#

my_annee = 1989

dataPlot_indicateur <- dataFinaleClimsec %>%
  filter(annee==my_annee) %>% 
  merge(com_bound, by.x="id", by.y="code", all.y = T) 


ggplot(data=dataPlot_indicateur) +
  geom_sf(aes(geometry = geometry, fill = sswi_B1), colour = NA) +
  geom_sf(data = dep_bound, aes(geometry = geometry), color="grey40", fill = NA) +
  scale_fill_gradient2(low = "#99000D", mid = "white", high = "#084594", midpoint= 0, limits = c(-2.5,2.5)) +
  theme_void() +
  ggtitle("")

ggplot(data=dataPlot_indicateur) +
  geom_sf(aes(geometry = geometry, fill = pfl90), colour = NA) +
  geom_sf(data = dep_bound, aes(geometry = geometry), color="grey40", fill = NA) +
  scale_fill_gradient2(low = "white", high = "#99000D") +
  theme_void() +
  ggtitle("")


# -----------------------------------------------------------------------------#
# Carte des indicateurs moyens sur la période historique -----------------------
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

  

# -----------------------------------------------------------------------------#
# Stat globales des bases de modelisation                -----------------------
# -----------------------------------------------------------------------------#

dataFinaleClimsec_pl <- read_rds("../Data/modelisation/data_modelisation_1982_2021_all_V2_climsec_pl.rds")

data_modelisation_ino <- dataFinaleClimsec %>% filter(annee > 1984)
sum(data_modelisation_ino$inondation)
mean(data_modelisation_ino$inondation)

data_modelisation_ino <- data_modelisation_ino %>% group_by(id) %>% summarise(nb_ino = sum(inondation))
table(data_modelisation_ino$nb_ino)
1-1858/35228


data_modelisation_sec <- dataFinaleClimsec_pl %>% filter(annee > 1990 & annee < 2021)
sum(data_modelisation_sec$secheresse)
mean(data_modelisation_sec$secheresse)
data_modelisation_sec <- data_modelisation_sec %>% group_by(id) %>% summarise(nb_sec = sum(secheresse))
table(data_modelisation_sec$nb_sec)
1-23023/35228




## Communes remarquables ---------------------------------------
sum_catnat <- dataFinaleCom %>%
  group_by(id, commune) %>%
  summarise(nb_ino = sum(inondation),
            nb_sec = sum(secheresse, na.rm = T))



sum_catnat %>% filter(nb_ino > 20) %>% arrange(-nb_ino)

plot_ly(dataFinaleCom %>% filter(id == "13055"), colors = "Blues") %>%
  add_bars(x =~ annee, y =~ inondation)

plot_ly(dataFinaleCom %>% filter(id == "13055"), colors = "Blues") %>%
  add_bars(x =~ annee, y =~ pfl90)
