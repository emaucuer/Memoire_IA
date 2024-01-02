# -----------------------------------------------------------------------------#
#               Projection à horizon 2050 pour la sécheresse
# -----------------------------------------------------------------------------#

#setwd("C:/Users/emaucuer/OneDrive - SIA PARTNERS/BU_ACT_FR/4 - Mémoires/Mémoires 2022/Mémoire de MAUCUER Eurydice/Code")

library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(ranger)
library(sf)
library(scales)

source("../00_lib_externe.R")

# Chargement du modele
fit <- readRDS("../fitrf_sec_1102.rds")
fit <- readRDS("../new_rf_sec.rds") # avec le scenario B1  pour 1999-2020
fit <- readRDS("../RF_sec_new_sswi.rds") # avec les nouveaux sswi

# Chargement des données de projection et des données de modélisation
data85 <- read_rds("../../Data/projection/data_proj_2022_2050_RCP85_A2.rds")
data45 <- read_rds("../../Data/projection/data_proj_2022_2050_RCP45_B1.rds")
data26 <- read_rds("../../Data/projection/data_proj_2022_2050_RCP26_B1.rds")
#dataFinaleCom <- read_rds("../../Data/modelisation/old/data_modelisation_1982_2021_all_V2_A2.rds") 
#dataFinaleCom <- read_rds("../../Data/modelisation/data_modelisation_1982_2021_all_V2_climsec.rds") 
dataFinaleCom <- read_rds("../../Data/modelisation/data_modelisation_1982_2021_f_pl.rds") 

# data de cartographie
com_bound <- read_rds("../../Data/Data_GIS/geojson/communes-vs.rds")
dep_bound <- read_rds("../../Data/Data_GIS/geojson/departements-vs-om.rds") %>%
  dplyr::filter(code<=95)


# -----------------------------------------------------------------------------#
# Importance des variables ----------------------------------------------------
# -----------------------------------------------------------------------------#

# Importance plot
imp_sec <- as.data.frame(ranger::importance(fit))

# write.csv2(imp_sec, file = "imp_sec_new.csv")






# -----------------------------------------------------------------------------#
# Prediction 2050  -------------------------------------------------------------
# -----------------------------------------------------------------------------#
y_pred_2050_RCP85 <- predict(fit, data=data85 %>% filter(annee==2050))
y_pred_2050_RCP45 <- predict(fit, data=data45 %>% filter(annee==2050))
y_pred_2050_RCP26 <- predict(fit, data=data26 %>% filter(annee==2050))


data85_2050 <- bind_cols(data85 %>% filter(annee==2050), y_pred_2050_RCP85$predictions) %>%
  rename("pred" = "...56")
data45_2050 <- bind_cols(data45 %>% filter(annee==2050), y_pred_2050_RCP45$predictions) %>%
  rename("pred" = "...56")
data26_2050 <- bind_cols(data26 %>% filter(annee==2050), y_pred_2050_RCP26$predictions) %>%
  rename("pred" = "...56")


data85_2050 <- data85_2050 %>% 
  mutate(pred_cat = pred %/% 0.05)

data85_2050$pred_cat <- ordered(data85_2050$pred_cat,
                                levels = c(0,1,2,3,4,5,6,7,8,9),
                                labels = c("<= 0.05", 
                                           "0.05 à 0.1",
                                           "0.1 à 0.2", "0.1 à 0.2",
                                           "0.2 à 0.3", "0.2 à 0.3", 
                                           "0.3 à 0.4","0.3 à 0.4",
                                           ">= 0.4", ">= 0.4"))

data45_2050 <- data45_2050 %>% 
  mutate(pred_cat = pred %/% 0.05)

data45_2050$pred_cat <- ordered(data45_2050$pred_cat,
                                levels = c(0,1,2,3,4,5,6,7,8,9,10),
                                labels = c("<= 0.05", 
                                           "0.05 à 0.1",
                                           "0.1 à 0.2", "0.1 à 0.2",
                                           "0.2 à 0.3", "0.2 à 0.3", 
                                           "0.3 à 0.4","0.3 à 0.4",
                                           ">= 0.4", ">= 0.4", ">= 0.4"))

data26_2050 <- data26_2050 %>% 
  mutate(pred_cat = pred %/% 0.05)

data26_2050$pred_cat <- ordered(data26_2050$pred_cat,
                                levels = c(0,1,2,3,4,5,6,7,8,9),
                                labels = c("<= 0.05", 
                                           "0.05 à 0.1",
                                           "0.1 à 0.2", "0.1 à 0.2",
                                           "0.2 à 0.3", "0.2 à 0.3", 
                                           "0.3 à 0.4","0.3 à 0.4",
                                           ">= 0.4", ">= 0.4"))

# -----------------------------------------------------------------------------#
# Plot des prédictions 2050 ----------------------------------------------------
# -----------------------------------------------------------------------------#

### RCP85 ----------------------------------------------------------------------
dataPlot85 <- merge(data85_2050, com_bound, by.x = "id", by.y = "code") %>%
  filter(id < 96000)
sum(data85_2050$pred)

plot_carto_gradient(dataPlot85, "pred", color_high="#B90000")
ggplot(data=dataPlot85) +
  geom_sf(aes(geometry = geometry, fill = pred), colour = NA) +
  geom_sf(data = dep_bound, aes(geometry = geometry), color="grey40", fill = NA) +
  scale_fill_gradient(low = "white", high = "#B90000", limits = c(0,0.51)) +
  theme_void() 

#png("secRF85.png", units="in", width=8.5, height=5, res=300)
colors_red_ <- c('white', '#fee5d9','#fc9272','#de2d26','#a50f15','#67000d')
plot_carto_manual(dataPlot85, "pred_cat", colors_red_, color_contour="grey70", legend_title="Prédiction fréquence 2050")
ggplot(data=dataPlot85) +
  geom_sf(aes(geometry = geometry, fill = pred_cat), color = NA) +
  scale_fill_manual(values = c('white', '#fee5d9','#fc9272','#de2d26','#a50f15','#67000d'),
                    name = "Prédiction fréquence 2050") +
  geom_sf(data = dep_bound, aes(geometry = geometry), color="grey70", fill = NA) +
  theme_void() + 
  theme(legend.position = c(1.1, .5))
dev.off()


### RCP45 ----------------------------------------------------------------------
dataPlot45 <- merge(data45_2050, com_bound, by.x = "id", by.y = "code") %>%
  filter(id < 96000)
sum(data45_2050$pred)

ggplot(data=dataPlot45) +
  geom_sf(aes(geometry = geometry, fill = pred), colour = NA) +
  geom_sf(data = dep_bound, aes(geometry = geometry), color="grey40", fill = NA) +
  scale_fill_gradient(low = "white", high = "#B90000", limits = c(0,0.51)) +
  theme_void() 

#png("secRF45.png", units="in", width=8.5, height=5, res=300)
plot_carto_manual(dataPlot45, "pred_cat", colors_red_, color_contour="grey70", legend_title="Prédiction fréquence 2050")
ggplot(data=dataPlot45) +
  geom_sf(aes(geometry = geometry, fill = pred_cat), color = NA) +
  scale_fill_manual(values = c('white', '#fee5d9','#fc9272','#de2d26','#a50f15','#67000d'),
                    name = "Prédiction fréquence 2050") +
  geom_sf(data = dep_bound, aes(geometry = geometry), color="grey70", fill = NA) +
  theme_void() + 
  theme(legend.position = c(1.1, .5))
dev.off()


### RCP26 ----------------------------------------------------------------------
dataPlot26 <- merge(data26_2050, com_bound, by.x = "id", by.y = "code") %>%
  filter(id < 96000)
sum(data26_2050$pred)

ggplot(data=dataPlot26) +
  geom_sf(aes(geometry = geometry, fill = pred), colour = NA) +
  geom_sf(data = dep_bound, aes(geometry = geometry), color="grey40", fill = NA) +
  scale_fill_manual(values = c('white', '#fee5d9','#fc9272','#de2d26','#a50f15','#67000d')) +
  theme_void() 

#png("secRF26.png", units="in", width=8.5, height=5, res=300)
plot_carto_manual(dataPlot26, "pred_cat", colors_red_, color_contour="grey70", legend_title="Prédiction fréquence 2050")
ggplot(data=dataPlot26) +
  geom_sf(aes(geometry = geometry, fill = pred_cat), color = NA) +
  scale_fill_manual(values = c('white', '#fee5d9','#fc9272','#de2d26','#a50f15','#67000d'),
                    name = "Prédiction fréquence 2050") +
  geom_sf(data = dep_bound, aes(geometry = geometry), color="grey70", fill = NA) +
  theme_void() + 
  theme(legend.position = c(1.1, .5))
dev.off()


### Nouveau format de plot pour White paper ------------------------------------
# library(data.table)
# setDT(data26_2050)
# data26_2050$pred_cat2 <- "0 - 0.05"
# data26_2050[pred > 0.05]$pred_cat2 <- "0.05 - 0.1"
# data26_2050[pred > 0.1]$pred_cat2 <- "0.1 - 0.2"
# data26_2050[pred > 0.2]$pred_cat2 <- "0.2 +"
# 
# setDT(data45_2050)
# data45_2050$pred_cat2 <- "0 - 0.05"
# data45_2050[pred > 0.05]$pred_cat2 <- "0.05 - 0.1"
# data45_2050[pred > 0.1]$pred_cat2 <- "0.1 - 0.2"
# data45_2050[pred > 0.2]$pred_cat2 <- "0.2 +"
# 
# setDT(data85_2050)
# data85_2050$pred_cat2 <- "0 - 0.05"
# data85_2050[pred > 0.05]$pred_cat2 <- "0.05 - 0.1"
# data85_2050[pred > 0.1]$pred_cat2 <- "0.1 - 0.2"
# data85_2050[pred > 0.2]$pred_cat2 <- "0.2 +"
# 
# 
# 
# dataPlotWP <- merge(data45_2050, com_bound, by.x = "id", by.y = "code") %>%
#   filter(id < 96000)
# sum(dataPlot$pred)
# 
# color_red <-  c("#FFE6DF","#F5AD99","#FE754E","#900C3F")
# color_blue_green <-  c("#F1F1F1","#91B1A7","#5F7F75","#0C1E18")
# 
# plot_manual(dataPlotWP, "pred_cat", color_blue_green, color_contour=grey70", title="Fréquence projetée", legend_position="none")
# ggplot(data=dataPlotWP) +
#   geom_sf(aes(geometry = geometry,fill = pred_cat2),color=NA)+
#   geom_sf(data = dep_bound, aes(geometry = geometry), color=NA, fill = NA)+
#   scale_fill_manual(values = color_blue_green,na.value = "white", name="Fréquence projetée")+
#   labs(fill="Fr?quence projet?e")+
#   theme_void()+
#   theme(legend.position = "none")



# -----------------------------------------------------------------------------#
# Prédicion pour chaque année de projection ------------------------------------
# -----------------------------------------------------------------------------#
y_pred_RCP85 <- predict(fit, data=data85)
y_pred_RCP45 <- predict(fit, data=data45)
y_pred_RCP26 <- predict(fit, data=data26)

data85 <- bind_cols(data85, y_pred_RCP85$predictions) %>%
  rename("pred" = "...56")
data45 <- bind_cols(data45, y_pred_RCP45$predictions) %>%
  rename("pred" = "...56")
data26 <- bind_cols(data26, y_pred_RCP26$predictions) %>%
  rename("pred" = "...56")

# write_rds(data85, "../../Data/Projection/data_proj_2022_2050_RCP85_A2_resultats.rds")
# write_rds(data45, "../../Data/Projection/data_proj_2022_2050_RCP45_B1_resultats.rds")
# write_rds(data26, "../../Data/Projection/data_proj_2022_2050_RCP26_B1_resultats.rds")

data_pred_ag_sec <- data85 %>% group_by(annee) %>% summarise(sum_85 = sum(pred)) %>%
  merge(data45 %>% group_by(annee) %>% summarise(sum_45 = sum(pred)), 
        by = "annee") %>%
  merge(data26 %>% group_by(annee) %>% summarise(sum_26 = sum(pred)),
        by = "annee")

#write.csv2(data_pred_ag_sec, "../../Plots&Resultats/data_pred_ag_sec_new.csv")



# -----------------------------------------------------------------------------#
# Comparaison avec la période actuelle -----------------------------------------
# -----------------------------------------------------------------------------#
create_freq_n <- function(n, data){
  dataFreq <- data %>% 
    filter(annee>2021-n) %>%
    group_by(id) %>%
    summarize(nb_reco = sum(secheresse, na.rm = T),
              nb_annee = n()) %>%
    mutate(freq = nb_reco/nb_annee) %>% 
    rename_with(.fn = ~paste0("freq", n), .cols = freq) %>% 
    rename_with(.fn = ~paste0("nb_reco", n), .cols = nb_reco)
  
  return(dataFreq)
}

freq_actuelle_30 <- create_freq_n(30, dataFinaleCom)
freq_actuelle_25 <- create_freq_n(25, dataFinaleCom)
freq_actuelle_20 <- create_freq_n(20, dataFinaleCom)
freq_actuelle_15 <- create_freq_n(15, dataFinaleCom)
freq_actuelle_10 <- create_freq_n(10, dataFinaleCom)
freq_actuelle_5 <- create_freq_n(5, dataFinaleCom)


freq_actuelle <- dataFinaleCom %>% 
  group_by(id) %>%
  summarize(nb_reco = sum(secheresse, na.rm = T),
            nb_annee = n()) %>%
  mutate(freq = nb_reco/nb_annee) %>%
  merge(freq_actuelle_30, by = c("id")) %>%
  merge(freq_actuelle_25, by = c("id")) %>%
  merge(freq_actuelle_20, by = c("id")) %>%
  merge(freq_actuelle_15, by = c("id")) %>%
  merge(freq_actuelle_10, by = c("id")) %>%
  merge(freq_actuelle_5, by = c("id")) 


delta_freq_all  <- data85_2050 %>%
  rename(pred85 = pred) %>%
  select(id, longM, latM, pred85) %>%
  bind_cols(y_pred_2050_RCP45$predictions) %>%
  bind_cols(y_pred_2050_RCP26$predictions) %>%
  rename("pred45" = "...5", pred26 = "...6")%>%
  merge(freq_actuelle, by = c("id")) %>%
  mutate(delta_85 = pred85-freq,
         delta30_85 = pred85-freq30,
         delta25_85 = pred85-freq25,
         delta20_85 = pred85-freq20,
         delta15_85 = pred85-freq15,
         delta10_85 = pred85-freq10,
         delta5_85 = pred85-freq5,
         delta_45 = pred45-freq,
         delta30_45 = pred45-freq30,
         delta25_45 = pred45-freq25,
         delta20_45 = pred45-freq20,
         delta15_45 = pred45-freq15,
         delta10_45 = pred45-freq10,
         delta5_45 = pred45-freq5,
         delta_26 = pred26-freq,
         delta30_26 = pred26-freq30,
         delta25_26 = pred26-freq25,
         delta20_26 = pred26-freq20,
         delta15_26 = pred26-freq15,
         delta10_26 = pred26-freq10,
         delta5_26 = pred26-freq5) %>%
  mutate(evol_15_85 = ifelse(freq15!=0, delta15_85/freq15, NA),
         evol_85 = ifelse(freq!=0, delta_85/freq, NA),
         delta15_cat_85 = ifelse(delta15_85 < -0.1, 0,
                              ifelse(delta15_85 < -0.02, 1,
                                     ifelse(delta15_85 < 0.02, 2,
                                            ifelse(delta15_85 < 0.1, 3,
                                                   ifelse(delta15_85 < 0.2, 4, 5))))),
         evol_15_45 = ifelse(freq15!=0, delta15_45/freq15, NA),
         evol_45 = ifelse(freq!=0, delta_45/freq, NA),
         delta15_cat_45 = ifelse(delta15_45 < -0.1, 0,
                                 ifelse(delta15_45 < -0.02, 1,
                                        ifelse(delta15_45 < 0.02, 2,
                                               ifelse(delta15_45 < 0.1, 3,
                                                      ifelse(delta15_45 < 0.2, 4, 5))))),
         evol_15_26 = ifelse(freq15!=0, delta15_26/freq15, NA),
         evol_26 = ifelse(freq!=0, delta_26/freq, NA),
         delta15_cat_26 = ifelse(delta15_26 < -0.1, 0,
                                 ifelse(delta15_26 < -0.02, 1,
                                        ifelse(delta15_26 < 0.02, 2,
                                               ifelse(delta15_26 < 0.1, 3,
                                                      ifelse(delta15_26 < 0.2, 4, 5))))))



delta_freq_all$delta15_cat_85 <- ordered(delta_freq_all$delta15_cat_85,
                                levels = 0:5,
                                labels = c("< -0.1", 
                                           "-0.1 à -0.02",
                                           "-0.02 à 0.02", 
                                           "0.05 à 0.1", 
                                           "0.1 à 0.2", 
                                           ">= 0.2"))
delta_freq_all$delta15_cat_45 <- ordered(delta_freq_all$delta15_cat_45,
                                    levels = 0:5,
                                    labels = c("< -0.1", 
                                               "-0.1 à -0.02",
                                               "-0.02 à 0.02", 
                                               "0.05 à 0.1", 
                                               "0.1 à 0.2", 
                                               ">= 0.2"))
delta_freq_all$delta15_cat_26 <- ordered(delta_freq_all$delta15_cat_26,
                                    levels = 0:5,
                                    labels = c("< -0.1", 
                                               "-0.1 à -0.02",
                                               "-0.02 à 0.02", 
                                               "0.05 à 0.1", 
                                               "0.1 à 0.2", 
                                               ">= 0.2"))

summary(delta_freq_all)



## Plot delta frequence --------------------------------------------------------
dataPlot_delta <- delta_freq_all %>% merge(com_bound, by.x="id", by.y="code", all.y = T)


# Plot continus
plot_carto_gradient2(dataPlot_delta, "delta15_45", color_low="#C8CBE3", color_high="#99000D", my_limits=c(-0.5, 0.5))
ggplot(data=dataPlot_delta) +
  geom_sf(aes(geometry = geometry, fill = delta15_45), colour = NA) +
  geom_sf(data = dep_bound, aes(geometry = geometry), color="grey40", fill = NA) +
  scale_fill_gradient2(low = "#C8CBE3", mid = "white", high = "#99000D", 
                       midpoint= 0, limits = c(-0.5, 0.5)) +
  theme_void() +
  ggtitle("")

plot_carto_gradient2(dataPlot_delta, "delta15_26", color_low="#084594", color_high="#99000D", my_limits=c(-0.5, 0.12))
ggplot(data=dataPlot_delta) +
  geom_sf(aes(geometry = geometry, fill = delta15_26), colour = NA) +
  geom_sf(data = dep_bound, aes(geometry = geometry), color="grey40", fill = NA) +
  scale_fill_gradientn(colours = c("#084594","white","#99000D"), 
                       values = rescale(c(-0.5, 0, 0.12)),
                       limits = c(-0.5, 0.12)) +
  theme_void() +
  ggtitle("")


# Plot catégorisé
colors_delta <- c('#67a9cf','#d1e5f0','white','#fddbc7','#ef8a62','#b2182b')
#png("map_delta15_sec26.png", units="in", width=8.5, height=5, res=300)
plot_carto_manual(dataPlot_delta, "delta15_cat_26", colors_delta, color_contour="grey70", legend_title="Delta de fréquence")
ggplot(data=dataPlot_delta) +
  geom_sf(aes(geometry = geometry, fill = delta15_cat_26), color = NA) +
  scale_fill_manual(values = c('#67a9cf','#d1e5f0','white','#fddbc7','#ef8a62','#b2182b'), name = "Delta de fréquence") +
  geom_sf(data = dep_bound, aes(geometry = geometry), color="grey70", fill = NA) +
  theme_void() +
  theme(legend.position = c(1,0.5))
dev.off()


# plot par departement

delta_dep <- delta_freq_all %>%
  mutate(dep = substr(id,1,2)) %>%
  group_by(dep) %>%
  summarise(freq = sum(freq),
            freq15 = sum(freq15),
           delta15_26 = sum(delta15_26),
           delta15_45 = sum(delta15_45),
           delta15_85 = sum(delta15_85),
           delta_26 = sum(delta_26),
           delta_45 = sum(delta_45),
           delta_85 = sum(delta_85)) %>%
  mutate(evol_26 = delta_26/freq,
         evol_45 = delta_45/freq,
         evol_85 = delta_85/freq,
         evol15_26 = delta15_26/freq,
         evol15_45 = delta15_45/freq,
         evol15_85 = delta15_85/freq) %>%
  mutate(evol_cat_26 = ifelse(evol_26 == Inf, Inf, 
                              ifelse(evol_26 < -0.5, 0,
                                     ifelse(evol_26 < -0.2, 1,
                                            ifelse(evol_26 < 0.2, 2,
                                              ifelse(evol_26 < 1, 3,
                                                     ifelse(evol_26 < 2, 4,
                                                            ifelse(evol_26 < 10, 5, 6))))))),
         evol_cat_45 = ifelse(evol_45 == Inf, Inf, 
                              ifelse(evol_45 < -0.5, 0,
                                     ifelse(evol_45 < -0.2, 1,
                                            ifelse(evol_45 < 0.2, 2,
                                                  ifelse(evol_45 < 1, 3,
                                                        ifelse(evol_45 < 2, 4,
                                                              ifelse(evol_45 < 10, 5, 6))))))),
         evol_cat_85 = ifelse(evol_85 == Inf, Inf, 
                              ifelse(evol_85 < -0.5, 0,
                                     ifelse(evol_85 < -0.2, 1,
                                            ifelse(evol_85 < 0.2, 2,
                                                   ifelse(evol_85 < 1, 3,
                                                          ifelse(evol_85 < 2, 4,
                                                                 ifelse(evol_85 < 10, 5, 6)))))))) %>%
  merge(dep_bound, by.x="dep", by.y="code", all.y = T) 


delta_dep$evol_cat_26 <- ordered(delta_dep$evol_cat_26,
                                           levels = 0:6,
                                           labels = c("< -50%", 
                                                      "-50% à -20%",
                                                      "-20% à +20%", 
                                                      "+20% à +100%",
                                                      "+100% à +200%", 
                                                      "+200% à +1000%", 
                                                      "> +1000%"))
delta_dep$evol_cat_45 <- ordered(delta_dep$evol_cat_45,
                                 levels = 0:6,
                                 labels =c("< -50%", 
                                           "-50% à -20%",
                                           "-20% à +20%", 
                                           "+20% à +100%",
                                           "+100% à +200%", 
                                           "+200% à +1000%", 
                                           "> +1000%"))
delta_dep$evol_cat_85 <- ordered(delta_dep$evol_cat_85,
                                 levels = 0:6,
                                 labels = c("< -50%", 
                                            "-50% à -20%",
                                            "-20% à +20%", 
                                            "+20% à +100%",
                                            "+100% à +200%", 
                                            "+200% à +1000%", 
                                            "> +1000%"))

plot_carto_gradient2(delta_dep, "evol_85", color_low="#084594", color_high="#99000D", my_limits=c(-20,20), legend_title="Ecart relatif")
ggplot(data=delta_dep ) +
  geom_sf(aes(geometry = geometry, fill = evol_85), colour = NA) +
  geom_sf(data = dep_bound, aes(geometry = geometry), color="grey40", fill = NA) +
  scale_fill_gradient2(low = "#084594", mid = "white", high = "#99000D", 
                       midpoint= 0, limits = c(-20,20),
                       name = "Ecart relatif") +
  theme_void() + 
  ggtitle("")


pal1 <- c('#92c5de','#d1e5f0','white','#fddbc7',"#f4a582",'#d6604d','#b2182b')
# png("map_evol_freq_dep_45.png", units="in", width=8.5, height=5, res=300)
plot_carto_manual(delta_dep, "evol_cat_45", pal1, color_contour="grey70", legend_title="Evolution de fréquence")
ggplot(data=delta_dep) +
  geom_sf(aes(geometry = geometry, fill = evol_cat_45), color = NA) +
  scale_fill_manual(values = c('#92c5de','#d1e5f0','white','#fddbc7',"#f4a582",'#d6604d','#b2182b'), name = "Evolution de fréquence") +
  geom_sf(data = dep_bound, aes(geometry = geometry), color="grey70", fill = NA) +
  theme_void()
dev.off()

pal2 <- c('#d1e5f0','white','#fddbc7',"#f4a582",'#d6604d','#b2182b')
# png("map_evol_freq_dep_85.png", units="in", width=8.5, height=5, res=300)
plot_carto_manual(delta_dep, "evol_cat_85", pal2, color_contour="grey70", legend_title="Evolution de fréquence")
ggplot(data=delta_dep) +
  geom_sf(aes(geometry = geometry, fill = evol_cat_85), color = NA) +
  scale_fill_manual(values = c('#d1e5f0','white','#fddbc7',"#f4a582",'#d6604d','#b2182b'), name = "Evolution de fréquence") +
  geom_sf(data = dep_bound, aes(geometry = geometry), color="grey70", fill = NA) +
  theme_void() 
dev.off()



  

