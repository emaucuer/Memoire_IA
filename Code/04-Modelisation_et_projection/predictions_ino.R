# -----------------------------------------------------------------------------#
#               Projection à horizon 2050 pour la sécheresse
# -----------------------------------------------------------------------------#

library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(ranger)
library(rpart.plot)

# Chargement du modele
fitrf_ino <- readRDS("fitrf_ino.rds")
fitrf_ino <- readRDS("new_fit_rf_ino.rds")

# Chargement des données de projection et des données de modélisation
data85 <- read_rds("../Data/projection/data_proj_2022_2050_RCP85_A2.rds")
data45 <- read_rds("../Data/projection/data_proj_2022_2050_RCP45_B1.rds")
data26 <- read_rds("../Data/projection/data_proj_2022_2050_RCP26_B1.rds")
dataFinaleCom <- read_rds("../Data/modelisation/data_modelisation_1982_2021_f.rds")

# data de cartographie
com_bound <- read_rds("../Data_GIS/geojson/communes-vs.rds")
dep_bound <- read_rds("../Data_GIS/geojson/departements-vs-om.rds") %>%
  dplyr::filter(code<=95)


# -----------------------------------------------------------------------------#
# Importance des variables ----------------------------------------------------
# -----------------------------------------------------------------------------#

plot(fit_ino)

# Importance plot
imp_ino <- as.data.frame(ranger::importance(fitrf_ino))

# Variable importance plot
ggplot(fit_ino$variable.importance, aes(x=reorder(variable,importance), y=importance,fill=importance))+ 
  geom_bar(stat="identity", position="dodge")+ coord_flip()+
  ylab("Variable Importance")+
  xlab("")+
  ggtitle("Information Value Summary")+
  guides(fill=F)+
  scale_fill_gradient(low="red", high="blue")


# -----------------------------------------------------------------------------#
# RMSE
# -----------------------------------------------------------------------------#




# -----------------------------------------------------------------------------#
# prediction 2050  -------------------------------------------------------------
# -----------------------------------------------------------------------------#
y_pred_2050_RCP85 <- predict(fitrf_ino, data=data85 %>% filter(annee==2050))
y_pred_2050_RCP45 <- predict(fitrf_ino, data=data45 %>% filter(annee==2050))
y_pred_2050_RCP26 <- predict(fitrf_ino, data=data26 %>% filter(annee==2050))


data85_2050 <- bind_cols(data85 %>% filter(annee==2050), y_pred_2050_RCP85$predictions) %>%
  rename("pred" = "...52")
data45_2050 <- bind_cols(data45 %>% filter(annee==2050), y_pred_2050_RCP45$predictions) %>%
  rename("pred" = "...52")
data26_2050 <- bind_cols(data26 %>% filter(annee==2050), y_pred_2050_RCP26$predictions) %>%
  rename("pred" = "...52")



# -----------------------------------------------------------------------------#
# plot des prédictions 2050 ----------------------------------------------------
# -----------------------------------------------------------------------------#


# plots
dataPlot85 <- merge(data85_2050, com_bound, by.x = "id", by.y = "code") %>%
  filter(id < 96000)

ggplot(data=dataPlot85) +
  geom_sf(aes(geometry = geometry, fill = pred), colour = NA) +
  geom_sf(data = dep_bound, aes(geometry = geometry), color="grey40", fill = NA) +
  scale_fill_gradient(low = "white", high = "#084594", limits = c(0,0.27)) +
  theme_void() 


dataPlot45 <- merge(data45_2050, com_bound, by.x = "id", by.y = "code")

ggplot(data=dataPlot45) +
  geom_sf(aes(geometry = geometry, fill = pred), colour = NA) +
  geom_sf(data = dep_bound, aes(geometry = geometry), color="grey40", fill = NA) +
  scale_fill_gradient(low = "white", high = "#084594", limits = c(0,0.27)) +
  theme_void() 


dataPlot26 <- merge(data26_2050, com_bound, by.x = "id", by.y = "code") 

ggplot(data=dataPlot26) +
  geom_sf(aes(geometry = geometry, fill = pred), colour = NA) +
  geom_sf(data = dep_bound, aes(geometry = geometry), color="grey40", fill = NA) +
  scale_fill_gradient(low = "white", high = "#084594", limits = c(0,0.2704)) +
  theme_void() 


# -----------------------------------------------------------------------------#
# Prédicion pour chaque année de prjection -------------------------------------
# -----------------------------------------------------------------------------#
y_pred_RCP85 <- predict(fitrf_ino, data=data85)
y_pred_RCP45 <- predict(fitrf_ino, data=data45)
y_pred_RCP26 <- predict(fitrf_ino, data=data26)

data85 <- bind_cols(data85, y_pred_RCP85$predictions) %>%
  rename("pred" = "...52")
data45 <- bind_cols(data45, y_pred_RCP45$predictions) %>%
  rename("pred" = "...52")
data26 <- bind_cols(data26, y_pred_RCP26$predictions) %>%
  rename("pred" = "...52")

data85_ag <- data85 %>% 
  group_by(annee) %>%
  summarise(sum = sum(pred))

write.csv2(data85_ag, "data85_ag.csv")

data26_ag <- data26 %>% 
  group_by(annee) %>%
  summarise(sum = sum(pred))
write.csv2(data26_ag, "data26_ag.csv")


# -----------------------------------------------------------------------------#
# Comparaison avec la période actuelle -----------------------------------------
# -----------------------------------------------------------------------------#
create_freq_n <- function(n, data){
  dataFreq <- data %>% 
    filter(annee>2021-n) %>%
    group_by(id) %>%
    summarize(nb_reco = sum(inondation),
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
  summarize(nb_reco = sum(inondation),
            nb_annee = n()) %>%
  mutate(freq = nb_reco/nb_annee) %>%
  merge(freq_actuelle_30, by = c("id")) %>%
  merge(freq_actuelle_25, by = c("id")) %>%
  merge(freq_actuelle_20, by = c("id")) %>%
  merge(freq_actuelle_15, by = c("id")) %>%
  merge(freq_actuelle_10, by = c("id")) %>%
  merge(freq_actuelle_5, by = c("id")) 
  


delta_freq85 <- data85_2050 %>%
  select(id, annee, longM, latM, pred) %>%
  merge(freq_actuelle, by = c("id")) %>%
  mutate(delta = pred-freq,
         delta30 = pred-freq30,
         delta25 = pred-freq25,
         delta20 = pred-freq20,
         delta15 = pred-freq15,
         delta10 = pred-freq10,
         delta5 = pred-freq5)

delta_freq45 <- data45_2050 %>%
  select(id, annee, longM, latM, pred) %>%
  merge(freq_actuelle, by = c("id")) %>%
  mutate(delta = pred-freq,
         delta30 = pred-freq30,
         delta25 = pred-freq25,
         delta20 = pred-freq20,
         delta15 = pred-freq15,
         delta10 = pred-freq10,
         delta5 = pred-freq5)

delta_freq26 <- data26_2050 %>%
  select(id, annee, longM, latM, pred) %>%
  merge(freq_actuelle, by = c("id")) %>%
  mutate(delta = pred-freq,
         delta30 = pred-freq30,
         delta25 = pred-freq25,
         delta20 = pred-freq20,
         delta15 = pred-freq15,
         delta10 = pred-freq10,
         delta5 = pred-freq5)


summary(delta_freq85)
summary(delta_freq45)
summary(delta_freq26)

## Plot delta frequence --------------------------------------------------------
dataPlot_delta85 <- delta_freq85 %>% merge(com_bound, by.x="id", by.y="code", all.y = T)
dataPlot_delta45 <- delta_freq45 %>% merge(com_bound, by.x="id", by.y="code", all.y = T)
dataPlot_delta26 <- delta_freq26 %>% merge(com_bound, by.x="id", by.y="code", all.y = T)

ggplot(data=dataPlot_delta26 ) +
  geom_sf(aes(geometry = geometry, fill = delta15), colour = NA) +
  geom_sf(data = dep_bound, aes(geometry = geometry), color="grey40", fill = NA) +
  scale_fill_gradient2(low = "#084594", mid = "white", high = "#99000D", 
                       midpoint= 0, limits = c(-0.7,0.6)) +
  theme_void() +
  ggtitle("")
