library(caTools)
library(randomForest)
library(ranger)
library(vip)
library(rpart)
library(rpart.plot)
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(corrplot)
library(Metrics)
library(plotly)

#DATA------------------------------------------------------
##Import data----------------------------------------------
dataFinaleCom <- read_rds("../Data/modelisation/old/data_modelisation_1982_2021_all_V2_B1_pl.rds")

dataFinaleCom <- dataFinaleCom%>%
mutate(indic_in=ifelse(inondation==0,0,1))


list_communes = unique(dataFinaleCom$id)
list_CN <- c("inondation","secheresse","indic_in")
list_var <- setdiff(names(dataFinaleCom),
  c("id","annee","longM","latM","inondation","secheresse",
  "commune","indic_in","code_dep"))

list_var_ppri <- names(dataFinaleCom)[startsWith(names(dataFinaleCom),"ppri")]

list_var_sec <- setdiff(list_var,
union(c("tnav","tnq10","rr1mm","rr","pav","pn20mm","pq99","pint",
  "part_alea_faible_commune_cat",
  "part_alea_moyen_fort_commune_cat",
  "part_alea"),
  list_var_ppri))

list_var2 <- c("pfl90","tav","tnav","txav","txq10","tnq10","tr","sd",
  "txq90","inondationH3",
  "rr1mm","tnnd","pq90","rr","pav","tncwd","pxcwd","txfd","tnfd","altitude")


my_colors = c("#1DE9B6", "#A6A6A6", "#707070", "#363636")

##Split train/test---------------------------------------
dataFinaleCom <- dataFinaleCom %>%
  mutate(old_periode_sec = annee < 1999)

dataFinaleCom_trainSec <- dataFinaleCom%>%
  filter(1990<annee&annee<2016)

dataFinaleCom_testSec <- dataFinaleCom%>%
  filter(annee>=2016 & annee != 2021)

train <- dataFinaleCom_trainSec %>% select(list_var_sec,secheresse)
test <- dataFinaleCom_testSec %>% select(list_var_sec,secheresse)


##RandomForest---------------------------------------------------------------
start=Sys.time()
fitrf_sec=ranger(secheresse~
                    tav+
                    txav+
                    sd+
                    tx35+
                    txnd+
                    tnht+
                    tr+
                    tnfd+
                    tnnd+
                    txfd+
                    tncwd+
                    txhwd+
                    trav+
                    txq90+
                    txq10+
                    tnq90+
                    pfl90+
                    pxcwd+
                    pxcdd+
                    pq90+
                    inondationH3+
                    secheresseH3+
                    altitude+
                    part_alea_faible_commune+
                    part_alea_moyen_fort_commune+
                    spi+
                    sswi+
                    rr1mm,
  data=dataFinaleCom_trainSec,
  num.trees=700, 
  importance='impurity',
  min.node.size=16000, # A augmenter 
  write.forest=TRUE)
Sys.time()-start


# Evaluation -------------------------------------------------------------------
y_pred_rd <- predict(fitrf_sec, data=dataFinaleCom_testSec)
rmse(dataFinaleCom_testSec$secheresse, y_pred_rd$predictions)

rmse(fitrf_sec$predictions, dataFinaleCom_trainSec$secheresse)


# Save ------------------------------------------
saveRDS(fitrf_sec,"fitrf_sec_1102.rds")



# Fonction ------------------------
rf_sec <- function(trees,node,output=F){
  start=Sys.time()
  fit=ranger(secheresse~
                      tav+
                      txav+
                      sd+
                      tx35+
                      txnd+
                      tnht+
                      tr+
                      tnfd+
                      tnnd+
                      txfd+
                      tncwd+
                      txhwd+
                      trav+
                      txq90+
                      txq10+
                      tnq90+
                      pfl90+
                      pxcwd+
                      pxcdd+
                      pq90+
                      inondationH3+
                      secheresseH3+
                      altitude+
                      part_alea_faible_commune+
                      part_alea_moyen_fort_commune+
                      spi+
                      sswi+
                      rr1mm+
               old_periode_sec,
                    data=dataFinaleCom_trainSec,
                    num.trees=trees, 
                    importance='impurity',
                    min.node.size=node, 
                    write.forest=TRUE)
  print(Sys.time()-start)
  
  # rmse
  y_pred_rd <- predict(fit, data=dataFinaleCom_testSec)
  print(rmse(dataFinaleCom_testSec$secheresse, y_pred_rd$predictions))
  print(rmse(fit$predictions, dataFinaleCom_trainSec$secheresse))
  
  if (output){return(fit)}
}

rf_sec(400,500)
rf_sec(400,1000)
rf_sec(400,2000)
rf_sec(400,3000)
rf_sec(400,4000)
rf_sec(400,5000)
rf_sec(400,8000)
rf_sec(400,10000)
rf_sec(400,12000)
rf_sec(400,14000)
rf_sec(400,16000)
rf_sec(400,18000)
rf_sec(400,20000)

rf_sec(100,500)
rf_sec(100,1000)
rf_sec(100,2000)
rf_sec(100,3000)
rf_sec(100,4000)
rf_sec(100,5000)
rf_sec(100,8000)
rf_sec(100,10000)

rf_sec(700,8000)
rf_sec(700,10000)
rf_sec(700,12000)
rf_sec(700,14000)
rf_700_16k <- rf_sec(700,16000,T)
rf_sec(700,18000)
rf_sec(700,20000)

rf_sec(200,8000)
rf_sec(200,10000)
rf_sec(200,12000)
rf_sec(200,14000)



saveRDS(rf_700_16k, "new_rf_sec.rds")



# -----------------------------------------------------------------------------#
# Evaluation du modele ---------------------------------------------------------
# -----------------------------------------------------------------------------#

fit_plot = rf_700_16k
y_pred_rd <- predict(fit_plot, data=dataFinaleCom_testSec)
rmse(dataFinaleCom_testSec$secheresse, y_pred_rd$predictions)

rmse(fit_plot$predictions, dataFinaleCom_trainSec$secheresse)


dataPlotTrain <- dataFinaleCom_trainSec %>% 
  bind_cols(fit_plot$predictions) %>%
  dplyr::rename('pred'="...58") %>%
  group_by(annee) %>%
  summarise(obs = sum(secheresse), pred = sum(pred))

plot_ly(dataPlotTrain,
        colors = my_colors) %>%
  add_lines(x=~as.factor(annee), y=~obs, name = "observé", color ="1") %>%
  add_lines(x=~as.factor(annee), y=~pred, name = "prédiction RF", color="2") %>%
  layout(yaxis = list(rangemode='tozero', title = "Nombre de CatNat secheresse"),
         xaxis = list(title = "Année"))


dataPlotTest<- dataFinaleCom_testSec %>% 
  bind_cols(y_pred_rd$predictions) %>%
  rename('pred'="...57") %>%
  group_by(annee) %>%
  summarise(obs = sum(secheresse), pred = sum(pred))


plot_ly(dataPlotTest,
        colors = my_colors) %>%
  add_lines(x=~as.factor(annee), y=~obs, name = "observé", color ="1") %>%
  add_lines(x=~as.factor(annee), y=~pred, name = "prédiction RF", color="2") %>%
  layout(yaxis = list(rangemode='tozero', title = "Nombre de CatNat secheresse"),
         xaxis = list(title = "Année"))



# Importance plot
imp_sec <- as.data.frame(ranger::importance(rf_700_16k))



# -----------------------------------------------------------------------------#
# New SSWI Variable ----------------------------------------------------------
# -----------------------------------------------------------------------------#

dataFinaleCom_new <- read_rds("../Data/modelisation/data_modelisation_1982_2021_f.rds")
dataFinaleCom_new_pl <- read_rds("../Data/modelisation/data_modelisation_1982_2021_f_pl.rds")


# Preparation des donnees ------------------------------------------------------
dataFinaleCom_new_pl <- dataFinaleCom_new_pl %>%
  mutate(old_periode_sec = annee < 1999)

dataFinaleNew_trainSec_pl <- dataFinaleCom_new_pl%>%
  filter(1990<annee&annee<2016)

dataFinaleNew_testSec_pl <- dataFinaleCom_new_pl%>%
  filter(annee>=2016 & annee<=2020) # attention année 2021


dataFinaleCom_new <- dataFinaleCom_new %>%
  mutate(old_periode_sec = annee < 1999)

dataFinaleNew_trainSec <- dataFinaleCom_new %>%
  filter(1990<annee&annee<2016)

dataFinaleNew_testSec <- dataFinaleCom_new %>%
  filter(annee>=2016 & annee<=2020) # attention année 2021






# Fonction ------------------------
rf_sec <- function(trees,node,output=F){
  start=Sys.time()
  fit=ranger(secheresse~
               tav+
               txav+
               sd+
               tx35+
               txnd+
               tnht+
               tr+
               tnfd+
               tnnd+
               txfd+
               tncwd+
               txhwd+
               trav+
               txq90+
               txq10+
               tnq90+
               pfl90+
               pxcwd+
               pxcdd+
               pq90+
               inondationH3+
               secheresseH3+
               altitude+
               part_alea_faible_commune+
               part_alea_moyen_fort_commune+
               sswi_min+sswi_max+sswi_moy+
               rr1mm+
               old_periode_sec,
             data=dataFinaleNew_trainSec_pl,
             num.trees=trees, 
             importance='impurity',
             min.node.size=node, 
             write.forest=TRUE)
  print(Sys.time()-start)
  
  # rmse
  y_pred_rd <- predict(fit, data=dataFinaleNew_testSec_pl)
  print(rmse(dataFinaleNew_testSec_pl$secheresse, y_pred_rd$predictions))
  print(rmse(fit$predictions, dataFinaleNew_trainSec_pl$secheresse))
  
  if (output){return(fit)}
}
rf_sec(700,2000)

rf_700_16k_new <- rf_sec(700,16000,T)

saveRDS(rf_700_16k_new, "RF_sec_new_sswi.rds")


fitTest=ranger(secheresse~
             tr+
             txav+
             #txnd+       
             txhwd+
             txq10+
             pav+
             #rr+
             pint+
             pq90+
             part_alea+ # part_alea_moyen_fort_commune +
             old_periode_sec+
             sswi_min+sswi_max+sswi_moy,
           data=dataFinaleNew_trainSec_pl,
           num.trees=700, 
           importance='impurity',
           min.node.size=16000, 
           write.forest=TRUE)
saveRDS(fitTest, "RF_test_new_sswi.rds")


# -----------------------------------------------------------------------------#
# Plots par année --------------------------------------------------------------
# -----------------------------------------------------------------------------#
fit_plot = rf_700_16k_new
y_pred_rd <- predict(fit_plot, data=dataFinaleNew_testSec_pl)
rmse(dataFinaleNew_testSec_pl$secheresse, y_pred_rd$predictions)
rmse(fit_plot$predictions, dataFinaleNew_trainSec_pl$secheresse)


dataPlotTrain <- dataFinaleNew_trainSec_pl %>% 
  bind_cols(fit_plot$predictions) %>%
  dplyr::rename('pred'="...58") %>%
  group_by(annee) %>%
  summarise(obs = sum(secheresse), pred = sum(pred))

plot_ly(dataPlotTrain,
        colors = my_colors) %>%
  add_lines(x=~as.factor(annee), y=~obs, name = "observé", color ="1") %>%
  add_lines(x=~as.factor(annee), y=~pred, name = "prédiction RF", color="2") %>%
  layout(yaxis = list(rangemode='tozero', title = "Nombre de CatNat secheresse"),
         xaxis = list(title = "Année"))


dataPlotTest<- dataFinaleNew_testSec_pl %>% 
  bind_cols(y_pred_rd$predictions) %>%
  rename('pred'="...58") %>%
  group_by(annee) %>%
  summarise(obs = sum(secheresse), pred = sum(pred))


plot_ly(dataPlotTest,
        colors = my_colors) %>%
  add_lines(x=~as.factor(annee), y=~obs, name = "observé", color ="1") %>%
  add_lines(x=~as.factor(annee), y=~pred, name = "prédiction RF", color="2") %>%
  layout(yaxis = list(rangemode='tozero', title = "Nombre de CatNat secheresse"),
         xaxis = list(title = "Année"))



# -----------------------------------------------------------------------------#
# Plots par région -------------------------------------------------------------
# -----------------------------------------------------------------------------#
dep <- read_csv("../Data/cog_ensemble_2021_csv/departement2021.csv") %>%
  rename_with(~ tolower(gsub("#| ", "",.x))) %>%
  select(dep,libelle,reg)
reg <- read_csv("../Data/cog_ensemble_2021_csv/region2021.csv") %>%
  rename_with(~ tolower(gsub("#| ", "",.x))) %>%
  select(reg,libelle)
dep_reg <- merge(dep, reg, by = "reg", suffixes = c("_dep","_reg"))

dep_bound <- read_rds("../Data_GIS/geojson/departements-vs-om.rds") %>%
  dplyr::filter(code<=95)
reg_bound <- sf::st_read("../Data_GIS/geojson/regions.geojson")

# prep data
dataTrain_dep <- dataFinaleNew_trainSec_pl %>% 
  bind_cols(fit_plot$predictions) %>%
  dplyr::rename('pred'="...58") %>%
  mutate(dep = substr(id,1,2)) %>%
  group_by(dep) %>%
  summarise(obs = sum(secheresse), pred = sum(pred)) %>%
  mutate(delta = (pred-obs)/obs) %>%
  merge(dep_bound, by.x = "dep", by.y = "code")

dataTrain_reg <- dataFinaleNew_trainSec_pl %>% 
  bind_cols(fit_plot$predictions) %>%
  dplyr::rename('pred'="...58") %>%
  mutate(dep = substr(id,1,2)) %>%
  merge(dep_reg, by = "dep") %>%
  group_by(libelle_reg, reg) %>%
  summarise(obs = sum(secheresse), pred = sum(pred)) %>%
  mutate(delta = (pred-obs)/obs) %>%
  merge(reg_bound, by.x = "reg", by.y = "code")

dataTest_reg <- dataFinaleNew_testSec_pl %>% 
  bind_cols(y_pred_rd$predictions) %>%
  dplyr::rename('pred'="...58") %>%
  mutate(dep = substr(id,1,2)) %>%
  merge(dep_reg, by = "dep") %>%
  group_by(libelle_reg, reg) %>%
  summarise(obs = sum(secheresse), pred = sum(pred)) %>%
  mutate(delta = (pred-obs)/obs) %>%
  merge(reg_bound, by.x = "reg", by.y = "code")

# plots
ggplot(data=dataTrain_dep ) +
  geom_sf(aes(geometry = geometry, fill = obs), colour = NA) +
  geom_sf(data = dep_bound, aes(geometry = geometry), color="grey40", fill = NA) +
  scale_fill_gradient(low = "white", high = "#99000D") +
  theme_void() +
  ggtitle("")

ggplot(data=dataTrain_dep ) +
  geom_sf(aes(geometry = geometry, fill = delta), colour = NA) +
  geom_sf(data = reg_bound, aes(geometry = geometry), color="grey40", fill = NA) +
  scale_fill_gradient2(low = "#084594", mid = "white", high = "#99000D", 
                       midpoint= 0, limits = c(-20,20),
                       name = "Ecart relatif") +
  theme_void() + 
  ggtitle("")

