# -----------------------------------------------------------------------------#
#                   Modele GLM opour les secheresses
# -----------------------------------------------------------------------------#


setwd("G:/Drive partagés/BU_ACT_FR/4 - Mémoires/Mémoires 2022/Mémoire de MAUCUER Eurydice/Code")


library(readr)
library(Metrics)
library(MASS)
library(dplyr)
library(plotly)



# Fonction d'évaluation des modèles --------------------------------------------
my_colors = c("#1DE9B6", "#A6A6A6", "#707070", "#363636")

eval_model_sec <- function(model, trainset, testset){
  dataPlot_train <- trainset %>% mutate(pred = predict(model, type = "response"))
  dataPlot_test <- testset %>% 
    mutate(pred = predict(model, type = "response", newdata=testset))
  
  
  print(paste("RMSE train :",rmse(dataPlot_train$secheresse, dataPlot_train$pred)))
  print(paste("RMSE test :",rmse(dataPlot_test$secheresse, dataPlot_test$pred)))
  print(paste("AIC :",AIC(model)))
  print(paste("BIC :",BIC(model)))
  
  
  dataPlot <- dataPlot_test  %>% 
    group_by(annee) %>%
    summarise(obs = sum(secheresse),
              pred = sum(pred))
  
  print(plot_ly(dataPlot, colors = my_colors) %>%
          add_lines(x=~as.factor(annee), y=~obs, name = "observé", color ="1") %>%
          add_lines(x=~as.factor(annee), y=~pred, name = "prédiction", color="2") %>%
          layout(yaxis = list(rangemode='tozero', title = "Nombre de CatNat secheresse"),
                 xaxis = list(title = "Année")))
  
  
  dataPlot <-   dataPlot_train %>%
    group_by(annee) %>%
    summarise(obs = sum(secheresse),
              pred = sum(pred))
  
  plot_ly(dataPlot, colors = my_colors) %>%
    add_lines(x=~as.factor(annee), y=~obs, name = "observé", color ="1") %>%
    add_lines(x=~as.factor(annee), y=~pred, name = "prédiction", color="2") %>%
    layout(yaxis = list(rangemode='tozero', title = "Nombre de CatNat secheresse"),
           xaxis = list(title = "Année"))
}



# -----------------------------------------------------------------------------#
# AVEC LES DONNEES DU PROJET CLIMSEC -------------------------------------------
# -----------------------------------------------------------------------------#


## Chargement des donnees ------------------------------------------------------
# dataFinaleCom <- read_rds("../Data/data_modelisation_1982_2021_all.rds")
dataFinaleCom <- read_rds("../Data/modelisation/data_modelisation_1982_2021_all_V2.rds")
dataFinaleClimsec <- read_rds("../Data/modelisation/data_modelisation_1982_2021_all_V2_climsec.rds")
dataFinaleClimsec_pl <- read_rds("../Data/modelisation/data_modelisation_1982_2021_all_V2_climsec_pl.rds")



list_communes = unique(dataFinaleCom$id)
list_CN <- c("inondation", "secheresse")
list_var <- setdiff(names(dataFinaleCom), 
                    c("id","annee","longM","latM","inondation", "secheresse",
                      "commune", "indic_in", "dep"))

list_var_continuous <- setdiff(list_var, c("part_alea_faible_commune_cat","part_alea_moyen_fort_commune_cat"))

list_var2 <- c("pfl90","tav","tnav","txav","txq10","tnq10","tr","sd",
               "txq90","inondationH3",
               "rr1mm","tnnd","pq90","rr","pav","tncwd","pxcwd","txfd","tnfd","altitude")



## Preparation des donnees -----------------------------------------------------
dataFinaleClimsec_pl <- dataFinaleClimsec_pl %>%
  mutate(old_periode_sec = annee < 1999)

dataFinaleCom_trainSec <- dataFinaleClimsec_pl%>%
  filter(1990<annee&annee<2016)

dataFinaleCom_testSec <- dataFinaleClimsec_pl%>%
  filter(annee>=2016) # attention année 2021



# ---------------------------------------------------------------------------- #
## Categorisation de variables -------------------------------------------------
# ---------------------------------------------------------------------------- #
# dataFinaleCom_train$rr_cat <- cut(dataFinaleCom_train$rr, breaks=quantile(dataFinaleCom_train$rr, c(0, 0.25, 0.5, 0.75, 1)), include.lowest = TRUE)
# dataFinaleCom_test$rr_cat <- cut(dataFinaleCom_test$rr, breaks=quantile(dataFinaleCom_train$rr, c(0, 0.25, 0.5, 0.75, 1)), include.lowest = TRUE)
# 
# dataFinaleCom_train$rr_cat2 <- cut(dataFinaleCom_train$rr, breaks=quantile(dataFinaleCom_train$rr, c(0, 0.75, 1)), include.lowest = TRUE)
# dataFinaleCom_test$rr_cat2 <- cut(dataFinaleCom_test$rr, breaks=quantile(dataFinaleCom_train$rr, c(0, 0.75, 1)), include.lowest = TRUE)
# 
# dataFinaleCom_train$rr_cat3 <- cut(dataFinaleCom_train$rr, breaks=quantile(dataFinaleCom_train$rr, c(0, 0.5, 1)), include.lowest = TRUE)
# dataFinaleCom_test$rr_cat3 <- cut(dataFinaleCom_test$rr, breaks=quantile(dataFinaleCom_train$rr, c(0, 0.5, 1)), include.lowest = TRUE)
# 
# dataFinaleCom_train$pav_cat <- cut(dataFinaleCom_train$pav, breaks=quantile(dataFinaleCom_train$pav, c(0, 0.25, 0.5, 0.75, 1)), include.lowest = TRUE)
# dataFinaleCom_test$pav_cat <- cut(dataFinaleCom_test$pav, breaks=quantile(dataFinaleCom_train$pav, c(0, 0.25, 0.5, 0.75, 1)), include.lowest = TRUE)
# 
# dataFinaleCom_train$pav_cat2 <- cut(dataFinaleCom_train$pav, breaks=quantile(dataFinaleCom_train$pav, c(0, 0.25, 0.75, 1)), include.lowest = TRUE)
# dataFinaleCom_test$pav_cat2 <- cut(dataFinaleCom_test$pav, breaks=quantile(dataFinaleCom_train$pav, c(0, 0.25, 0.75, 1)), include.lowest = TRUE)
# 
# dataFinaleCom_train$pav_cat3 <- cut(dataFinaleCom_train$pav, breaks=quantile(dataFinaleCom_train$pav, c(0, 0.5, 1)), include.lowest = TRUE)
# dataFinaleCom_test$pav_cat3 <- cut(dataFinaleCom_test$pav, breaks=quantile(dataFinaleCom_train$pav, c(0, 0.5, 1)), include.lowest = TRUE)
# 
# dataFinaleCom_train$pfl90_cat <- cut(dataFinaleCom_train$pfl90, breaks=quantile(dataFinaleCom_train$pfl90, c(0, 0.25, 0.75, 1)), include.lowest = TRUE)
# dataFinaleCom_test$pfl90_cat <- cut(dataFinaleCom_test$pfl90, breaks=quantile(dataFinaleCom_train$pfl90,  c(0, 0.25, 0.75, 1)), include.lowest = TRUE)
# 



# ---------------------------------------------------------------------------- #
## GLM Poisson -----------------------------------------------------------------
# ---------------------------------------------------------------------------- #
# Modèle 1
glm_poisson <- glm(secheresse ~ 
                     altitude+
                     sd+
                     tnht+
                     tr+
                     tncwd+
                     tx35+
                     txav+
                     txnd+       
                     txhwd+
                     trav+
                     txq90+
                     txq10+
                     pfl90+
                     pxcwd+
                     pxcdd+
                     pint+
                     pq99+
                     part_alea_moyen_fort_commune+
                     sswi_B1+
                     old_periode_sec+
                     secheresseH3,
                   family=poisson("log"), data = dataFinaleCom_trainSec)
summary(glm_poisson)
eval_model_sec(glm_poisson)


glm_work <- glm(secheresse ~ 
                  tr+
                  txav+
                  txnd+       
                  txhwd+
                  txq10+
                  pav+
                  #rr+
                  pint+
                  pq90+
                  part_alea+ # part_alea_moyen_fort_commune +
                  sswi_B1+
                  old_periode_sec,
                   family=poisson("log"), data = dataFinaleCom_trainSec)
summary(glm_work)
eval_model_sec(glm_work, dataFinaleCom_trainSec, dataFinaleCom_testSec)



glm_work2 <- glm(secheresse ~ sswi_B1 + part_alea_moyen_fort_commune +
                   old_periode_sec + rr + rr1mm*pxcdd,
                  # txav+
                  #  tav+
                  # txq90+
                  # txnd+       
                  # txhwd+
                  # sd+
                  # #tx35+
                  # tnht+
                  # tr+
                  # #rr1mm+
                  # pav+
                  # pint+
                  #  pxcdd+
                  #  pxcwd+
                  # sswi_B1+
                  # part_alea_moyen_fort_commune+
                  # old_periode_sec,
                family=poisson("log"), data = dataFinaleCom_trainSec)
summary(glm_work2)
eval_model_sec(glm_work2)




# ---------------------------------------------------------------------------- #
## Step AIC Poisson -------------------------------------------------------------
# ---------------------------------------------------------------------------- #
list_var_sec <- c("txav","txq90","txnd","txhwd","sd","tx35",
                  "tnht","tr", "rr1mm", "pav", "pint", "sswi_B1",
                  "part_alea_moyen_fort_commune",
                  "secheresseH3","old_periode_sec")

df_train <- dataFinaleCom_trainSec %>% select(all_of(list_var_sec), secheresse) 

glmP = glm(secheresse ~., data = df_train, family=poisson("log"))
Pois_AIC <- stepAIC(glmP, direction = "both")






# -----------------------------------------------------------------------------#
# AVEC LES NOUVELLES DONNEES        --------------------------------------------
# -----------------------------------------------------------------------------#

dataFinaleCom_new <- read_rds("../Data/modelisation/data_modelisation_1982_2021_f.rds")
dataFinaleCom_new_pl <- read_rds("../Data/modelisation/data_modelisation_1982_2021_f_pl.rds")


## Preparation des donnees -----------------------------------------------------
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




## Modelisation pluri annuelle -------------------------------------------------
glm_work_new <- glm(secheresse ~ 
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
                      sswi_min*part_alea_moyen_fort_commune +
                      sswi_min*old_periode_sec+
                      sswi_max+
                      sswi_moy+
                      sswi_moy*sswi_min
                    ,
                    family=poisson("log"), data = dataFinaleNew_trainSec_pl)
summary(glm_work_new)
eval_model_sec(glm_work_new, dataFinaleNew_trainSec_pl,dataFinaleNew_testSec_pl)


# [1] "RMSE train : 0.198459329934321"
# [1] "RMSE test : 0.263777048844369"
# [1] "AIC : 225652.762118654"
# [1] "BIC : 225816.400731201"

# > eval_model_sec(glm_work_new, dataFinaleNew_trainSec_pl,dataFinaleNew_testSec_pl)
# [1] "RMSE train : 0.193312977206192"
# [1] "RMSE test : 0.261567677148428"
# [1] "AIC : 209611.569230012"
# [1] "BIC : 209810.273259534"

dataFinaleNew_trainSec_pl <- dataFinaleNew_trainSec_pl %>% 
  mutate(pred = predict(glm_work_new , type = "response")) %>%
  mutate(delta = abs(secheresse-pred))
sum(dataFinaleNew_trainSec_pl$delta)

glm_work_new2 <- glm(secheresse ~ 
                       txav + txq90 + txnd + txhwd + sd + 
                       tx35 + tnht + tr + rr1mm + pav + pint + part_alea_moyen_fort_commune + 
                       secheresseH3 + old_periode_sec + sswi_min + sswi_moy + sswi_max + 
                       annee
                    ,
                    family=poisson("log"), data = dataFinaleNew_trainSec_pl)
summary(glm_work_new2)
eval_model_sec(glm_work_new2, dataFinaleNew_trainSec_pl,dataFinaleNew_testSec_pl)

# Step AIC
list_var_sec <- c("txav","txq90","txnd","txhwd","sd","tx35",
                  "tnht","tr", "rr1mm", "pav", "pint", 
                  "part_alea_moyen_fort_commune",
                  "secheresseH3","old_periode_sec", "sswi_min", "sswi_moy", "sswi_max",
                  "annee")

df_train_pl <- dataFinaleNew_trainSec_pl %>% select(all_of(list_var_sec), secheresse) 

glmP = glm(secheresse ~., data = df_train_pl, family=poisson("log"))
Pois_AIC <- stepAIC(glmP, direction = "both")


## Modelisation 2---------------------------------------------------------------
glm_work2 <- glm(secheresse ~ 
                  tr+
                  txav+
                  txnd+       
                  txhwd+
                  txq10+
                  pav+
                  #rr+
                  pint+
                  pq90+
                  part_alea+ # part_alea_moyen_fort_commune +
                  sswi_max+
                  sswi_moy+
                  old_periode_sec*sswi_min,
                family=poisson("log"), data = dataFinaleNew_trainSec)
summary(glm_work2)
eval_model_sec(glm_work2, dataFinaleNew_trainSec, dataFinaleNew_testSec)


# Step AIC
list_var_sec <- c("txav","txq90","txnd","txhwd","sd","tx35",
                  "tnht","tr", "rr1mm", "pav", "pint", 
                  "part_alea_moyen_fort_commune",
                  "secheresseH3","old_periode_sec", "sswi_min", "sswi_moy", "sswi_max",
                  "annee")

df_train <- dataFinaleNew_trainSec %>% select(all_of(list_var_sec), secheresse) 

glmP = glm(secheresse ~., data = df_train, family=poisson("log"))
Pois_AIC <- stepAIC(glmP, direction = "both")

