
setwd("G:/Drive partagés/BU_ACT_FR/4 - Mémoires/Mémoires 2022/Mémoire de MAUCUER Eurydice/Code")


library(readr)
library(Metrics)
library(MASS)
library(dplyr)
library(plotly)
library(texreg)
library(ranger)

# LIste des variables 
# tav+tnav+txav+sd+tx35+txnd+            
# tnht+tr+tnfd+tnnd+txfd+tncwd+           
# txhwd+trav+txq90+txq10+tnq10+tnq90+
# pav+rr+rr1mm+pn20mm+pfl90+pxcwd+
# pxcdd+pint+pq90+pq99+altitude


# pfl90+tav+tnav+txav+txq10+tnq10+tr+sd+
# txq90+inondationH3+
# rr1mm+tnnd+pq90+rr+pav+tncwd+pxcwd+txfd+tnfd+altitude


my_colors = c("#1DE9B6", "#A6A6A6", "#707070", "#363636")

# -----------------------------------------------------------------------------#
# Chargement des donnees -------------------------------------------------------
# -----------------------------------------------------------------------------#
dataFinaleCom <- read_rds("../../Data/modelisation/data_modelisation_1982_2021_f.rds")

dataFinaleCom_pl <- dataFinaleCom %>% 
  select(-inondation, -secheresse) %>%
  rename(inondation = inondation_pl,
         secheresse = secheresse_pl)

dataFinaleCom <- dataFinaleCom %>% 
  select(-inondation_pl, -secheresse_pl) 

dataFinaleCom <- dataFinaleCom %>%
  mutate(indic_in = ifelse(inondation==0,0,1))
dataFinaleCom_pl <- dataFinaleCom_pl %>%
  mutate(indic_in = ifelse(inondation==0,0,1))

list_communes = unique(dataFinaleCom$id)
list_CN <- c("inondation", "secheresse", "indic_in")
list_var <- setdiff(names(dataFinaleCom), 
                    c("id","annee","longM","latM", "code_dep",  "commune",
                      "inondation", "secheresse", "indic_in"))

list_var2 <- c("pfl90","tav","tnav","txav","txq10","tnq10","tr","sd",
  "txq90","inondationH3",
  "rr1mm","tnnd","pq90","rr","pav","tncwd","pxcwd","txfd","tnfd","altitude")




# -----------------------------------------------------------------------------#
# Fonctions ------------------------------------------------------------------ #
# -----------------------------------------------------------------------------#
eval_model <- function(model, trainset, testset){
  dataPlot_train <- trainset %>% mutate(pred = predict(model, type = "response"))
  dataPlot_test <- testset %>% 
    mutate(pred = predict(model, type = "response", newdata=testset))
  
  print(paste("RMSE train :",rmse(dataPlot_train$inondation, dataPlot_train$pred)))
  print(paste("RMSE test :",rmse(dataPlot_test$inondation, dataPlot_test$pred)))
  print(paste("AIC :",AIC(model)))
  print(paste("BIC :",BIC(model)))
  
  dataPlot <- dataPlot_train  %>% 
    group_by(annee) %>%
    summarise(obs = sum(inondation),
              pred = sum(pred))
  View(dataPlot)
  dataPlot2 <- dataPlot_test  %>% 
    group_by(annee) %>%
    summarise(obs = sum(inondation),
              pred = sum(pred))
  View(dataPlot2)
  
  plot_ly(dataPlot, colors = my_colors) %>%
    add_lines(x=~as.factor(annee), y=~obs, name = "observé", color ="1") %>%
    add_lines(x=~as.factor(annee), y=~pred, name = "prédiction", color="2") %>%
    layout(yaxis = list(rangemode='tozero', title = "Nombre de CatNat inondation"),
           xaxis = list(title = "Année"))
  
}




create_new_var <- function(trainset, testset, train = T){
  if (train) {
    trainset$rr_cat <- cut(trainset$rr, breaks=quantile(trainset$rr, c(0, 0.25, 0.5, 0.75, 1)), include.lowest = TRUE)
    trainset$rr_cat2 <- cut(trainset$rr, breaks=quantile(trainset$rr, c(0, 0.75, 1)), include.lowest = TRUE)
    trainset$rr_cat3 <- cut(trainset$rr, breaks=quantile(trainset$rr, c(0, 0.5, 1)), include.lowest = TRUE)
    trainset$pav_cat <- cut(trainset$pav, breaks=quantile(trainset$pav, c(0, 0.25, 0.5, 0.75, 1)), include.lowest = TRUE)
    trainset$pav_cat2 <- cut(trainset$pav, breaks=quantile(trainset$pav, c(0, 0.25, 0.75, 1)), include.lowest = TRUE)
    trainset$pav_cat3 <- cut(trainset$pav, breaks=quantile(trainset$pav, c(0, 0.5, 1)), include.lowest = TRUE)
    trainset$pfl90_cat <- cut(trainset$pfl90, breaks=quantile(trainset$pfl90, c(0, 0.25, 0.75, 1)), include.lowest = TRUE)
    return(as.data.frame(trainset))
  }
  else{  
    testset$rr_cat <- cut(testset$rr, breaks=quantile(trainset$rr, c(0, 0.25, 0.5, 0.75, 1)), include.lowest = TRUE)
    testset$rr_cat2 <- cut(testset$rr, breaks=quantile(trainset$rr, c(0, 0.75, 1)), include.lowest = TRUE)
    testset$rr_cat3 <- cut(testset$rr, breaks=quantile(trainset$rr, c(0, 0.5, 1)), include.lowest = TRUE)
    testset$pav_cat <- cut(testset$pav, breaks=quantile(trainset$pav, c(0, 0.25, 0.5, 0.75, 1)), include.lowest = TRUE)
    testset$pav_cat2 <- cut(testset$pav, breaks=quantile(trainset$pav, c(0, 0.25, 0.75, 1)), include.lowest = TRUE)
    testset$pav_cat3 <- cut(testset$pav, breaks=quantile(trainset$pav, c(0, 0.5, 1)), include.lowest = TRUE)
    testset$pfl90_cat <- cut(testset$pfl90, breaks=quantile(trainset$pfl90,  c(0, 0.25, 0.75, 1)), include.lowest = TRUE)
    return(as.data.frame(testset))
  }
}



# -----------------------------------------------------------------------------#
# Preparation des donnees ------------------------------------------------------
# -----------------------------------------------------------------------------#
dataFinaleCom <- dataFinaleCom %>% rename(sswi = sswi_A2, spi = spi_A2)
dataFinaleCom_pl <- dataFinaleCom_pl %>% rename(sswi = sswi_A2, spi = spi_A2)

dataFinaleCom_train <- dataFinaleCom %>%
  filter(1984 < annee & annee < 2017 & annee != 1999) ############# -- Atention a 1999

dataFinaleCom_test <- dataFinaleCom %>%
  filter(annee >= 2017)

dataFinaleCom_train_pl <- dataFinaleCom_pl %>%
  filter(1984 < annee & annee < 2017 & annee != 1999) ############# -- Atention a 1999

dataFinaleCom_test_pl <- dataFinaleCom_pl %>%
  filter(annee >= 2017)

dataFinaleCom_train <- create_new_var(dataFinaleCom_train, dataFinaleCom_test)
dataFinaleCom_test <- create_new_var(dataFinaleCom_train, dataFinaleCom_test, train = F)

dataFinaleCom_train_pl <- create_new_var(dataFinaleCom_train_pl, dataFinaleCom_test_pl)
dataFinaleCom_test_pl <- create_new_var(dataFinaleCom_train_pl, dataFinaleCom_test_pl, F)





# ---------------------------------------------------------------------------- #
#                                   GLM 
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
##                         Final Model for paper                         -----
# ---------------------------------------------------------------------------- #

# Modèle 16
glm_poisson16 <- glm(inondation ~ txav+tr+
                       txq90+inondationH3+
                       rr1mm+rr_cat+tncwd+pxcwd+txfd+tnfd+altitude+as.factor(ppri),
                     family=poisson("log"), data = dataFinaleCom_train)
summary(glm_poisson16)
eval_model(glm_poisson16, dataFinaleCom_train, dataFinaleCom_test)


# New modèle
glm_poisson_new <- glm(inondation ~ sswi_max+sswi_min+sswi_moy+
                         txav+tr+
                         txq90+inondationH3+
                         rr1mm+rr_cat+tncwd+pxcwd+txfd+tnfd+altitude+as.factor(ppri),
                       family=poisson("log"), data = dataFinaleCom_train_new)
summary(glm_poisson_new)
eval_model(glm_poisson_new, dataFinaleCom_train_new, dataFinaleCom_test_new)



# ---------------------------------------------------------------------------- #
##                              GLM Binomial                             -----
# ---------------------------------------------------------------------------- #
binomiale <- glm(indic_in ~tav+tnav+txnd+
                   pav+rr+rr1mm+pn20mm+pxcdd+
                   pq90+altitude+
                   annee,
                 family=binomial, data = dataFinaleCom_train)
summary(binomiale)
eval_model(binomiale)

result_bin <- as.data.frame(predict(binomiale, type = "response"))
colnames(result_bin) <- c("bin")


# ---------------------------------------------------------------------------- #
##                              GLM Poisson                             -----
# ---------------------------------------------------------------------------- #

# Modèle 1
glm_poisson <- glm(inondation ~ altitude+
                     sd+tav+tnav+tnfd+tnht+tnnd+tr+tncwd+
                     tx35+txav+txfd+txnd+       
                     txhwd+trav+txq90+txq10+tnq10+tnq90+
                     pav+rr+rr1mm+pn20mm+pfl90+pxcwd+
                     pxcdd+pint+pq90+pq99,
                   family=poisson("log"), data = dataFinaleCom_train)
summary(glm_poisson)
eval_model(glm_poisson, dataFinaleCom_train, dataFinaleCom_test)


  
  
# Modèle 2
glm_poisson2 <- glm(inondation ~ tav+tnav+txav+sd+tx35+txnd+            
                     tnht+tr+tnfd+tnnd+txfd+tncwd+           
                     txhwd+trav+txq90+txq10+tnq10+tnq90+
                     pav+rr+rr1mm+pn20mm+pfl90+pxcwd+
                     pxcdd+pint+pq90+altitude,
                   family=poisson("log"), data = dataFinaleCom_train)
summary(glm_poisson2)
eval_model(glm_poisson2, dataFinaleCom_train, dataFinaleCom_test)


# Modèle 3
glm_poisson3 <- glm(inondation ~ tnav+txav+sd+tx35+txnd+            
                     tnht+tr+tnfd+tnnd+txfd+tncwd+           
                     txhwd+trav+txq90+txq10+tnq10+tnq90+
                     pav+rr+rr1mm+pn20mm+pfl90+pxcwd+
                     pxcdd+pint+pq90+altitude,
                   family=poisson("log"), data = dataFinaleCom_train)
summary(glm_poisson3)
eval_model(glm_poisson3, dataFinaleCom_train, dataFinaleCom_test)


# Modèle 4
glm_poisson4 <- glm(inondation ~ pfl90+tav+tnav+txav+txq10+tnq10+tr+sd+
                     txq90+inondationH3+
                     rr1mm+tnnd+pq90+rr+pav+tncwd+pxcwd+txfd+tnfd+altitude,
                   family=poisson("log"), data = dataFinaleCom_train)
summary(glm_poisson4)
eval_model(glm_poisson4, dataFinaleCom_train, dataFinaleCom_test)


# Modèle 5
glm_poisson5 <- glm(inondation ~ pfl90+tnav+txav+txq10+tnq10+tr+sd+
                     txq90+inondationH3+
                     rr1mm+tnnd+pq90+rr+pav+tncwd+pxcwd+txfd+tnfd+altitude,
                   family=poisson("log"), data = dataFinaleCom_train)
summary(glm_poisson5)
eval_model(glm_poisson5, dataFinaleCom_train, dataFinaleCom_test)


# Modèle 6
glm_poisson6 <- glm(inondation ~ pfl90+tnav+txav+txq10+tnq10+tr+sd+
                     txq90+inondationH3+
                     rr1mm+tnnd+pq90+rr_cat+pav+tncwd+pxcwd+txfd+tnfd+altitude,
                   family=poisson("log"), data = dataFinaleCom_train)
summary(glm_poisson6)
eval_model(glm_poisson6, dataFinaleCom_train, dataFinaleCom_test)


# Modèle 7
glm_poisson7 <- glm(inondation ~ pfl90+tnav+txav+txq10+tnq10+tr+sd+
                     txq90+inondationH3+
                     rr1mm+tnnd+pq90+rr_cat+pav_cat+tncwd+pxcwd+txfd+tnfd+altitude,
                   family=poisson("log"), data = dataFinaleCom_train)
summary(glm_poisson7)
eval_model(glm_poisson7, dataFinaleCom_train, dataFinaleCom_test)


# Modèle 8
glm_poisson8 <- glm(inondation ~ pfl90+tnav+txav+txq10+tnq10+tr+sd+
                     txq90+inondationH3+
                     rr1mm+tnnd+pq90+rr+pav_cat+tncwd+pxcwd+txfd+tnfd+altitude,
                   family=poisson("log"), data = dataFinaleCom_train)
summary(glm_poisson8)
eval_model(glm_poisson8, dataFinaleCom_train, dataFinaleCom_test)


# Modèle 9
glm_poisson9 <- glm(inondation ~ pfl90+tnav+txav+txq10+tnq10+tr+sd+
                     txq90+inondationH3+
                     rr1mm+tnnd+pq90+rr_cat+pav_cat2+tncwd+pxcwd+txfd+tnfd+altitude,
                   family=poisson("log"), data = dataFinaleCom_train)
summary(glm_poisson9)
eval_model(glm_poisson9, dataFinaleCom_train, dataFinaleCom_test)


# Modèle 10
glm_poisson10 <- glm(inondation ~ pfl90+tnav+txav+txq10+tnq10+tr+sd+
                     txq90+inondationH3+
                     rr1mm+tnnd+pq90+rr_cat+pav_cat3+tncwd+pxcwd+txfd+tnfd+altitude,
                   family=poisson("log"), data = dataFinaleCom_train)
summary(glm_poisson10)
eval_model(glm_poisson10, dataFinaleCom_train, dataFinaleCom_test)


# Modèle 11
glm_poisson11 <- glm(inondation ~ pfl90+tnav+txav+txq10+tnq10+tr+sd+
                     txq90+inondationH3+
                     rr1mm+tnnd+pq90+rr_cat2+pav_cat3+tncwd+pxcwd+txfd+tnfd+altitude,
                   family=poisson("log"), data = dataFinaleCom_train)
summary(glm_poisson11)
eval_model(glm_poisson11, dataFinaleCom_train, dataFinaleCom_test)


# Modèle 12
glm_poisson12 <- glm(inondation ~ pfl90+txav+txq10+tnq10+tr+sd+
                     txq90+inondationH3+
                     rr1mm+tnnd+pq90+rr_cat+pav_cat3+tncwd+pxcwd+txfd+tnfd+altitude,
                   family=poisson("log"), data = dataFinaleCom_train)
summary(glm_poisson12)
eval_model(glm_poisson12, dataFinaleCom_train, dataFinaleCom_test)


# Modèle 13
glm_poisson13 <- glm(inondation ~ pfl90+txav+txq10+tnq10+tr+sd+
                     txq90+inondationH3+
                     rr1mm+pq90+rr_cat+pav_cat3+tncwd+pxcwd+txfd+tnfd+altitude,
                   family=poisson("log"), data = dataFinaleCom_train)
summary(glm_poisson13)
eval_model(glm_poisson13, dataFinaleCom_train, dataFinaleCom_test)


# Modèle 14
glm_poisson14 <- glm(inondation ~ txav+txq10+tnq10+tr+sd+
                     txq90+inondationH3+
                     rr1mm+pq90+rr_cat+pav_cat3+tncwd+pxcwd+txfd+tnfd+altitude,
                   family=poisson("log"), data = dataFinaleCom_train)
summary(glm_poisson14)
eval_model(glm_poisson14, dataFinaleCom_train, dataFinaleCom_test)


# Modèle 15
glm_poisson15 <- glm(inondation ~ txav+txq10+tnq10+tr+sd+
                     txq90+inondationH3+
                     rr1mm+pq90+rr_cat+pav_cat3+tncwd+pxcwd+txfd+tnfd+altitude,
                   family=poisson("log"), data = dataFinaleCom_train)
summary(glm_poisson15)
eval_model(glm_poisson15, dataFinaleCom_train, dataFinaleCom_test)





# Modèle work
dataFinaleCom_train2 <- dataFinaleCom_new %>%
  filter(1984 < annee & annee < 2003 & annee != 1999) ############# -- Atention a 1999

dataFinaleCom_test2 <- dataFinaleCom_new %>%
  filter(annee >= 2003 & annee < 2006)

glm_poissonwork <- glm(inondation ~ sswi_max+sswi_min+pfl90 + rr1mm + pav + tnnd + as.factor(ppri),
                     family=poisson("log"), data = dataFinaleCom_train2)
summary(glm_poissonwork)
eval_model(glm_poissonwork, dataFinaleCom_train2, dataFinaleCom_test2)





## Step AIC Poisson -------------------------------------------------------------
df_train <- dataFinaleCom_train %>% 
  select(txfd,txq90,tx35,tnht,tnnd,
         rr1mm,pn20mm,inondationH3, altitude, 
         ppri, ppri_sum, inondation) 

glmP = glm(inondation ~., data = df_train, family=poisson("log"))
Pois_AIC <- stepAIC(glmP, direction = "both")

eval_model(Pois_AIC)



# ---------------------------------------------------------------------------- #
# RANDOM FOREST ---------------------------------------------------------------
# ---------------------------------------------------------------------------- #

# Loading package
library(caTools)
library(randomForest)
library(ranger)

train <- dataFinaleCom_train %>% select(list_var2[1:5], inondation, -secheresseH3)
test <- dataFinaleCom_test %>% select(list_var2[1:5], inondation, -secheresseH3)


# Fitting Random Forest to the train dataset
set.seed(120)  # Setting seed

fitrf_ino_essai=ranger(inondation~
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
                   altitude+
                   spi+
                   sswi+
                   rr1mm+
                   ppri_sum,
                 data=dataFinaleCom_train,
                 num.trees=700, 
                 importance='impurity',
                 min.node.size=16000, # A augmenter 
                 write.forest=TRUE)


fitrf_ino_new=ranger(inondation~
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
                   altitude+
                   sswi_min+
                   sswi_max+
                   sswi_moy+
                   rr1mm+
                   ppri_sum,
                 data=dataFinaleCom_train_new,
                 num.trees=700, 
                 importance='impurity',
                 min.node.size=16000, # A augmenter 
                 write.forest=TRUE)

### Save -------------------------------------------------------------------------
saveRDS(fitrf_ino,"../Modeles/fitrf_ino.rds")
saveRDS(fitrf_ino_new, "../Modeles/new_fit_rf_ino.rds")
saveRDS(fitrf_ino_essai, "../Modeles/fit_rf_ino_essai.rds")



### Evaluation -------------------------------------------------------------------
fit_plot = fitrf_ino_essai

y_pred_rd <- predict(fit_plot, data=dataFinaleCom_test)
rmse(dataFinaleCom_test$inondation, y_pred_rd$predictions)

rmse(fit_plot$predictions, dataFinaleCom_train_new$inondation)


dataPlotTrain <- dataFinaleCom_train_new %>% 
  bind_cols(fit_plot$predictions) %>%
  rename('pred'="...65") %>%
  group_by(annee) %>%
  summarise(obs = sum(inondation), pred = sum(pred))

plot_ly(dataPlotTrain,
        colors = my_colors) %>%
  add_lines(x=~as.factor(annee), y=~obs, name = "observé", color ="1") %>%
  add_lines(x=~as.factor(annee), y=~pred, name = "prédiction RF", color="2") %>%
  layout(yaxis = list(rangemode='tozero', title = "Nombre de CatNat inondation"),
         xaxis = list(title = "Année"))


dataPlotTest<- dataFinaleCom_test_new %>% 
  bind_cols(y_pred_rd$predictions) %>%
  rename('pred'="...65") %>%
  group_by(annee) %>%
  summarise(obs = sum(inondation), pred = sum(pred))


plot_ly(dataPlotTest,
        colors = my_colors) %>%
  add_lines(x=~as.factor(annee), y=~obs, name = "observé", color ="1") %>%
  add_lines(x=~as.factor(annee), y=~pred, name = "prédiction RF", color="2") %>%
  layout(yaxis = list(rangemode='tozero', title = "Nombre de CatNat inondation"),
         xaxis = list(title = "Année"))






# Confusion Matrix
confusion_mtx = table(test$inondation, y_pred)
confusion_mtx

# Plotting model
plot(classifier_RF)

# Importance plot
importance(classifier_RF)

# Variable importance plot
varImpPlot(classifier_RF)






# ---------------------------------------------------------------------------- #
# Plots ------------------------------------------------------------------------
# ---------------------------------------------------------------------------- #
dataFinaleCom_train$pred <- predict(glm_poisson, type = "response")
dataFinaleCom_test$pred <- predict(glm_poisson, type = "response", newdata=dataFinaleCom_test)

dataFinaleCom_train$pred12 <- predict(glm_poisson12, type = "response")
dataFinaleCom_test$pred12 <- predict(glm_poisson12, type = "response", newdata=dataFinaleCom_test)

dataFinaleCom_train$pred14 <- predict(glm_poisson14, type = "response")
dataFinaleCom_test$pred14 <- predict(glm_poisson14, type = "response", newdata=dataFinaleCom_test)

my_colors = c("#1DE9B6", "#A6A6A6", "#707070", "#363636")


plot_ly(dataFinaleCom_train %>% group_by(annee) %>% summarise(obs = sum(inondation),
                                                              pred12 = sum(pred12),
                                                              pred14 = sum(pred14),
                                                              pred = sum(pred)),
        colors = my_colors) %>%
  add_lines(x=~as.factor(annee), y=~obs, name = "observé", color ="1") %>%
  add_lines(x=~as.factor(annee), y=~pred12, name = "prédiction modèle 12", color="2") %>%
  add_lines(x=~as.factor(annee), y=~pred14, name = "prédiction modèle 14", color="3") %>%
  add_lines(x=~as.factor(annee), y=~pred, name = "prédiction modèle 1", color="4") %>%
  layout(yaxis = list(rangemode='tozero', title = "Nombre de CatNat inondation"))
