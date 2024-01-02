library(plotly)
library(dplyr)
library(readr)

setwd("G:/Drive partagés/BU_ACT_FR/4 - Mémoires/Mémoires 2022/Mémoire de MAUCUER Eurydice/Code")
dataFinaleCom <- read_rds("../Data/modelisation/data_modelisation_1982_2021_all.rds")


dataFinaleCom <- dataFinaleCom %>%
  mutate(indic_in = ifelse(inondation==0,0,1))

list_communes = unique(dataFinaleCom$id)
list_CN <- c("inondation", "secheresse", "indic_in")
list_var <- setdiff(names(dataFinaleCom), 
                    c("id","annee","longM","latM","inondation", "secheresse",
                      "commune", "indic_in", "code_dep"))

list_var2 <- c("pfl90","tav","tnav","txav","txq10","tnq10","tr","sd",
               "txq90","inondationH3",
               "rr1mm","tnnd","pq90","rr","pav","tncwd","pxcwd","txfd","tnfd","altitude")

# Preparation des donnees ------------------------------------------------------
dataFinaleCom_train <- dataFinaleCom %>%
  filter(1984 < annee & annee < 2017 & annee!=1999)

dataFinaleCom_test <- dataFinaleCom %>%
  filter(annee >= 2017)

dataFinaleCom_trainSec <- dataFinaleCom %>%
  filter(1987 < annee & annee < 2017)

dataFinaleCom_testSec <- dataFinaleCom %>%
  filter(annee >= 2017)


# MODELISATION INONDATION ------------------------------------------------------
dataFinaleCom_train$rrm <- dataFinaleCom_train$rr / dataFinaleCom_train$rr1mm
dataFinaleCom_test$rrm <- dataFinaleCom_test$rr / dataFinaleCom_test$rr1mm

dataFinaleCom_train$rr_cat <- cut(dataFinaleCom_train$rr, breaks=quantile(dataFinaleCom_train$rr, c(0, 0.25, 0.5, 0.75, 1)), include.lowest = TRUE)
dataFinaleCom_test$rr_cat <- cut(dataFinaleCom_test$rr, breaks=quantile(dataFinaleCom_train$rr, c(0, 0.25, 0.5, 0.75, 1)), include.lowest = TRUE)

dataFinaleCom_train$rr_cat2 <- cut(dataFinaleCom_train$rr, breaks=quantile(dataFinaleCom_train$rr, c(0, 0.75, 1)), include.lowest = TRUE)
dataFinaleCom_test$rr_cat2 <- cut(dataFinaleCom_test$rr, breaks=quantile(dataFinaleCom_train$rr, c(0, 0.75, 1)), include.lowest = TRUE)

dataFinaleCom_train$rr_cat3 <- cut(dataFinaleCom_train$rr, breaks=quantile(dataFinaleCom_train$rr, c(0, 0.5, 1)), include.lowest = TRUE)
dataFinaleCom_test$rr_cat3 <- cut(dataFinaleCom_test$rr, breaks=quantile(dataFinaleCom_train$rr, c(0, 0.5, 1)), include.lowest = TRUE)

dataFinaleCom_train$pav_cat <- cut(dataFinaleCom_train$pav, breaks=quantile(dataFinaleCom_train$pav, c(0, 0.25, 0.5, 0.75, 1)), include.lowest = TRUE)
dataFinaleCom_test$pav_cat <- cut(dataFinaleCom_test$pav, breaks=quantile(dataFinaleCom_train$pav, c(0, 0.25, 0.5, 0.75, 1)), include.lowest = TRUE)

dataFinaleCom_train$pav_cat2 <- cut(dataFinaleCom_train$pav, breaks=quantile(dataFinaleCom_train$pav, c(0, 0.25, 0.75, 1)), include.lowest = TRUE)
dataFinaleCom_test$pav_cat2 <- cut(dataFinaleCom_test$pav, breaks=quantile(dataFinaleCom_train$pav, c(0, 0.25, 0.75, 1)), include.lowest = TRUE)

dataFinaleCom_train$pav_cat3 <- cut(dataFinaleCom_train$pav, breaks=quantile(dataFinaleCom_train$pav, c(0, 0.5, 1)), include.lowest = TRUE)
dataFinaleCom_test$pav_cat3 <- cut(dataFinaleCom_test$pav, breaks=quantile(dataFinaleCom_train$pav, c(0, 0.5, 1)), include.lowest = TRUE)

dataFinaleCom_train$pfl90_cat <- cut(dataFinaleCom_train$pfl90, breaks=quantile(dataFinaleCom_train$pfl90, c(0, 0.25, 0.75, 1)), include.lowest = TRUE)
dataFinaleCom_test$pfl90_cat <- cut(dataFinaleCom_test$pfl90, breaks=quantile(dataFinaleCom_train$pfl90,  c(0, 0.25, 0.75, 1)), include.lowest = TRUE)



# GLM --------------------------------------------------------------------------
# Modèle 1
glm_poisson <- glm(inondation ~ tav+tnav+txav+sd+tx35+txnd+            
                     tnht+tr+tnfd+tnnd+txfd+tncwd+           
                     txhwd+trav+txq90+txq10+tnq10+tnq90+
                     pav+rr+rr1mm+pn20mm+pfl90+pxcwd+
                     pxcdd+pint+pq90+pq99+altitude,
                   family=poisson("log"), data = dataFinaleCom_train)

# Modèle 12
glm_poisson12 <- glm(inondation ~ pfl90+txav+txq10+tnq10+tr+sd+
                     txq90+inondationH3+
                     rr1mm+tnnd+pq90+rr_cat+pav_cat3+tncwd+pxcwd+txfd+tnfd+altitude,
                   family=poisson("log"), data = dataFinaleCom_train)

glm_poisson14 <- glm(inondation ~ txav+txq10+tnq10+tr+sd+
                     txq90+inondationH3+
                     rr1mm+pq90+rr_cat+pav_cat3+tncwd+pxcwd+txfd+tnfd+altitude,
                   family=poisson("log"), data = dataFinaleCom_train)



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


plot_ly(dataFinaleCom_test %>% group_by(annee) %>% summarise(obs = sum(inondation),
                                                              pred12 = sum(pred12),
                                                              pred14 = sum(pred14)),
        colors = my_colors) %>%
  add_lines(x=~as.factor(annee), y=~obs, name = "observé", color ="1") %>%
  add_lines(x=~as.factor(annee), y=~pred12, name = "prédiction modèle 12", color ="2") %>%
  add_lines(x=~as.factor(annee), y=~pred14, name = "prédiction modèle 14", color ="3") %>%
  layout(yaxis = list(rangemode='tozero', title = "Nombre de CatNat inondation"))


sum(dataFinaleCom_test$inondation)
sum(dataFinaleCom_test$pred12)
sum(dataFinaleCom_test$pred14)


to_exp1999 <- rbind(dataFinaleCom_train %>% group_by(annee) %>% summarise(obs = sum(inondation),pred12 = sum(pred12),pred14 = sum(pred14),pred = sum(pred)), 
                dataFinaleCom_test %>% group_by(annee) %>% summarise(obs = sum(inondation),pred12 = sum(pred12),pred14 = sum(pred14),pred = sum(pred)))
write.csv(to_exp1999, "obsVSpred_1999ex.csv")



"tav","tnav","txav","sd","tx35","txnd","            
  tnht","tr","tnfd","tnnd","txfd","tncwd","           
  txhwd","trav","txq90","txq10","tnq10","tnq90","
  pav","rr","rr1mm","pn20mm","pfl90","pxcwd","
  pxcdd","pint","pq90","pq99","altitude"
