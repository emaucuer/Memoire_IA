library(dplyr)
library(readr)
library(tidyr)


setwd("G:/Drive partagés/BU_ACT_FR/4 - Mémoires/Mémoires 2022/Mémoire de MAUCUER Eurydice/Code")
dataFinaleDep <- read_rds("../Data/modelisation/data_modelisation_dep_1982_2021.rds")


# Liste des variables 
# tav+tnav+txav+sd+tx35+txnd+            
# tnht+tr+tnfd+tnnd+txfd+tncwd+           
# txhwd+trav+txq90+txq10+tnq10+tnq90+
# pav+rr+rr1mm+pn20mm+pfl90+pxcwd+
# pxcdd+pint+pq90+pq99+altitude


# Correlation 

# Preparation des donnees
dataFinaleDep_train <- dataFinaleDep %>%
  filter(annee < 2017)

dataFinaleDep_test <- dataFinaleDep %>%
  filter(annee >= 2017)

# densite
d <- density(dataFinaleDep_train$inondation) # returns the density data
plot(d, xlim = c(0,150))

# LM --------------------------------------------
lm <- lm(inondation ~tav+txav+txnd+
                  pav+rr1mm+pn20mm+pxcdd+
                  pq90+ nb_commune+
                  annee,
                data = dataFinaleDep_train)
summary(lm)

result_lm <- as.data.frame(predict(lm, type = "response"))
colnames(result_lm) <- c("lm")

# GLM ----------------------------------------------------
quasipoisson <- glm(inondation ~tav+txav+txnd+
           pav+rr+rr1mm+pn20mm+pxcdd+
           pq90+
           nb_commune+
           annee,
         family=quasipoisson, data = dataFinaleDep_train)
summary(quasipoisson)

result_quasi <- as.data.frame(predict(quasipoisson, type = "response"))
colnames(result_quasi) <- c("quasi")



poisson <- glm(inondation ~tav+tnav+txnd+
                  pav+rr+rr1mm+pn20mm+pxcdd+
                  pq90+
                  nb_commune+altitude_moyenne+
                  annee,
                family=poisson("log"), data = dataFinaleDep_train)
summary(poisson)

result_poisson <- as.data.frame(predict(poisson, type = "response"))
colnames(result_poisson) <- c("poisson")


res <- dataFinaleDep_train %>%
  bind_cols(result_quasi) %>%
  bind_cols(result_lm)%>%
  bind_cols(result_poisson)

res$ecart = res$inondation - res$gaussian

