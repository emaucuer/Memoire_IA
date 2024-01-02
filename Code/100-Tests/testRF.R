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
library(data.table)
library(corrplot)
library(Metrics)

# DATA ------------------------------------------------------
## Import data ----------------------------------------------
dataFinaleCom <- read_rds("../Data/data_modelisation_1982_2021_all_V2_A2.rds")

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

## Split train / test ---------------------------------------
dataFinaleCom_train <- dataFinaleCom %>%
  filter(1984 < annee & annee < 2017 ) #& annee != 1999) ############# -- Atention a 1999

dataFinaleCom_test <- dataFinaleCom %>%
  filter(annee >= 2017)


train <- dataFinaleCom_train %>% select(list_var2[1:5], inondation, -secheresseH3)
test <- dataFinaleCom_test %>% select(list_var2[1:5], inondation, -secheresseH3)




# Modelisation -------------------------------------------------
## ARBRE -------------------------------------------------------
arbre <- rpart(as.factor(inondation) ~ altitude +   
                 inondationH3+pav+pfl90+
                 pint+pn20mm+pq90+
                 pq99+pxcdd+pxcwd+
                 rr+rr1mm+sd+
                 tav+tnav+tncwd+
                 tnfd+tnht+tnnd+
                 tnq10+tnq90+tr+
                 trav+tx35+txav+
                 txfd+txhwd+txnd+
                 txq10+txq90, 
        data = dataFinaleCom_train)

rpart.plot(arbre)
y_pred <- predict(arbre, data=dataFinaleCom_test)
rmse(arbre$inondation, y_pred)



## RANDOM FOREST ----------------------------------------------
trainShort <- dataFinaleCom_train %>%
  head(200000)

start = Sys.time()
fitrf_bis=ranger(inondation ~ altitude +   
               inondationH3+pav+pfl90+
               pint+pn20mm+pq90+
               pq99+pxcdd+pxcwd+
               rr+rr1mm+sd+
               tav+tnav+tncwd+
               tnfd+tnht+tnnd+
               tnq10+tnq90+tr+
               trav+tx35+txav+
               txfd+txhwd+txnd+
               txq10+txq90,
             data=trainShort,
             num.trees=500, 
             importance='impurity', 
             min.node.size=2000,
             write.forest=TRUE)
Sys.time() - start



saveRDS(fitrf, "fitrf.rds")
y_pred_rd <- predict(fitrf, data=dataFinaleCom_test)
rmse(dataFinaleCom_test$inondation, y_pred_rd$predictions)

plot(dataFinaleCom_test$inondation ~ y_pred_rd$predictions , 
     asp=1, pch=20, xlab="fitted", ylab="actual", xlim = c(0,1))

vip(fitrf)



altitude +   
inondationH3+
pav         +
pfl90       +
pint        +
pn20mm      +
pq90        +
pq99        +
pxcdd       +
pxcwd       +
rr          +
rr1mm       +
sd          +
tav         +
tnav        +
tncwd       +
tnfd        +
tnht        +
tnnd        +
tnq10       +
tnq90       +
tr          +
trav        +
tx35        +
txav        +
txfd        +
txhwd       +
txnd        +
txq10       +
txq90       +
  
  
  %>% select(inondation, 
             altitude ,   
             inondationH3,
             pav,
             pfl90,
             pint,
             pn20mm,
             pq90,
             pq99,
             pxcdd,
             pxcwd,
             rr,
             rr1mm,
             sd,
             tav,
             tnav,
             tncwd,
             tnfd,
             tnht,
             tnnd,
             tnq10,
             tnq90,
             tr,
             trav,
             tx35,
             txav,
             txfd,
             txhwd,
             txnd,
             txq10,
             txq90)  %>% 
