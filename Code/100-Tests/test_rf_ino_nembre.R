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

#DATA------------------------------------------------------
##Import data----------------------------------------------
dataFinaleCom <- read_rds("../Data/modelisation/data_modelisation_1982_2021_all_V2_A2_pl.rds")



list_communes = unique(dataFinaleCom$id)
list_CN <- c("inondation","secheresse")
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



##Split train/test---------------------------------------
dataFinaleCom_trainSec <- dataFinaleCom%>%
  filter(1990<annee&annee<2016)

dataFinaleCom_testSec <- dataFinaleCom%>%
  filter(annee>=2016)

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
#saveRDS(fitrf_sec,"fitrf_sec_1102.rds")



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
               rr1mm,
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
