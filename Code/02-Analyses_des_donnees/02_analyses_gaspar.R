# --------------------------------------------- #
# Date : 12/07/2022
# Auteur : E. Maucuer
# Description : Analyse et exploitation des
# données catnat GASPAR
# --------------------------------------------- #

# Libraries
library(readr)
library(dplyr)
library(plotly)
library(tidyr)

source("../00_lib_externe.R")



# Data 
gaspar <- read_rds("../../Data/Gaspar/gaspar.rds")
gaspar_pl <- read_rds("../../Data/Gaspar/gaspar_pluri_annuel.rds")
infos_com <- read_rds("../../Data/Administratif/infos_communes.rds")
com_bound <- read_rds("../../Data/Data_GIS/geojson/communes-vs.rds")
dep_bound <- read_rds("../../Data/Data_GIS/geojson/departements-vs-om.rds") %>%
  filter(code<=95)



gaspar <- gaspar %>% filter(an_debut < 2022)
gaspar_pl <- gaspar_pl %>% filter(an_debut < 2022)

gaspar$localisation <- ifelse(gaspar$dep <= 95, "Métropole", "Outre-Mer")


# -----------------------------------------------------------------------------#
## 1) Duree d un sinistre ------------------------------------------------------
# -----------------------------------------------------------------------------#
boxplot(gaspar %>% filter(inondation==T) %>% select(duree), outline=FALSE)
boxplot(gaspar %>% filter(secheresse==T) %>% select(duree), outline=FALSE)


# -----------------------------------------------------------------------------#
## 2) Statistiques globales ----------------------------------------------------
# -----------------------------------------------------------------------------#
peril_global <- gaspar %>%
  group_by(lib_risque_jo2) %>%
  summarize(nb_perils = n()) %>%
  ungroup()



# -----------------------------------------------------------------------------#
## 3) Statistiques annuelles ---------------------------------------------------
# -----------------------------------------------------------------------------#

### Nb de communes reconnues par an  -------------------------------------------
peril_an <- gaspar %>%
  distinct(an_debut, cod_commune) %>%
  group_by(an_debut) %>%
  summarise(nb_commune = n())

plot_ly(peril_an) %>%
  add_bars(x =~ an_debut, y =~ nb_commune, width = 0.6,
           marker = list(color = my_colors3[1])) %>%
  layout(title = "", #"Nombre de communes reconnues par exercice",
         xaxis = list(tickangle = -45,title = ""),
         yaxis = list(title = ""),
         margin = list(l=50, r=50, b=50, t=50))


# version pluri annuelle 
peril_an_pl <- gaspar_pl %>%
  distinct(annee, cod_commune) %>%
  group_by(annee) %>%
  summarise(nb_commune = n())

plot_ly(peril_an_pl) %>%
  add_bars(x =~ annee, y =~ nb_commune, width = 0.6,
           marker = list(color = my_colors3[1])) %>%
  layout(title = "", #"Nombre de communes reconnues par exercice",
         xaxis = list(tickangle = -45,title = "", font.size = 20),
         yaxis = list(title = ""),
         margin = list(l=50, r=50, b=50, t=50),
         font= list(size=25))

mean(peril_an_pl$nb_commune)
summary(peril_an_pl %>% filter(annee != 1982 & annee != 1999))


### Détail de chaque risque par an ---------------------------------------------
peril_an_type <- gaspar %>% group_by(an_debut, lib_risque_jo2) %>%
  summarize(nb_perils = n()) %>%
  ungroup() 
  
plot_ly(peril_an_type, colors = "Pastel1") %>%
  add_trace(x =~ an_debut, y =~ nb_perils, color=~lib_risque_jo2, type = "bar") %>% 
  layout(barmode = 'stack') 

peril_an_type_pl <- gaspar_pl %>% group_by(an_debut, lib_risque_jo2) %>%
  summarize(nb_perils = n()) %>%
  ungroup() 

plot_ly(peril_an_type_pl, colors = "Pastel1") %>%
  add_trace(x =~ an_debut, y =~ nb_perils, color=~lib_risque_jo2, type = "bar") %>% 
  layout(barmode = 'stack') 


### Inondation vs Secheresse vs Autre par an -----------------------------------
peril_an_type_agg <- gaspar %>% 
  group_by(an_debut, lib_risque) %>%
  summarize(nb_perils = n()) %>%
  ungroup() 

plot_ly(peril_an_type_agg, colors = my_colors2) %>%
  add_trace(x =~ an_debut, y =~ nb_perils, color=~lib_risque, type = "bar") %>% 
  layout(barmode = 'stack',
         yaxis = list(title = "Nombre de reconaissances CATNAT", 
                      categoryarray = ~lib_risque, categoryorder = "array"), 
         xaxis = list(title = ""))


# Pluri annuelle
peril_an_type_agg_pl <- gaspar_pl %>% 
  group_by(annee, lib_risque) %>%
  summarize(nb_perils = n()) %>%
  mutate(order = ifelse(lib_risque == "Inondation",1,
                        ifelse(lib_risque == "Sécheresse", 2, 3))) %>%
  arrange(order) %>%
  ungroup() 

plot_ly(peril_an_type_agg_pl, colors = my_colors3) %>%
  add_trace(x =~ annee, y =~ nb_perils, color=~lib_risque, type = "bar") %>% 
  layout(barmode = 'stack',
         yaxis = list(title = "", #"Nombre de reconaissances CATNAT", 
         categoryarray = ~order, categoryorder = "array"), 
         xaxis = list(title = ""))





# -----------------------------------------------------------------------------#
## 4) Stat sur les inondations -------------------------------------------------
# -----------------------------------------------------------------------------#

### Vision annuelle ------------------------------------------------------------
inondation_an <- gaspar %>%
  filter(inondation == T) %>%
  distinct(an_debut, cod_commune) %>%
  group_by(an_debut) %>%
  summarize(nb_communes = n()) %>%
  ungroup()

plot_ly(inondation_an, colors = c("#2171B5")) %>%
  add_bars(x =~ an_debut, y =~ nb_communes, color = "#2171B5") %>% 
  layout(barmode = 'stack',
         title = "", #Nombre de communes reconnues par exercice au titre des inondations",
         xaxis = list(tickangle = -45, title = "",tickfont = list(size = 18)),
         yaxis = list(title = "",tickfont = list(size = 18)),
         margin = list(l=50, r=50, b=50, t=50)) 

mean(inondation_an$nb_communes)


# Version détaillée
inondation_an_detail <- gaspar %>%
  filter(inondation == T | num_risque_jo %in% c(67,68,73)) %>%
  distinct(an_debut, cod_commune, lib_risque_jo) %>%
  group_by(an_debut, lib_risque_jo) %>%
  summarize(nb_perils = n()) %>%
  ungroup()

plot_ly(inondation_an_detail, colors = "Blues") %>%
  add_trace(x =~ an_debut, y =~ nb_perils, color =~ lib_risque_jo, type="bar") %>% 
  layout(barmode = 'stack',
         title = "Nombre de communes reconnues par exercice au titre des inondations",
         xaxis = list(tickangle = -45, title = ""),
         yaxis = list(title = ""),
         margin = list(l=50, r=50, b=50, t=50)) 


### Decomposition par departement ----------------------------------------------
inondation_dep <- gaspar %>% 
  filter(inondation==T) %>%
  group_by(dep, ncc_dep, an_debut) %>%
  summarize(nb_inondation = sum(inondation))

plot_ly(inondation_dep, colors = "Blues") %>%
  add_bars(x =~ an_debut, y =~ nb_inondation, color=~ncc_dep, type = "bar") %>% 
  layout(barmode = 'stack') 

peril_an_dep <- gaspar %>% group_by(an_debut, dep, ncc_dep) %>%
  summarise(nb_inon = sum(inondation),
            nb_sech = sum(secheresse)) %>%
  ungroup()

plot_ly(peril_an_dep, colors = "Paired") %>%
  add_lines(x =~ an_debut, y =~ nb_inon, color=~ncc_dep) 

### Vision aggrégée ------------------------------------------------------------
nb_perils_com <- gaspar %>%
  group_by(lib_commune, cod_commune) %>%
  summarise(inondation = sum(inondation),
            secheresse = sum(secheresse, na.rm = T),
            catnat = n()) 

effectifs <- nb_perils_com %>% 
  merge(com_bound %>% select(code), 
        by.x = "cod_commune", by.y = "code", all = T) %>%
  mutate(inondation = replace_na(inondation, 0)) %>%
  group_by(inondation) %>%
  summarise(effectifs = n()) 
  



# -----------------------------------------------------------------------------#
## 5) Stat sur les secheresses -------------------------------------------------
# -----------------------------------------------------------------------------#

### Vision annuelle ------------------------------------------------------------
sec_an <- gaspar %>%
  filter(secheresse == T) %>%
  distinct(an_debut, cod_commune) %>%
  group_by(an_debut) %>%
  summarize(nb_commune = n()) %>%
  ungroup()

plot_ly(sec_an, colors = c("firebrick3")) %>%
  add_bars(x =~ as.factor(an_debut), y =~ nb_commune, color = "firebrick3") %>% 
  layout(barmode = 'stack',
         #title = "Nombre de communes reconnues par exercice \nau titre de la sécheresse",
         xaxis = list(tickangle = -45, title = ""),
         yaxis = list(title = ""),
         margin = list(l=50, r=50, b=50, t=50)) 

sec_an_pl <- gaspar_pl %>%
  filter(secheresse == T) %>%
  distinct(annee, cod_commune) %>%
  group_by(annee) %>%
  summarize(nb_commune = n()) %>%
  ungroup()

plot_ly(sec_an_pl, colors = c("#CB181D")) %>%
  add_bars(x =~ annee, y =~ nb_commune, color = "#CB181D") %>% 
  layout(barmode = 'stack',
         xaxis = list(tickangle = -45, title = "",tickfont = list(size = 18)),
         yaxis = list(title = "",tickfont = list(size = 18)),
         margin = list(l=50, r=50, b=50, t=50)) 



mean(sec_an$nb_commune)
mean(sec_an_pl$nb_commune)
summary(sec_an_pl %>% filter(annee >= 1989 & annee <= 1998))


### Vision aggrégée ------------------------------------------------------------
# Par commune
nb_perils_com <- gaspar %>%
  group_by(lib_commune, cod_commune) %>%
  summarise(inondation = sum(inondation),
            secheresse = sum(secheresse, na.rm = T),
            catnat = n()) 

effectifs_sec <- nb_perils_com %>% 
  merge(com_bound %>% select(code), 
        by.x = "cod_commune", by.y = "code", all = T) %>%
  mutate(secheresse = replace_na(secheresse, 0)) %>%
  group_by(secheresse) %>%
  summarise(effectifs = n()) 


# Par dep
nb_perils_dep <- gaspar %>%
  group_by(dep) %>%
  summarise(inondation = sum(inondation),
            secheresse = sum(secheresse, na.rm = T),
            catnat = n()) 




# -----------------------------------------------------------------------------#
# 6) Quelques test de merge ----------------------------------------------------
# -----------------------------------------------------------------------------#
test <- merge(gaspar, infos_com%>%mutate(infos=1), by.x="cod_commune", by.y="code_insee", all.x=T)
table(test %>% filter(is.na(infos)) %>% select(lib_commune))


test2 <- merge(gaspar, com_bound%>%mutate(bound=1), by.x="cod_commune", by.y="code", all.x=T)
table(test2 %>% filter(is.na(bound)) %>% select(lib_commune))



# -----------------------------------------------------------------------------#
# 7) Bases maille geographiques x annees ---------------------------------------
# -----------------------------------------------------------------------------#
an_min = min(gaspar$an_debut)
an_max = max(gaspar$an_fin)
delta_an = an_max-an_min+1

# Tous les dep x tous les ans 
dep_an <- insee_dep %>% select("dep")
dep_an <- dep_an[rep(c(1:nrow(dep_an)), delta_an),]
dep_an <- dep_an %>% 
  arrange(dep) %>% 
  mutate(an = rep(an_min:an_max, length(unique(dep_an$dep))))

# Toutes les communes x tous les ans
com_an <- infos_com %>% select("code_insee", "altitude_moyenne", "superficie", "population")
com_an <- com_an[rep(c(1:nrow(com_an)), delta_an),]
com_an <- com_an %>% 
  arrange(code_insee) %>% 
  mutate(an = rep(an_min:an_max, length(com_an$code_insee)/(delta_an)))





# -----------------------------------------------------------------------------#
# 8) Cartes sur l'ensemble de la periode par communes --------------------------
# -----------------------------------------------------------------------------#
nb_perils_com <- gaspar %>%
  group_by(lib_commune, cod_commune) %>%
  summarise(inondation = sum(inondation),
            secheresse = sum(secheresse, na.rm = T),
            catnat = n()) 

dataPlot <- nb_perils_com %>%
  merge(com_bound, by.x="cod_commune", by.y="code", all.y = T) %>%
  filter(cod_commune < 96000) %>%  
  mutate(inondation = ifelse(is.na(inondation), 0, inondation),
         secheresse = ifelse(is.na(secheresse), 0, secheresse),
         catnat = ifelse(is.na(catnat), 0, catnat)) %>%
  mutate(inondation_cat = case_when(inondation == 0 ~ 0,
                                    inondation %in% c(1,2) ~ 1,
                                    inondation %in% c(3,4,5) ~ 2,
                                    inondation %in% c(6,7,8,9) ~ 3,
                                    inondation > 9 & inondation <= 20 ~ 4,
                                    inondation > 20   ~ 5),
         secheresse_cat = case_when(secheresse == 0 ~ 0,
                                    secheresse == 1 ~ 1,
                                    secheresse %in% c(2,3,4) ~ 2,
                                    secheresse %in% c(5,6) ~ 3,
                                    secheresse %in% c(7,8,9,10) ~ 4,
                                    secheresse > 10 ~ 5),
         catnat_cat = case_when(catnat == 0 ~ 0,
                                catnat %in% c(1,2,3) ~ 1,
                                catnat %in% c(4,5,6) ~ 2,
                                catnat %in% c(7,8,9,10) ~ 3,
                                catnat > 10 ~ 4))

lab = c("0", "1-2", "3-5", "6-9", "10-20","> 20")
lab_sec = c("0", "1", "2-4", "5-6", "7-10", "> 10")
lab_cat = c("0", "1-3", "4-6", "7-10", "> 10")
dataPlot$inondation_cat <- ordered(dataPlot$inondation_cat,
                                   levels = c(0,1,2,3,4,5),
                                   labels = lab)
dataPlot$secheresse_cat <- ordered(dataPlot$secheresse_cat,
                                   levels = c(0,1,2,3,4,5),
                                   labels = lab_sec)
dataPlot$catnat_cat <- ordered(dataPlot$catnat_cat,
                                   levels = c(0,1,2,3,4),
                                   labels = lab_cat)




### All cat --------------------------------------------------------------------
colors_grey <- c("white", "#D0FBF0","#A4F7E2","#1DE9B6","#11B38A")
plot_carto_manual(dataPlot, "catnat_cat", colors_grey, title="Nombre de reconnaissances par commune")


### Inondation cat -------------------------------------------------------------
plot_carto_manual(dataPlot, "inondation_cat", colors_blue6, title="Nombre de reconnaissances par commune")


### Inondation -----------------------------------------------------------------
plot_carto_gradient(dataPlot, "inondation", color_high="#084594", title="Nombre de CATNAT inondation depuis le début du régime")


### Secheresse cat -------------------------------------------------------------
plot_carto_manual(dataPlot, "secheresse_cat", colors_red6, title="Nombre de reconnaissances par commune")


### Secheresse -----------------------------------------------------------------
plot_carto_gradient(dataPlot, "secheresse", color_high="#99000D", title="Nombre de CATNAT sécheresse depuis le début du régime")

