# --------------------------------------------- #
# Date : 05/07/2022
# Auteur : E. Maucuer
# Description : Première découverte des données
# d'arrétés de CATNAT FR metropolitaine
# --------------------------------------------- #

# Libraries
library(readr)
library(dplyr)
library(readxl)
library(stringi)
library(lubridate)
library(plotly)

# Import Data

data <- read_excel("../Data/Arretes_de_catastrophe_naturelles.xlsx")
insee_com2015 <- read_delim("../Data/comsimp2015.txt")
insee_com <- read_csv("../Data/cog_ensemble_2021_csv/commune2021.csv")
insee_dep <- read_csv("../Data/cog_ensemble_2021_csv/departement2021.csv")
insee_reg <- read_csv( "../Data/cog_ensemble_2021_csv/region2021.csv")



# Traitement des noms de variables -----
names(data) <- names(data) %>%
  stri_trans_tolower() %>%
  stri_trans_general(id = "Latin-ASCII") 
names(data) <- gsub(" ", "_", names(data)) 

names(insee_com2015) <- names(insee_com2015) %>% stri_trans_tolower() 
names(insee_com) <- names(insee_com) %>% stri_trans_tolower() 
names(insee_dep) <- names(insee_dep) %>% stri_trans_tolower() 
names(insee_reg) <- names(insee_reg) %>% stri_trans_tolower() 



# Variables géographiques -----
# Données insee (CATNAT sur la metropole uniquement)
insee_com2015 <- insee_com2015 %>%  
  mutate(com = paste0(insee_com2015$dep, insee_com2015$com)) %>% 
  filter(nchar(dep)<=2) %>%
  select(com, ncc)
insee_com <- insee_com %>% filter(nchar(dep)<=2) 
insee_dep <- insee_dep %>% 
  filter(nchar(dep)<=2) %>% select(dep, reg, ncc)
insee_reg <- insee_reg %>% filter(! reg %in% c("01","02","03","04","05","06")) %>% 
  select(reg, ncc)



# Traitement code insee -----
data <- data %>% 
  mutate(insee = sub("\\.(.+?)$", "", insee)) %>%
  mutate(insee = ifelse(nchar(insee)==4, paste0("0", insee), insee),
         departement = ifelse(nchar(departement)==1, paste0("0", departement), departement)) %>% 
  left_join(insee_dep, by=c("departement"="dep")) %>%
  left_join(insee_reg, by="reg", suffix=c("_dep", "_reg"))



# Variables temporelles -----
data <- data %>%
  mutate(date_fin = ymd(date_fin),
         date_debut = ymd(date_debut),
         an_debut = as.integer(year(date_debut)),
         an_fin = as.integer(year(date_fin)),
         duree = date_fin-date_debut +1)
an_min = min(data$an_debut)
an_max = max(data$an_fin)

for (an in (an_min:(an_max+1))){
  varname = paste0("y",an)
  data[[varname]] <- with(data, an >= an_debut & an <= an_fin)
}

# Tous les dep x tous les ans 
dep_an <- insee_dep %>% select("dep", "ncc")
dep_an <- dep_an[rep(c(1:nrow(dep_an)), 2015-1982+1),]
dep_an <- dep_an %>% 
  arrange(dep) %>% 
  mutate(an = rep(1982:2015, length(unique(dep_an$dep))))
# Toutes les reg x tous les ans
reg_an <- insee_reg %>% select("reg", "ncc")
reg_an <- reg_an[rep(c(1:nrow(reg_an)), 2015-1982+1),]
reg_an <- reg_an %>% 
  arrange(reg) %>% 
  mutate(an = rep(1982:2015, length(unique(reg_an$reg))))
# Toutes les communes x tous les ans
com_an <- insee_com2015 %>% select("com", "ncc")
com_an <- com_an[rep(c(1:nrow(com_an)), 2015-1982+1),]
com_an <- com_an %>% 
  arrange(com) %>% 
  mutate(an = rep(1982:2015, length(com_an$com)/(2015-1982+1)))



# Variable perils -----
table(data$perils)
data$inondation <- stri_detect_regex(data$perils, "inondation", case_insensitive = T)
data$secheresse <- stri_detect_regex(data$perils, "sécheresse", case_insensitive = T)



# Analyse des doublons -----
data <- data %>% mutate(dup = duplicated(data))
table(data$dup)
data <- data %>% select(-dup) %>% distinct()

# Meme date de debut pour une meme commune et un meme peril
data <- data %>% group_by(insee, date_debut, perils) %>% mutate(dup = n()) %>% ungroup()
table(data$dup)
data <- data %>% 
  arrange(insee, date_debut, desc(duree)) %>% 
  distinct(insee, date_debut, perils, .keep_all = T) %>%
  select(-dup)

# Meme date de fin pour une meme commune et un meme peril
data <- data %>% group_by(insee, date_fin, perils) %>% mutate(dup2 = n()) %>% ungroup()
table(data$dup2)
data <- data %>% 
  arrange(insee, date_debut, desc(duree)) %>% 
  distinct(insee, date_debut, perils, .keep_all = T) %>%
  select(-dup2)






# Analyse par region et par an -----
data_reg_an <- data %>% 
  select(-c(insee, commune, departement, grep("_dep", names(data)))) %>%
  distinct() %>%
  group_by(reg, ncc_reg, an_debut) %>%
  summarise(nb_inondation = sum(inondation),
            nb_secheresse = sum(secheresse)) %>%
  merge(reg_an, by.x=c("an_debut","reg","ncc_reg"), by.y=c("an","reg","ncc"), all = T) %>%
  replace(is.na(.), 0)

reg_inondation <- plot_ly(data_reg_an, colors="Blues") %>%
  add_lines(x =~ an_debut, y =~ nb_inondation, color=~ncc_reg) 
reg_inondation

reg_secheresse <- plot_ly(data_reg_an, colors="Reds") %>%
  add_lines(x =~ an_debut, y =~ nb_secheresse, color=~ncc_reg) 
reg_secheresse


# Analyse par departement et par an -----
data_dep_an <- data %>% 
  select(-c(insee, commune)) %>%
  distinct() %>%
  mutate(x = 1) %>%
  group_by(departement, an_debut, ncc_dep, x) %>%
  summarise(nb_inondation = sum(inondation),
            nb_secheresse = sum(secheresse)) %>%
  merge(dep_an %>% mutate(y=1), by.x=c("an_debut","departement","ncc_dep"), by.y=c("an","dep","ncc"), all = T) %>%
  replace(is.na(.), 0)


dep_inondation <- plot_ly(data_dep_an) %>%
  add_lines(x =~ an_debut, y =~ nb_inondation, color=~departement) 
dep_inondation


# Analyse par commune et par an -----
data_com_an <- data %>% 
  distinct() %>%
  group_by(insee, an_debut) %>%
  summarise(nb_inondation = sum(inondation),
            nb_secheresse = sum(secheresse)) %>%
  merge(com_an %>% mutate(y=1), by.x=c("an_debut","insee"), by.y=c("an","com"), all = T) %>%
  replace(is.na(.), 0)



# Save data -----
write_rds(data_reg_an, "../Data/data_reg_an.rds")
write_rds(data_dep_an, "../Data/data_dep_an.rds")
write_rds(data_com_an, "../Data/data_com_an.rds")





plot_ly(data %>% group_by(libelle_dep, an_debut) %>% summarise(nb_inondation = sum(inondation))) %>%
  add_trace(x =~ an_debut, 
            y =~ nb_inondation, 
            color=~as.factor(libelle_dep), 
            type = 'bar', 
            colors="Pastel2") %>%
  layout(barmode = 'stack')

plot_ly(data %>% group_by(libelle_reg, an_debut) %>% summarise(nb_inondation = sum(inondation))) %>%
  add_trace(x =~ an_debut, 
            y =~ nb_inondation, 
            color=~as.factor(libelle_reg), 
            type = 'bar', 
            colors="Pastel2") %>%
  layout(barmode = 'stack')


plot_ly(data %>% group_by(libelle_dep, an_debut) %>% summarise(nb_secheresse = sum(secheresse))) %>%
  add_trace(x =~ an_debut, 
            y =~ nb_secheresse, 
            color=~as.factor(libelle_dep), 
            type = 'bar', 
            colors="Pastel2") %>%
  layout(barmode = 'stack')

plot_ly(data %>% group_by(libelle_reg, an_debut) %>% summarise(nb_secheresse = sum(secheresse))) %>%
  add_trace(x =~ an_debut, 
            y =~ nb_secheresse, 
            color=~as.factor(libelle_reg), 
            type = 'bar', 
            colors="Pastel2") %>%
  layout(barmode = 'stack')

