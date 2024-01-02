library(readr)
library(dplyr)
library(stringi)
library(lubridate)
library(plotly)
library(tidyr)

# Data -----------------------------------------------------------------
data_reg_an_dedup <- readRDS("../Data/CATNAT/dataexplorer/data_reg_an_dedup.rds")
data_reg_an <- readRDS("../Data/data_reg_an.rds")

data_dep_an_dedup <- readRDS("../Data/CATNAT/dataexplorer/data_dep_an_dedup.rds")
data_dep_an <- readRDS("../Data/CATNAT/dataexplorer/data_dep_an.rds")

data_com_an <- readRDS("../Data/CATNAT/dataexplorer/data_com_an.rds")

data <- readRDS("../Data/CATNAT/dataexplorer/arrete_catnat.rds")


# Analyses  -----------------------------------------------------------------
## Analyse annuelle -----
data_an <- data %>%
  group_by(annee) %>%
  summarise(nb_inondation = sum(inondation, na.rm=T),
            nb_secheresse = sum(secheresse, na.rm=T)) 

plot_ly(data_an, colors = c("steelblue2", "firebrick1")) %>%
  add_lines(x =~ annee, y =~ nb_inondation, color = "1", name="Inondation") %>%
  add_lines(x =~ annee, y =~ nb_secheresse, color = "2", name = "Secheresse") %>%
  layout(yaxis = list(title = "Nb de reconaissance CATNAT"),
         title = "Nombre de reconnaissances CATNAT par an")



## Analyse par region et par an -----
plot_ly(data_reg_an_dedup, colors="Blues") %>%
  add_lines(x =~ annee, y =~ nb_inondation, color=~ncc_reg) 

plot_ly(data_reg_an_dedup, colors="Reds") %>%
  add_lines(x =~ annee, y =~ nb_secheresse, color=~ncc_reg) 

plot_ly(data_reg_an_dedup) %>%
  add_trace(x =~ annee, y =~ nb_inondation, 
            type = 'bar', color =~ ncc_reg,
            colors = "Blues") %>% 
  layout(barmode = 'stack',
         title = "Inondations par an et par région (déduplication faite)")

plot_ly(data_reg_an_dedup) %>%
  add_trace(x =~ annee, y =~ nb_secheresse, 
            type = 'bar', color =~ ncc_reg,
            colors = "Reds") %>% 
  layout(barmode = 'stack',
         title = "Sécheresses par an et par région (déduplication faite)")




plot_ly(data_reg_an, colors="Blues") %>%
  add_lines(x =~ annee, y =~ nb_inondation, color=~ncc_reg) 

plot_ly(data_reg_an, colors="Reds") %>%
  add_lines(x =~ annee, y =~ nb_secheresse, color=~ncc_reg) 

plot_ly(data_reg_an) %>%
  add_trace(x =~ annee, y =~ nb_inondation, 
            type = 'bar', color =~ ncc_reg,
            colors = "Blues") %>% 
  layout(barmode = 'stack',
         title = "Inondations par an et par région")

plot_ly(data_reg_an) %>%
  add_trace(x =~ annee, y =~ nb_secheresse, 
            type = 'bar', color =~ ncc_reg,
            colors = "Reds") %>% 
  layout(barmode = 'stack',
         title = "Sécheresses par an et par région")




## Analyse par departement et par an -----
plot_ly(data_dep_an_dedup, colors = "Blues") %>%
  add_lines(x =~ annee, y =~ nb_inondation, color=~ncc_dep) 

plot_ly(data_dep_an, colors = "Blues") %>%
  add_lines(x =~ annee, y =~ nb_inondation, color=~ncc_dep) 




## Analyse temporelle -----
data_catnat <- data %>% filter(inondation==T | secheresse==T) %>%
  mutate(catnat = ifelse(inondation==T, "inondation", 
                  ifelse(secheresse==T, "secheresse", NA))) 

plot_ly(data_catnat,
        y = ~as.integer(duree), 
        color = ~catnat,
        type = "box")

summary(data %>% 
       filter(inondation==T) %>% 
       mutate(duree = as.integer(duree)) %>%
       select(duree))

summary(data %>% 
          filter(secheresse==T) %>% 
          mutate(duree = as.integer(duree)) %>%
          select(duree))


