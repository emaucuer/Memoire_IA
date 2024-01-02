
# Libraries
library(readr)
library(dplyr)
library(data.table)
library(plotly)
library(ggplot2)


# ---------------------------------------------------------------------------- #
# Importation et preparation des donnees ---------------------------------------
# ---------------------------------------------------------------------------- #

## Chargement des donnees Gapsar -----------------------------------------------

gaspar <- read_rds("../../Data/Gaspar/gaspar.rds")
gaspar_pl <- read_rds("../../Data/Gaspar/gaspar_pluri_annuel.rds")


## Variables temporelles -------------------------------------------------------
gaspar <- gaspar %>% mutate(mois_deb = month(date_debut),
                            mois_fin = month(date_fin))


# ---------------------------------------------------------------------------- #
# Analyse des donnees ----------------------------------------------------------
# ---------------------------------------------------------------------------- #

## Nombre de Cat Nat par mois --------------------------------------------------
gaspar_ino <- gaspar %>% 
  filter(inondation == TRUE) %>%
  select(-secheresse, -inondation, -inondation_all)

table(gaspar_ino$mois_deb)


gaspar_sec <- gaspar %>% 
  filter(secheresse == TRUE) %>%
  select(-secheresse, -inondation, -inondation_all)

table(gaspar_ino$mois_fin)


## Evolution annuelle ----------------------------------------------------------
an <- sort(rep(1982:2021, 12))
mois <- rep(1:12, 2021-1982+1)
all_y_m <- data.table(an, mois) 


gaspar_ino_ag <- gaspar_ino %>% 
  group_by(an_debut, mois_deb) %>%
  summarise(ino = n()) %>% 
  merge(all_y_m, by.x = c("an_debut", "mois_deb"), by.y = c("an", "mois"), all.y = T) %>%
  replace(is.na(.), 0) %>%
  filter((an_debut>1982 & an_debut<2022) |
            (an_debut==1982 & mois_deb >= 8) |
            (an_debut==2022 & mois_deb <= 3)) %>%
  mutate(ym = paste(an_debut,mois_deb, sep = "-"))





plot_ly(gaspar_ino_ag) %>%
  add_lines(x =~ ym, y =~ ino) %>%
  layout(title = "", #"Nombre de communes reconnues par exercice",
         xaxis = list(tickangle = -45,title = ""),
         yaxis = list(title = ""),
         margin = list(l=50, r=50, b=50, t=50))

plot_ly(gaspar %>% 
          filter(inondation_all == T) %>%
          group_by(mois_deb, lib_risque_jo) %>% 
          summarise(ino = n() )) %>%
  add_bars(x =~ mois_deb, y =~ ino, color =~ lib_risque_jo ) %>%
  layout(title = "", #"Nombre de communes reconnues par exercice",
         xaxis = list(tickangle = -45,title = ""),
         yaxis = list(title = ""),
         margin = list(l=50, r=50, b=50, t=50),
         barmode = 'stack')
