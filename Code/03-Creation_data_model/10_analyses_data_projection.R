# -----------------------------------------------------------------------------#
#                   Analyse des données de projection 
# -----------------------------------------------------------------------------#


library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(data.table)
library(corrplot)
library(plotly)


# -----------------------------------------------------------------------------#
# Evolution des variables à horizon 2050 ---------------------------------------
# -----------------------------------------------------------------------------#

data85 <- read_rds("../../Data/projection/data_proj_2022_2050_RCP85_A2.rds")
data45 <- read_rds("../../Data/projection/data_proj_2022_2050_RCP45_B1.rds")

deltaA2 <- dataFinaleCom_new_pl %>% 
  select(id, starts_with("sswi")) %>%
  group_by(id) %>%
  summarise_all(mean) %>%
  merge(data85 %>%
          select(id, annee, starts_with("sswi")) %>%
          filter(annee == 2050)
        , by = "id", suffixes = c("_hist", "_2050")) %>%
  mutate(delta_min = sswi_min_2050-sswi_min_hist,
         delta_max = sswi_max_2050-sswi_max_hist,
         delta_moy = sswi_moy_2050-sswi_moy_hist)

deltaB1 <- dataFinaleCom_new_pl %>% 
  select(id, starts_with("sswi")) %>%
  group_by(id) %>%
  summarise_all(mean) %>%
  merge(data45 %>%
          select(id, annee, starts_with("sswi")) %>%
          filter(annee == 2050)
        , by = "id", suffixes = c("_hist", "_2050")) %>%
  mutate(delta_min = sswi_min_2050-sswi_min_hist,
         delta_max = sswi_max_2050-sswi_max_hist,
         delta_moy = sswi_moy_2050-sswi_moy_hist)


