-------------------------------------------- #
  
  # Libraries

library(readr)
library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)
library(plotly)
library(sf)



# Chargement des donnees ------------------------------------
# Data plot
com_bound <- read_rds("../Data_GIS/geojson/communes-vs.rds")
dep_bound <- read_rds("../Data_GIS/geojson/departements-vs-om.rds") %>%
  dplyr::filter(code<=95)


# Data gaspar AZI
azi <- read_csv2("../Data/CATNAT/gaspar/azi_gaspar.csv")

azi <- azi %>% 
  arrange(cod_commune, desc(dat_diffusion)) %>%
  mutate(indic_azi = "Oui") %>%
  distinct(cod_commune, .keep_all = T) 

aziPlot <- com_bound %>% merge(azi, by.y="cod_commune", by.x = "code", all.x=T)

aziPlot <- aziPlot %>% mutate(indic_azi = ifelse(is.na(indic_azi), "Non", indic_azi))

#png("../Plots&Resultats/exposition_risques/map_azi.png", units="in", width=8.5, height=5, res=300)
ggplot(data=aziPlot) +
  geom_sf(aes(geometry = geometry,fill = as.factor(indic_azi)), color = NA) +
  geom_sf(data = dep_bound, aes(geometry = geometry), color="grey40", fill = NA) +
  scale_fill_manual(values =c('white','#4292c6')) +
  theme_void() + 
  guides(fill=guide_legend(title="Commune concernée par \nun Atlas de Zone Inondable"))
dev.off()

