library(dplyr)
library(sf)
library(leaflet)



dep_bound <- readRDS("../../Data/Data_GIS/geojson/departements-vs-om.rds") %>%
  dplyr::filter(code<=95)

grille_safran <- read.csv2("../../Data/aladin/grilleSafran_utile_drias2021.csv")


leaflet(dep_bound) %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas)  %>%
  setView(lng = 3, lat = 47, zoom = 4) %>%
  addPolygons(color = "grey", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5) %>%
  addCircleMarkers(data = grille_safran, ~as.numeric(longitude), ~as.numeric(latitude),
                   stroke = FALSE, radius = 2, fillOpacity  = 1)


grille <- grille_safran %>% filter(X..idPointDrias < 300)
