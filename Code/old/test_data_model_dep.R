library(dplyr)
library(readr)
library(sf)

dep_bound <- sf::st_read("../Data_GIS/DEPARTEMENT.shp")


# Création de la base par département à partir de celle par commune
dataFinaleCom <- 
dataVar <- read_delim("../Data/indicesALADIN63_CNRM-CM5_1982_2005.txt",
                      delim = ";", skip = 27)


dataVar <- dataVar %>% 
  rename_with(~ tolower(gsub("#| ", "",.x))) %>%
  filter(! is.na(longitude)) %>%
  select(-...17) %>%
  mutate(expo=1)

dataVar = st_as_sf(dataVar, coords = c("longitude","latitude"), 
                   remove = FALSE)
dataVar$geometry <- st_set_crs(dataVar$geometry, "WGS84")

dataPoints <- dataVar %>% 
  select(point, latitude, longitude, geometry) %>%
  distinct()

dataPoints <- st_join(dataPoints, dep_bound)

sort(table(test$NOM, useNA='ifany')) # 65 points non attribues

library(leaflet)
library(htmltools)
leaflet(test) %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
  setView(lng = 3, lat = 47, zoom = 4.5) %>%
  addPolygons(data=dep_bound, weight = 1, smoothFactor = 0.5, 
              color='black', fillColor="transparent") %>%
  addCircleMarkers(radius=2, popup = ~htmlEscape(NOM))

