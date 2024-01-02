library(sf)
library(leaflet)
library(dplyr)
library(readr)
library(stringi)

# Chargement des donnees -------------------------------------------------------
setwd("G:/Drive partagés/BU_ACT_FR/4 - Mémoires/Mémoires 2022/Mémoire de MAUCUER Eurydice/Code")
soil1998 <- sf::st_read("../Data/Soil_Geographical_DB/30169_L93/30169_L93.shp")
smuDB <- read_csv2("../Data/Soil_Geographical_DB/smu.csv")
stuDB <- read_csv2("../Data/Soil_Geographical_DB/stu.csv")
stuorgDB <- read_csv2("../Data/Soil_Geographical_DB/stuorg.csv")

stuDB <- stuDB %>% 
  merge(stuorgDB, by = "stu", suffixes = c) %>%
  merge(smuDB, by = "smu", suffixes = c("_stu","_smu"))

names(soil1998) <- stri_trans_tolower(names(soil1998)) 
soil1998 <- soil1998 %>% left_join(stuDB, by = "smu")




# TRI -------------------
tri_carte <- sf::read_sf("tri_2020_sig_di/n_carte_inond_s.shp")
tri_carte$scenarioNum <- as.integer(substr(tri_carte$scenario, 1,2))
triMoy <- tri_carte %>% filter(scenario == "02Moy")

pal <- colorBin("YlOrRd", domain = tri_carte$scenarioNum, reverse= T)

leaflet() %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
  setView(lng = 3, lat = 47, zoom = 4.5) %>%
  addPolygons(data = tri_carte$geometry,
              fillColor = pal(tri_carte$scenarioNum),
              weight = 0.5,
              fillOpacity = 1,
              color = "white",
              dashArray = "1",
              label = paste("<strong>Scenario</strong><br>", tri_carte$scenario) %>%
                lapply(htmltools::HTML)) %>%
  addLegend(
    position = "bottomleft",
    pal = pal,
    values = tri_carte$scenario)




# ARGILE --------------

argile <- sf::read_sf("../Data/AleaRG_Fxx_L93/ExpoArgile_Fxx_L93.shp")
argile <- a 



idf <- argile %>% filter(DPT %in% c("75","77","78","91","92","93","94","95"))
idf$geometry <- st_transform(idf$geometry, crs = st_crs("WGS84"))


pal <- colorBin("YlOrRd", domain = argile$NIVEAU)


leaflet() %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
  setView(lng = 3, lat = 47, zoom = 4.5) %>%
  addPolygons(data = idf$geometry,
              fillColor = pal(idf$NIVEAU),
              weight = 0.5,
              fillOpacity = 0.5,
              color = "white",
              dashArray = "1") %>%
  addLegend(
    position = "bottomleft",
    pal = pal,
    values = as.factor(idf$NIVEAU))

