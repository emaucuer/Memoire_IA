library(data.table)
library(ggplot2)
library(Hmisc)
library(viridis)
library(readr)
library(dplyr)
library(sf)
library(shiny)
library(leaflet)



cod_com = 91477 # Palaiseau
cod_com = 91471 # Orsay

# ---------------------------------------------------------------------------- #
# Import data ------------------------------------------------------------------
# ---------------------------------------------------------------------------- #
dataCoord <- read_rds("../../Data/Administratif/coordCom2018.rds")
dataCoord <- dataCoord %>% 
  dplyr::rename(id = code) %>% 
  filter(id == cod_com) %>%
  arrange(id)


dataVar <- read_delim("../../Data/aladin/indicesALADIN63_CNRM-CM5_1982_2005_all.txt",
                      delim = ";", skip = 44) # skip = 27

dataVar <- dataVar %>% 
  rename_with(~ tolower(gsub("#| ", "",.x))) %>%
  filter(! is.na(longitude)) %>%
  select(-starts_with("..")) %>%
  mutate(expo=1) %>%
  filter(annee==2000)



# ---------------------------------------------------------------------------- #
# Weight calculation -----------------------------------------------------------
# ---------------------------------------------------------------------------- #
output <- dataVar %>% select(point, longitude, latitude)

longPal = dataCoord$longM
latPal = dataCoord$latM

output$distance = sqrt((longPal - output$longitude)^2 + (latPal - output$latitude)^2)

P1=1e4
output$poids1e4 = exp(-(sqrt((longPal - output$longitude)^2 + (latPal - output$latitude)^2))*P1) 
output$part1e4 = output$poids1e4/sum(output$poids1e4)

P1=2000
output$poids5k = exp(-(sqrt((longPal - output$longitude)^2 + (latPal - output$latitude)^2))*P1) 
output$part5k = output$poids5k/sum(output$poids5k)

P1=1e3
output$poids1e3 = exp(-(sqrt((longPal - output$longitude)^2 + (latPal - output$latitude)^2))*P1) 
output$part1e3 = output$poids1e3/sum(output$poids1e3)

P1=1e2
output$poids1e2 = exp(-(sqrt((longPal - output$longitude)^2 + (latPal - output$latitude)^2))*P1) 
output$part1e2 = output$poids1e2/sum(output$poids1e2)





# ---------------------------------------------------------------------------- #
# Plot -------------------------------------------------------------------------
# ---------------------------------------------------------------------------- #
com_bound <- read_rds("../../Data/Data_GIS/geojson/communes-om.rds") %>%
  dplyr::filter(substr(code,1,2)==91)

output_plot1e2 <- output %>% filter(poids1e2 >0)
output_plot1e3 <- output %>% filter(poids1e3 >0)
output_plot5k <- output %>% filter(poids5k >0)
output_plot1e4 <- output %>% filter(poids1e4 >0)

pal <- colorNumeric(
  palette = "YlOrRd",
  domain = c(0,1))

# 1e2 --------- #
leaflet(com_bound) %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas)  %>%
  setView(lng = longPal, lat = latPal, zoom = 9) %>%
  addPolygons(color =~ifelse(code == cod_com, "grey40", "grey"), weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5) %>%
  addCircleMarkers(data = output_plot1e2, 
                   ~as.numeric(longitude),~as.numeric(latitude),
                   stroke = FALSE, fillOpacity  = 0.9,
                   fillColor = ~pal(part1e2), radius=5) %>%
  addLegend("bottomright", pal = pal, values = c(0,1),
            title = "Poid relatif",
            opacity = 1)
 
# 1e3 --------- #
leaflet(com_bound) %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas)  %>%
  setView(lng = longPal, lat = latPal, zoom = 9) %>%
  addMarkers(~as.numeric(longPal), ~as.numeric(latPal)) %>%
  addPolygons(color =~ifelse(code == cod_com, "grey40", "grey"), weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5) %>%
  addCircleMarkers(data = output_plot1e3, 
                   ~as.numeric(longitude),~as.numeric(latitude),
                   stroke = FALSE, fillOpacity  = 0.9,
                   fillColor = ~pal(part1e3), radius=5) %>%
  addLegend("bottomright", pal = pal, values = c(0,1),
            title = "Poid relatif",
            opacity = 1)


# 5k ---------- #
leaflet(com_bound) %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas)  %>%
  setView(lng = longPal, lat = latPal, zoom = 9) %>%
  addPolygons(color =~ifelse(code == cod_com, "grey90", "grey"), weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5) %>%
  addMarkers(~as.numeric(longPal), ~as.numeric(latPal)) %>%
  addCircleMarkers(data = output_plot5k, 
                   ~as.numeric(longitude),~as.numeric(latitude),
                   stroke = FALSE, fillOpacity  = 0.9,
                   fillColor = ~pal(part5k), radius=5) %>%
  addLegend("bottomright", pal = pal, values = c(0,1),
            title = "Poid relatif",
            opacity = 0.9)


# 1e4 ---------- #
leaflet(com_bound) %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas)  %>%
  setView(lng = longPal, lat = latPal, zoom = 9) %>%
  addPolygons(color =~ifelse(code == cod_com, "grey90", "grey"), weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5) %>%
  addMarkers(~as.numeric(longPal), ~as.numeric(latPal)) %>%
  addCircleMarkers(data = output_plot1e4, 
                   ~as.numeric(longitude),~as.numeric(latitude),
                   stroke = FALSE, fillOpacity  = 0.9,
                   fillColor = ~pal(part1e4), radius=5) %>%
  addLegend("bottomright", pal = pal, values = c(0,1),
            title = "Poid relatif",
            opacity = 0.9) 






leaflet(com_bound) %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas)  %>%
  setView(lng = longPal, lat = latPal, zoom = 9) %>%
  addPolygons(color =~ifelse(code == cod_com, "lightblue", "grey"), weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5) %>%
  addCircleMarkers(data = output_plot, ~as.numeric(longitude), ~as.numeric(latitude),
                   stroke = FALSE, fillOpacity  = 0.8, radius = ~30*part1e4)%>%
  addMarkers(~as.numeric(longPal), ~as.numeric(latPal))


leaflet(com_bound) %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas)  %>%
  setView(lng = longPal, lat = latPal, zoom = 9) %>%
  addPolygons(color =~ifelse(code == cod_com, "lightblue", "grey"), weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5) %>%
  addCircleMarkers(data = output_plot, ~as.numeric(longitude), ~as.numeric(latitude),
                   stroke = FALSE, fillOpacity  = 0.8, radius =1)%>%
  addMarkers(~as.numeric(longPal), ~as.numeric(latPal))
