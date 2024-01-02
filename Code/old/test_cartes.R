library(leaflet)
library(maps)
library(rgdal)

m <- leaflet() %>%
  setView(lng = 3, lat = 47, zoom = 5) %>%
  addTiles()
m

map("france", fill = TRUE, plot = T)



%>%
  addPolygons(data = inondation_dep,
              color = ~pal(inondation_dep[["1982"]]),
              weight = 0.5,
              fillOpacity = 0.5,
              smoothFactor = 0.2) 




# Cartes :


communes_shp <- readOGR(paste0(global_path, "/Data_GIS/communes-20220101.shp"),
                        layer = "communes-20220101", 
                        GDAL1_integer64_policy = TRUE)
dep_shp <- readOGR(paste0(global_path, "/Data_GIS/departements-20220101.shp"),
                   layer = "departements-20220101", 
                   GDAL1_integer64_policy = TRUE)


dep_shp@data <- dep_shp@data %>% 
  merge(data_dep_an %>% filter(an_debut == 2001),
        by.x="code_insee",by.y="departement")



pal <- colorBin("YlOrRd", domain = dep_shp@data$nb_inondation)

leaflet(dep_shp) %>%
  addTiles() %>%
  setView(lng = 3, lat = 47, zoom = 4) %>%
  addPolygons(color = "white", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = ~pal(nb_inondation))
