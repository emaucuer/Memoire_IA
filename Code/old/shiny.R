# ----------------------------------------------------------------------- #
# Cartes interractives permettant de représenter le nb de périls par an
# pour une maille géographique donnée
# Pour lancer le shiny : Ctr+A puis Ctrl+Entrée
# Etre patient pour l'affichage en maille commune qui est un peu lent
# ----------------------------------------------------------------------- #


require(shiny)
require(leaflet)
require(dplyr)
require(tidyr)
require(reshape)

# Contours
dep_bound <- sf::st_read("../Data_GIS/geojson/departements-version-simplifiee.geojson")
com_bound <- sf::st_read("../Data_GIS/geojson/communes-version-simplifiee.geojson")

# Data par dep
data_dep <- readRDS("../Data/data_dep_an.rds")

data_inondation <- data_dep %>% cast(departement ~ annee, value = "nb_inondation")
dep_bound_inondation <- left_join(dep_bound, data_inondation, by = c("code"="departement"))
data_secheresse <- data_dep %>% cast(departement ~ annee, value = "nb_secheresse")
dep_bound_secheresse <- left_join(dep_bound, data_secheresse, by = c("code"="departement"))

# Data par dep
data_dep_dedup <- readRDS("../Data/data_dep_an_dedup.rds")

data_inondation_dedup <- data_dep_dedup %>% cast(departement ~ annee, value = "nb_inondation")
dep_bound_inondation_dedup <- left_join(dep_bound, data_inondation_dedup, by = c("code"="departement"))
data_secheresse_dedup <- data_dep_dedup %>% cast(departement ~ annee, value = "nb_secheresse")
dep_bound_secheresse_dedup <- left_join(dep_bound, data_secheresse_dedup, by = c("code"="departement"))

# Data par com
data_com <- readRDS("../Data/data_com_an.rds")

data_inondation_c <- data_com %>% cast(insee ~ annee, value = "nb_inondation")
com_bound_inondation <- left_join(com_bound, data_inondation_c, by = c("code"="insee"))
data_secheresse_c <- data_com %>% cast(insee ~ annee, value = "nb_secheresse")
com_bound_secheresse <- left_join(com_bound, data_secheresse_c, by = c("code"="insee"))


# Quelques paramètres généraux pour les cartes
hl_opt <- highlightOptions(
  weight = 2,
  color = "#666",
  dashArray = "",
  bringToFront = TRUE)


# UI -----
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("year", label=h3("Sélectionner une année"),
                  choices = as.character(1982:2015)),
      radioButtons("peril", label = h3("Sélectionner un péril"),
                   choices = list("Inondation" = 1, "Secheresse" = 2), 
                   selected = 1),
      radioButtons("geo", label = h3("Sélectionner une granularité"),
                   choices = list("Départements sans déduplication" = 1, 
                                  "Départements avec déduplication" = 2, 
                                  "Communes" = 3), 
                   selected = 1)
    ),
    
    mainPanel(
      leafletOutput("map", height = "605")
    )
  )
)



server = function(input, output) {
  
  year_to_map <- reactive({input$year})
  peril_to_map <- reactive({input$peril})
  geo_to_map <- reactive({input$geo})
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
      setView(lng = 3, lat = 47, zoom = 4.5) 
  })
  
  observeEvent(c(input$year, input$peril, input$geo), {
    if (peril_to_map() == "1"){  # Cartes inondation
      
      if (geo_to_map() == "1"){ # Inondations par dep sans dedup
        pal <- colorNumeric("Blues", range(dep_bound_inondation[[year_to_map()]], na.rm=T))
        leafletProxy("map") %>%
          clearShapes() %>%
          clearControls() %>%
          addPolygons(data = dep_bound_inondation,
                      fillColor =~ pal(dep_bound_inondation[[year_to_map()]]),
                      label =~ paste(dep_bound_inondation$nom),
                      weight = 0.5,
                      fillOpacity = 0.5,
                      color = "white",
                      dashArray = "1",
                      highlightOptions = hl_opt) %>%
          addLegend(
            position = "bottomleft",
            pal = pal,
            values = dep_bound_inondation[[year_to_map()]],
            title = "Nombre d'inondations")
        
      } else if(geo_to_map() == "2"){ # Inondations par dep avec dedup
        pal <- colorNumeric("Blues", range(dep_bound_inondation_dedup[[year_to_map()]], na.rm=T))
        leafletProxy("map") %>%
          clearShapes() %>%
          clearControls() %>%
          addPolygons(data = dep_bound_inondation_dedup,
                      fillColor =~ pal(dep_bound_inondation_dedup[[year_to_map()]]),
                      label =~ paste(dep_bound_inondation_dedup$nom),
                      weight = 0.5,
                      fillOpacity = 0.5,
                      color = "white",
                      dashArray = "1",
                      highlightOptions = hl_opt) %>%
          addLegend(
            position = "bottomleft",
            pal = pal,
            values = dep_bound_inondation_dedup[[year_to_map()]],
            title = "Nombre d'inondations")
        
      }else{ # Inondations par com
        pal <- colorNumeric("Blues", range(com_bound_inondation[[year_to_map()]], na.rm=T))
        leafletProxy("map") %>%
          clearShapes() %>%
          clearControls() %>%
          addPolygons(data = com_bound_inondation,
                      color =~ pal(com_bound_inondation[[year_to_map()]]),
                      fillColor =~ pal(com_bound_inondation[[year_to_map()]]),
                      label =~ paste(com_bound_inondation$nom),
                      weight = 0.5,
                      fillOpacity = 0.6,
                      dashArray = "1",
                      highlightOptions = hl_opt) %>%
          addLegend(
            position = "bottomleft",
            pal = pal,
            values = com_bound_inondation[[year_to_map()]],
            title = "Nombre d'inondations")
      }
      
    }else{ # Cartes secheresse
      
      if (geo_to_map() == "1"){ # Secheresses par dep sans dedup
        pal <- colorNumeric("Reds", range(dep_bound_secheresse[[year_to_map()]], na.rm=T))
        
        leafletProxy("map") %>%
          clearShapes() %>%
          clearControls() %>%
          addPolygons(data = dep_bound_secheresse,
                      fillColor =~ pal(dep_bound_secheresse[[year_to_map()]]),
                      label =~ paste(dep_bound_secheresse$nom),
                      weight = 0.5,
                      fillOpacity = 0.5,
                      color = "white",
                      dashArray = "1",
                      highlightOptions = hl_opt) %>%
          addLegend(
            position = "bottomleft",
            pal = pal,
            values = dep_bound_secheresse[[year_to_map()]],
            title = "Nombre de secheresses")
        
      } else if(geo_to_map() == "2"){ # Secheresses par dep avec dedup
        pal <- colorNumeric("Reds", range(dep_bound_secheresse_dedup[[year_to_map()]], na.rm=T))
        
        leafletProxy("map") %>%
          clearShapes() %>%
          clearControls() %>%
          addPolygons(data = dep_bound_secheresse_dedup,
                      fillColor =~ pal(dep_bound_secheresse_dedup[[year_to_map()]]),
                      label =~ paste(dep_bound_secheresse_dedup$nom),
                      weight = 0.5,
                      fillOpacity = 0.5,
                      color = "white",
                      dashArray = "1",
                      highlightOptions = hl_opt) %>%
          addLegend(
            position = "bottomleft",
            pal = pal,
            values = dep_bound_secheresse_dedup[[year_to_map()]],
            title = "Nombre de secheresses")
        
      } else{ # Secheresses par com
        pal <- colorNumeric("Reds", range(com_bound_inondation[[year_to_map()]], na.rm=T))
        leafletProxy("map") %>%
          clearShapes() %>%
          clearControls() %>%
          addPolygons(data = com_bound_secheresse,
                      color =~ pal(com_bound_secheresse[[year_to_map()]]),
                      fillColor =~ pal(com_bound_secheresse[[year_to_map()]]),
                      label =~ paste(com_bound_secheresse$nom),
                      weight = 0.5,
                      fillOpacity = 0.6,
                      dashArray = "1",
                      highlightOptions = hl_opt) %>%
          addLegend(
            position = "bottomleft",
            pal = pal,
            values = com_bound_secheresse[[year_to_map()]],
            title = "Nombre de secheresses")
      }
    }
  })
  
}

shinyApp(ui, server)
