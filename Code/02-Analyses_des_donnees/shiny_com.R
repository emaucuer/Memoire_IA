# ----------------------------------------------------------------------- #
# Cartes interractives permettant de représenter 
# ----------------------------------------------------------------------- #


library(shiny)
library(leaflet)
library(dplyr)
library(tidyr)
library(reshape)
library(readr)
library(data.table)

# Création de la base de plot
dataFinaleCom <- read_rds("../Data/data_modelisation_1982_2021_all_V2.rds")
com_bound <- read_rds("../Data_GIS/geojson/communes-vs-om.rds")
setDT(dataFinaleCom)
setDT(com_bound)
dataPlot<- merge(dataFinaleCom, com_bound, by.x="id", by.y="code")


# Quelques paramètres généraux pour les cartes
list_var = setdiff(names(dataFinaleCom), c("id","annee","longM","latM","commune","code_dep"))

hl_opt <- highlightOptions(
  weight = 2,
  color = "#666",
  dashArray = "",
  bringToFront = TRUE)


# UI -----
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("year", label=h5("Sélectionner une année :"),
                  choices = as.character(1982:2021)),
      selectInput("var", h5("Sélectionner une variable :"), 
                  choices = list_var)
    ),
    
    mainPanel(
      leafletOutput("map", height = "590")
    )
  )
)



server = function(input, output){
  year_to_map <- reactive({input$year})
  var_to_map <- reactive({input$var})

  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
      setView(lng = 3, lat = 47, zoom = 4.5) 
  })
  
  observeEvent(c(input$year, input$var), {
    # Préparation des données
    dataP <- dataPlot[annee==year_to_map(),]
    dataP <- dataP %>% dplyr::rename(my_var = !!var_to_map())
    
    # Création des palettes
    palette <- ifelse(var_to_map()=="inondation", "Blues", 
                       ifelse(var_to_map()=="secheresse", "Reds", "BuPu"))
    pal <- colorNumeric(palette, range(dataP$my_var, na.rm=T))
    
    leafletProxy("map") %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(data = dataP$geometry,
                  fillColor = pal(dataP$my_var),
                  label = paste(
                    "<strong>", dataP$nom,
                    "</strong><br>", dataP$my_var) %>%
                    lapply(htmltools::HTML),
                  weight = 0.5,
                  fillOpacity = 0.5,
                  color = "white",
                  dashArray = "1",
                  highlightOptions = hl_opt) %>%
      addLegend(
        position = "bottomleft",
        pal = pal,
        values = dataP$my_var,
        title = var_to_map())
  })
 
}

shinyApp(ui, server)




