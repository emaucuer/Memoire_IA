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
dataFinaleDep <- read_rds("../../Data/Modelisation/data_modelisation_dep_1982_2021.rds")
dep_bound <- read_rds("../../Data/Data_GIS/geojson/departements-vs-om.rds")
setDT(dataFinaleDep)
setDT(dep_bound)
dataPlot<- merge(dataFinaleDep, dep_bound, by.x="dep", by.y="code")


# Quelques paramètres généraux pour les cartes
list_var = setdiff(names(dataFinaleDep), c("dep","annee","longM","latM","commune"))

my_colors = c("#525252", "#5e7187", "#3c98b6", "#00c2c8", "#1de9b6")

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
      leafletOutput("map", height = 650)
    )
  )
)


# SERVER -----
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
    # palette <- ifelse(var_to_map()=="inondation", "Blues", 
    #                   ifelse(var_to_map()=="secheresse", "Reds", "BuPu"))
    pal <- colorNumeric(my_colors, range(dataP$my_var, na.rm=T))
    
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




