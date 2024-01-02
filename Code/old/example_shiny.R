## app.R ##
library(shiny)    # for shiny apps
library(leaflet)  # renderLeaflet function
library(RSocrata)
library(dplyr)

area_bound <- sf::st_read("https://data.cityofchicago.org/resource/igwz-8jzy.geojson")
health <- read.socrata("https://data.cityofchicago.org/resource/iqnk-2tcu.json")

health[3:29] <- lapply(health[3:29], as.numeric)
#> Warning in lapply(health[3:29], as.numeric): NAs introduced by coercion
health_area <- left_join(area_bound, health, by = c("area_num_1" = "community_area"))

groups <- c("Breast Cancer" = "breast_cancer_in_females", "Firearm" = "firearm_related")

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        inputId = "group",
        label = "Select a group to map",
        choices = groups
      )
    ),
    mainPanel(
      leafletOutput("map", height = "600")
    )
  )
)

server = function(input, output) {
  group_to_map <- reactive({
    input$group
  })
  
  output$map <- renderLeaflet({
    
    leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
      addProviderTiles(providers$Stamen.TonerLite) %>%
      setView(lng = -87.623177,
              lat = 41.881832,
              zoom = 8.5)
    
  })
  
  observeEvent(input$group, {
    
    pal <- colorNumeric("viridis", range(health_area[[group_to_map()]]))
    
    leafletProxy("map") %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(data = health_area,
                  color = ~pal(health_area[[group_to_map()]]),
                  weight = 0.5,
                  fillOpacity = 0.5,
                  smoothFactor = 0.2) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = health_area[[group_to_map()]],
        title = "% of population"
      )
  })
  
}

shinyApp(ui, server)
#> 
#> Listening on http://127.0.0.1:5938