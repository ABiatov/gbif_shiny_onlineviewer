# https://github.com/r-spatial/mapedit/issues/105

# setwd("C:/Mamba/Work/Presentations/2023-03_GBIF_Viewer/all_23-05-11/gbif_shiny_onlineviewer-main")

library(shiny)
library(leaflet)
library(mapedit)
library(sf)

# Load the sf object
nc <- st_read("../regions/gadm41_UKR_0.shp")

#poly_s <- "POLYGON ((36.28454 49.53358, 36.28454 49.64598, 36.41939 49.64598, 36.41939 49.53358, 36.28454 49.53358))"
#nc <- st_as_sfc(poly_s, crs = 4326)


# Project transformation
nc <- st_transform(
  nc, 
  crs = 4326
)

map <- leaflet() %>% 
  leaflet::addPolygons(
    data = nc,
    weight = 3,
    opacity = 1,
    fill = FALSE,
    color = 'black',
    fillOpacity = 1,
    smoothFactor = 0.01,
    group = "editable"
  )

ui <- fluidPage(
  
  # Application title
  titlePanel("Test Shiny Leaflet Mapedit"),
  
  sidebarLayout(
    sidebarPanel(
      actionButton('save', 'Save edits')
    ),
    
    mainPanel(
      editModUI("map")
    )
  )
)

server <- function(input, output) {
  
  edits <- callModule(
    editMod,
    leafmap = map,
    id = "map",
    #editor = "leafpm",
    targetLayerId = "editable"
  )
  
  observeEvent(input$save, {
    
    geom <- edits()$finished
    
    if (!is.null(geom)) {
      assign('new_geom', geom, envir = .GlobalEnv)
      sf::write_sf(
        geom, 
        'new_geom.geojson', 
        delete_layer = FALSE, 
        delete_dsn = TRUE
      )
    }
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
