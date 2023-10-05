## Import libraries ####
library(dplyr)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(sf)
library(sp)
library(leaflet)
# library(tmap)

## import data     ####
#Ukr_0 <- st_read("../regions/gadm41_UKR_0.shp")
Ukr_1 <- st_read("../regions/gadm41_UKR_1.shp")
#Ukr_2 <- st_read("../regions/gadm41_UKR_2.shp")

## define user interface ####
ui <- dashboardPage(
  dashboardHeader(title = "Region selection"),
  dashboardSidebar(
    pickerInput('regions', 'Select region', unique(Ukr_1$NL_NAME_1),
                selected = c(unique(Ukr_1$NL_NAME_1)),
                options = list(`actions-box` = TRUE), multiple = T)
  ),
  dashboardBody(
    tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
    leafletOutput("map"),
  )
)

## define back-end ####
server <- function(input, output) {
  obl <- reactive(subset(Ukr_1, Ukr_1$NL_NAME_1 %in% input$regions))
  output$map <- renderLeaflet({
    leaflet() %>% 
      addTiles(group = "OSM (default)") %>%
      addPolygons(data = obl(), weight = 2, fill = F)
  })
}

shinyApp(ui, server)
