

# # Crop test data ####
# load("~/GitHub/gbif_shiny_onlineviewer/data/gbif_sf_dataset.Rdata")
# box = c(xmin = 35.79, ymin = 49.32, xmax = 36.74, ymax = 50.24)
# data <- st_crop(gbif_sf_dataset, box) 
# # plot(data)
# rm(gbif_sf_dataset)
# save(data, file = "~/GitHub/gbif_shiny_onlineviewer/key_elements/Liaflet_filters/data.Rdata")

load("~/GitHub/gbif_shiny_onlineviewer/key_elements/Liaflet_filters/data.Rdata")

library(shiny)
library(shinyWidgets)
# library(sf)
library(leaflet)
# library(leaflet.extras)
# library(leafem)




ui <- fluidPage(
  # App title 
  titlePanel("Biodiversity Viewer"),
  # Tabs
  tabsetPanel(
    type = "tabs",
    tabPanel(
      "Карта",
      sidebarLayout(
        sidebarPanel(
          # sliderInput("range", "Year", min(data$year, na.rm = TRUE), max(data$year, na.rm = TRUE),
          #             value = range(data$year, na.rm = TRUE, finite = TRUE), step = 10
          #             ),
          pickerInput("iucn", "IUCN Red List",
                      unique(data$iucnRedListCategory),
                      selected = c(unique(data$iucnRedListCategory)),
                      options = list(`actions-box` = TRUE), multiple = T
          ),
          pickerInput("redbook", "Червона Книга України",
                       unique(data$ЧКУ),
                       selected = c(unique(data$ЧКУ)),
                       options = list(`actions-box` = TRUE), multiple = T
                      ),
          ),
          # checkboxInput("legend", "Show legend", TRUE)
        
        mainPanel(
          leafletOutput("map",  width = "100%", height="83vh"),
        )
      ),
    )
  )
)

server <- function(input, output, session) {
  
  filteredData <- reactive({
    # data[data$year >= input$range[1] & data$year <= input$range[2],] 
    data[data$iucnRedListCategory %in% input$iucn & data$ЧКУ %in% input$redbook ,]

  })
  
  data_bounds <- reactive(filteredData() %>% st_bbox() %>% as.character())
  
  main_map <-  leaflet() %>% addTiles()
  
  output$map <- renderLeaflet({
    main_map
  })
  
  # create map proxy to make further changes to existing map
  map <- leafletProxy("map", session)
  
  observe({
    map %>%
      clearShapes() %>%
      clearMarkers() %>%
      fitBounds(lng1 = data_bounds()[1], lat1 = data_bounds()[2], # set view by extent: p1 - top lext, p2 - bottom right
                lng2 = data_bounds()[3], lat2 = data_bounds()[4]) %>% # extent is set after selection of oblast
      addCircleMarkers(data = filteredData(), 
                       radius = 2,
                       color = "red",
                       popup = ~scientificName
      )
  })
  
  
  # observe({
  #   print("data_bounds:")
  #   print(data_bounds())
  # })
  
}

shinyApp(ui, server)



