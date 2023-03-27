library(shiny)
library(rgbif)
library(leaflet)
library(leaflet.extras)
library(leafem)
library(dplyr)
library(data.table)
library(DT)
library(sf)

specieses_list = c("Plecotus auritus", "Nyctalus leisleri", 
                   "Coronella austriaca", "Columba oenas", 
                   "Morchella steppicola", "Fritillaria meleagroides", 
                   "Tulipa quercetorum", "Utricularia minor")

aoi_buffered_wkt <- "POLYGON ((36.19919 49.56043, 36.19478 49.56584, 36.19447 49.57197, 36.21231 49.63071, 36.21512 49.6353, 36.22026 49.63894, 36.22707 49.64114, 36.23464 49.64162, 36.29995 49.63796, 36.3474 49.65431, 36.35541 49.65585, 36.36371 49.65523, 36.37097 49.65255, 36.40945 49.63076, 36.41511 49.62558, 36.41653 49.6193, 36.41455 49.60486, 36.44478 49.58112, 36.44842 49.57692, 36.44957 49.57215, 36.44809 49.56742, 36.44417 49.56332, 36.39401 49.52726, 36.38753 49.52411, 36.38014 49.52287, 36.37879 49.51815, 36.37329 49.50879, 36.3695 49.5047, 36.36378 49.50171, 36.35683 49.50019, 36.3495 49.50034, 36.31961 49.5044, 36.31943 49.50334, 36.31559 49.49874, 36.30938 49.49543, 36.3017 49.49388, 36.23095 49.48898, 36.22353 49.48933, 36.21678 49.49135, 36.21153 49.49479, 36.20847 49.4992, 36.20797 49.50403, 36.21414 49.53968, 36.21712 49.5453, 36.21995 49.54719, 36.21787 49.54766, 36.21224 49.55063, 36.19919 49.56043))"


# Frontend ####
ui = fluidPage(
  titlePanel("Biodiversity Viewer"),
  sidebarLayout(
    ## Button for send GBIF request ####
    sidebarPanel(
      actionButton("act_get_gbif_data", label = "Отримати GBIF дані"),
    ),
    mainPanel(
      leafletOutput("map",  width = "100%", height="50vh"),
      DT::dataTableOutput("gbif_table")
    )
  )
)

# Beckend ####
server = function(input, output, session) {
  
  recieved_data <- eventReactive(eventExpr = input$act_get_gbif_data, 
                   valueExpr = {
                     occ_search(scientificName = specieses_list, 
                                return = "data", 
                                hasCoordinate = T, 
                                geometry = aoi_buffered_wkt, 
                                limit = 500)
                     }
                   )
  #  <- rbindlist(lapply(recieved_data, function(x) x$data), fill = TRUE, use.names = TRUE)
  df_recieved_data <- reactive(rbindlist(lapply(recieved_data(), function(x) x$data), fill = TRUE, use.names = TRUE))
  output$gbif_table <- DT::renderDataTable(df_recieved_data())
  
  ## create the leaflet map ####
  main_map <-  leaflet() %>% addTiles()
  
  output$map <- renderLeaflet({
    main_map
  })
  
  map <- leafletProxy("map", session)
  
  observe({
    gbif_data_bounds <- reactive(df_recieved_data() %>% st_bbox() %>% as.character())
    map %>% 
      fitBounds(lng1 = min(df_recieved_data()[["decimalLongitude"]]), lat1 = max(df_recieved_data()[["decimalLatitude"]]),  # set view by extent: p1 - top lext, p2 - bottom right
                      lng2 = max(df_recieved_data()[["decimalLongitude"]]), lat2 = min(df_recieved_data()[["decimalLatitude"]])) %>% # to allow map zoom to selected area
      addCircleMarkers(data = df_recieved_data(), lng = ~decimalLongitude, lat = ~decimalLatitude, # add circle markers from dataframe
                           # radius = 10,  # static radius
                           radius = 2,  # calculation radius
                           color = "red",
                           popup = ~name # simple popup from field "scientificName"
                           # popup = ~paste0("<center>" ,"<b>", scientificName, "</b>", "</center>", "<br>",   # popup with HTML
                           #                 "Население: ", population, " чел." )
                           )  
  }  
    )
  
  
  observe({   # применяется для доступа к реактивным переменным, распечатки их в консоль и отладки
    print("recieved_data: ")
    print(str(recieved_data()))
    # print("decimalLongitude: ")
    # print(min(df_recieved_data()[["decimalLatitude"]]))
    print("done")
  })
  
  
}

shinyApp(ui, server)









