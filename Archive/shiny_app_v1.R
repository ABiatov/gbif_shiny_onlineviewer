# setwd("C:/Mamba/Work/Presentations/2023-03_GBIF_Viewer/all_23-03-20/gbif_shiny_onlineviewer-add_filter_geometry_by_region")

# Biodiversity Viewer v.0.1

# use https://mastering-shiny.org/action-layout.html
# https://shiny.rstudio.com/gallery/viscover.html
# https://github.com/XiaodanLyu/viscover/blob/master/inst/shiny-examples/overlay/app.r

# Import libs ####
# library(tidyverse)
library(dplyr)
library(shiny)
library(shinyWidgets)
# library(shinyjs)
library(sf)
library(sp)
library(rgeos)
# library(dismo)
library(rgbif)
library(leaflet)
library(leaflet.extras)
library(leafem)
library(DT)
library(data.table)

# TODO add function polygon_bufferisation(polygon, radius)

# Calling custom function
source("scripts/config.R")
source("functions/polygon_bufferisation.R")

# import data ####
## import spatial data ####
# Ukr_0 <- st_read("regions/gadm41_UKR_0.shp")
Ukr_1 <- st_read("regions/gadm41_UKR_1.shp")
Ukr_2 <- st_read("regions/gadm41_UKR_2.shp")

## load Red lists ####
load(file = "dictionaries/df_protected_status.Rdata")
load(file = "dictionaries/red_book_vrazlyvyi.Rdata")
load(file = "dictionaries/red_book_ridkisnyi.Rdata")
load(file = "dictionaries/red_book_znykaiuchyi.Rdata")
load(file = "dictionaries/red_book_znyklyi_v_pryrodi.Rdata")
load(file = "dictionaries/red_book_znyklyi.Rdata")
load(file = "dictionaries/red_book_nedostatno_vidomyi.Rdata")
load(file = "dictionaries/red_book_neotsinenyi.Rdata")
load(file = "dictionaries/bern_appendix_2.Rdata")
load(file = "dictionaries/bern_appendix_3.Rdata")
load(file = "dictionaries/bern_resolution_6.Rdata")
load(file = "dictionaries/redlist_poltavska.Rdata")
load(file = "dictionaries/redlist_chernivetska.Rdata")
load(file = "dictionaries/invasive_specieses.Rdata")

df_redbook <- dplyr::select(df_protected_status, all_of(c("base_name", "scientificName", "nameUk", "ЧКУ", "kingdom", "phylum", "class"))) %>% filter(ЧКУ != "NA")

# Frontend ####
ui = fluidPage(
  # App title 
  titlePanel("Biodiversity Viewer"),
  # Tabs
  tabsetPanel(type = "tabs",
              ## Tab main map ####
              tabPanel("Карта", 
                       ### Sidebar layout with input and output definitions ####
                       sidebarLayout(
                         #### Sidebar panel for inputs ####
                         sidebarPanel(
                           ## Oblast selection ####
                           pickerInput('regions', 'Оберіть область', unique(Ukr_1$NL_NAME_1),
                                       selected = c(unique(Ukr_1$NL_NAME_1)),
                                       options = list(`actions-box` = TRUE), multiple = T),
                           ## Raion selection ####
                           pickerInput('raions', 'Оберіть район',
                                       unique(Ukr_2$NAME_2),
                                       selected = NULL, #c(unique(Ukr_2$NAME_2)), # no regions initially selected
                                       options = list(`actions-box` = TRUE), multiple = T),
                           ## Select Conservation status ####
                           checkboxGroupInput(
                             inputId = "redbook_finder",
                             label = "Охоронні категорії Червоної книги України",
                             choices = c("вразливий" = "red_book_vrazlyvyi", "рідкісний" = "red_book_ridkisnyi",
                                         "зникаючий" = "red_book_znykaiuchyi", "зниклий в природі" = "red_book_znyklyi_v_pryrodi",
                                         "зниклий" = "red_book_znyklyi", "недостатньо відомий" = "red_book_nedostatno_vidomyi",
                                         "неоцінений" = "red_book_neotsinenyi" ),
                             selected = c("red_book_vrazlyvyi", "red_book_ridkisnyi",
                                          "red_book_znykaiuchyi", "red_book_znyklyi_v_pryrodi",
                                          "red_book_znyklyi", "red_book_nedostatno_vidomyi", 
                                          "red_book_neotsinenyi")
                           ),
                           
                           ## Select buffer radius ####
                           radioButtons(
                             inputId = "buffer_radius",
                             label = "Буфер довкола області інтересу",
                             choices = c(
                               "немає" = 0,
                               "1 км" = 1000,
                               "5 км" = 5000,
                               "10 км" = 10000,
                               "20 км" = 20000
                             ),
                             selected = 5000
                           ),
                           
                           ## Button for generate buffer ####
                           #submitButton("Побудувати / Видалити буфер"),
                           #br(),
                           ## Button for send GBIF request ####
                           actionButton("act_get_gbif_data", label = "Отримати GBIF дані"), # на эту кнопку повесить запрос данных из GBIF
                         ),
                         #### Main map panel for displaying outputs ####
                         mainPanel(
                           leafletOutput("map",  width = "100%", height="85vh"),
                         )
                       )
              ),
              ## Tab - Попередній перегляд ####
              tabPanel("Попередній перегляд", 
                       DT::dataTableOutput("gbif_table")
              ),
              ## Tab - Генерування звітів ####              
              tabPanel("Генерування звітів",
                       
              ),
              ## Tab Червона Книга України - for testing ####              
              tabPanel("Червона Книга України",
                       # tableOutput("my_table")
                       DT::dataTableOutput("redbook_table"),
              )
  )
)

# Back end ####
server = function(input, output, session) {
  
  # Create a global reactive value
  ## Create a global reactive value for WKT guffered polygon
  reaktive_bufered_polygon_wkt <- reactiveVal() # Create a global reactive value for WKT guffered polygon
  
  ## create the leaflet map ####
  main_map <-  leaflet() %>% addTiles() %>% #setView(36.39, 49.65, zoom = 11) %>% # to set view around Homilsha forest
    # addSearchOSM() %>% 
    #fitBounds(lng1 = 22.14, lat1 = 52.35,  # set view by extent: p1 - top lext, p2 - bottom right # extent is set later
    #          lng2 = 40.22, lat2 = 33.68) %>% # to allow map zoom to selected area
    leafem::addMouseCoordinates() %>%
    addDrawToolbar(
      polylineOptions = FALSE,
      # polygonOptions = TRUE,
      polygonOptions = drawPolygonOptions(
        showArea = TRUE,
        repeatMode = F,
        shapeOptions = draw_new_shape_options,
        # zIndexOffset = 30
      ),
      # rectangleOptions = TRUE,
      rectangleOptions = drawRectangleOptions(
        showArea = TRUE,
        repeatMode = F,
        shapeOptions = draw_new_shape_options),
      circleOptions = FALSE,
      markerOptions = FALSE,
      circleMarkerOptions = FALSE,
      # markerOptions = drawMarkerOptions(markerIcon = myMarkerIcon(2)),
      singleFeature = TRUE,
      # editOptions = FALSE, # hidden editTool button
      editOptions = editToolbarOptions(
        edit = TRUE, # hidden edit button
        remove = TRUE),
    )
  
  output$map <- renderLeaflet({
    main_map
  })
  
  # create map proxy to make further changes to existing map
  map <- leafletProxy("map", session)
  
  ## create object with selected oblasts ####
  obl <- reactive(subset(Ukr_1, Ukr_1$NL_NAME_1 %in% input$regions))
  obl_bounds <- reactive(obl() %>% st_bbox() %>% as.character()) # to set extent around selected oblasts
  ## conditional selection of raions based on selected oblast ####
  observeEvent(input$regions, {
    clearShapes(map) # clean map
    clearMarkers(map) # clean previously loaded markers
    
    updatePickerInput(session = session, inputId = "raions",
                      choices = subset(unique(Ukr_2$NAME_2), Ukr_2$NL_NAME_1 %in% input$regions),
                      selected = NULL) # raions are shown but nothing is selected initially
    map %>%  # oblasts are not added because I don't see a sense in it (YY)
      addPolygons(data = obl(), weight = 2, fill = F) %>%
      fitBounds(lng1 = obl_bounds()[1], lat1 = obl_bounds()[2], # set view by extent: p1 - top lext, p2 - bottom right
                lng2 = obl_bounds()[3], lat2 = obl_bounds()[4]) # extent is set after selection of oblast
  })
  
  ## create object with selected raion ##
  raion <- reactive(subset(Ukr_2, Ukr_2$NAME_2 %in% input$raions))
  raion_bounds <- reactive(raion() %>% st_bbox() %>% as.character()) # to set extent around selected raions
  ## change map based on selected raions ####
  observeEvent(input$raions, {
    clearShapes(map) # clean map
    clearMarkers(map) # clean previously loaded markers
    
    raion_buffered <- polygon_bufferisation(raion(), input$buffer_radius) %>%
      st_union() # dissolve all created polygons to avoid overlaps
    
    map %>% 
      addPolygons(data = raion(), weight = 2, fill = F) %>%
      fitBounds(lng1 = raion_bounds()[1], lat1 = raion_bounds()[2],  # set view by extent: p1 - top lext, p2 - bottom right
                lng2 = raion_bounds()[3], lat2 = raion_bounds()[4]) %>% # to allow map zoom to selected area
      addPolygons(data = raion_buffered, #layerId = id,   # add buffered polygon to map
                  options = buffered_polygon_options)
    
    # raion_buffered_geom <- st_geometry(raion_buffered) # to extract geometry
    # raion_buffered_WKT <- st_as_text(raion_buffered_geom) # to transform geometry to WKT
    raion_buffered_WKT <- st_as_text(raion_buffered) # to transform geometry to WKT
    reaktive_bufered_polygon_wkt(raion_buffered_WKT)    # write raion_buffered_WKT in my custom global reactive value
  })
  
  ## Add admin division layers to map if checkbox "Обрати способ вводу територїї" choused "Вибрти з адмінподілу"  ####
  # map %>% addPolygons(data = raion(), weight = 2, fill = F)}
  # observeEvent(input , {map %>% addPolygons(data = raion(), weight = 2, fill = F)})
  
  ## Draw polygon ####
  # aoi_poly <- eventReactive(input$map_draw_new_feature,{
  #   coords <- input$map_draw_new_feature$geometry$coordinates %>% unlist %>% matrix(nc = 2, byrow = T)
  #   sp_aoi_polygon <- sp::Polygon(coords) %>% list %>% sp::Polygons(ID=1) %>% list %>% sp::SpatialPolygons()
  #   sf_aoi_polygon <- st_as_sfc(sp_curent_polygon) %>% st_set_crs(4326)
  # })
  
  observeEvent(input$map_draw_new_feature, {
    clearShapes(map) # clean map
    clearMarkers(map) # clean previously loaded markers
    # Leaflet ID to edit
    id = input$map_draw_new_feature$properties$"_leaflet_id"
    print("leaflet_id")
    print(id)
    coords <- input$map_draw_new_feature$geometry$coordinates %>% unlist %>% matrix(nc = 2, byrow = T)
    curent_polygon <- sp::Polygon(coords) %>% list %>% sp::Polygons(ID=1) %>% list %>% sp::SpatialPolygons()
    sf_curent_polygon <- st_as_sfc(curent_polygon) %>% st_set_crs(4326) # it work
    curent_polygon_WKT <- st_as_text(sf_curent_polygon) # it work
    
    sf_curent_buffered <- polygon_bufferisation(sf_curent_polygon, input$buffer_radius)
    
    map %>% addPolygons(data = sf_curent_buffered, layerId = id,   # add buffered polygon to map
                        options = buffered_polygon_options 
    )
    
    curent_buffered_WKT <- st_as_text(sf_curent_buffered) # it work
    reaktive_bufered_polygon_wkt(curent_buffered_WKT)    # write curent_buffered_WKT in my custom global reactive value
    
  })
  
  observeEvent(input$map_draw_edited_features, {
    clearShapes(map) # clean map
    clearMarkers(map) # clean previously loaded markers
    # generate new buffer
    coords <- input$map_draw_edited_features$features[[1]]$geometry$coordinates %>% unlist %>% matrix(nc = 2, byrow = T)
    curent_polygon <- sp::Polygon(coords) %>% list %>% sp::Polygons(ID=1) %>% list %>% sp::SpatialPolygons()
    sf_curent_polygon <- st_as_sfc(curent_polygon) %>% st_set_crs(4326)
    sf_curent_buffered <- polygon_bufferisation(sf_curent_polygon, input$buffer_radius)
    
    map %>% addPolygons(data = sf_curent_buffered, layerId = id,   # add buffered polygon to map
                        options = buffered_polygon_options
    )
    
    curent_buffered_WKT <- st_as_text(sf_curent_buffered) # it work
    reaktive_bufered_polygon_wkt(curent_buffered_WKT)    # write curent_buffered_WKT in my custom global reactive value
    
  })
  
  # Delete buffered polygon with source polygon
  observeEvent(input$map_draw_deleted_features, {
    clearShapes(map) # delet all shapes from map
    clearMarkers(map) # clean previously loaded markers
    reaktive_bufered_polygon_wkt("")    # write curent_buffered_WKT in my custom global reactive value
    # removeShape(map, id_to_del) # TODO it
  })
  
  
  # add bufered polygon on map
  # observe({
  #   observeEvent(
  #     map %>% addPolygons(data = buffered_polygon())
  #     )
  # })

  species_list <- eventReactive(eventExpr = input$redbook_finder,
                                  valueExpr = {
                                    vectors_list <- list(red_book_vrazlyvyi, red_book_ridkisnyi,
                                                         red_book_znykaiuchyi, red_book_znyklyi_v_pryrodi,
                                                         red_book_znyklyi, red_book_nedostatno_vidomyi,
                                                         red_book_neotsinenyi)
                                    names(vectors_list) <- c("red_book_vrazlyvyi", "red_book_ridkisnyi",
                                                             "red_book_znykaiuchyi", "red_book_znyklyi_v_pryrodi",
                                                             "red_book_znyklyi", "red_book_nedostatno_vidomyi",
                                                             "red_book_neotsinenyi")
                                    spec_list <- vectors_list[input$redbook_finder]
                                    names(spec_list) <- NULL
                                    paste(unlist(spec_list))
                                  }
  )
  

  recieved_data <- eventReactive(eventExpr = input$act_get_gbif_data,
                                 valueExpr = {
                                   occ_search(scientificName = species_list(),
                                              return = "data",
                                              hasCoordinate = T,
                                              geometry = reaktive_bufered_polygon_wkt(),
                                              limit = query_limit)
                                 }
  )

  df_recieved_data <- reactive(rbindlist(lapply(recieved_data(), function(x) x$data), fill = TRUE, use.names = TRUE))



  # add request result to webmap
  observe({
    map %>%
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

  # render result of request in tab "Попередній перегляд"
  output$gbif_table <- DT::renderDataTable(df_recieved_data())
  
  
  ## Red book table ####
  output$redbook_table <- DT::renderDataTable(df_redbook)
  
  observe({   # применяется для доступа к реактивным переменным, распечатки их в консоль и отладки
    print("reaktive_bufered_polygon_wkt: ")
    print(reaktive_bufered_polygon_wkt())
    print("redbook_finder: ")
    print(input$redbook_finder)
    print("species_list: ")
    print(str(species_list()))
  	print(length(species_list()))
    # print("recieved_data: ")
    # print(str(recieved_data()))
	print("df_recieved_data: ")
    # print(str(df_recieved_data()))
    # print(head(df_recieved_data()))
    print(nrow(df_recieved_data()))
    # print("Map info: ")
    # print(getMapData(map))
    print("done")
	print(Sys.time())
  })
  
  
}



shinyApp(ui, server)
