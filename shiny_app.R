# Biodiversity Viewer v.0.1

# use https://mastering-shiny.org/action-layout.html
# https://shiny.rstudio.com/gallery/viscover.html
# https://github.com/XiaodanLyu/viscover/blob/master/inst/shiny-examples/overlay/app.r

# Import libs ####
# library(tidyverse)
library(dplyr)
library(shiny)
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

# TODO add function polygon_bufferisation(polygon, radius)

# Calling custom function
source("scripts/config.R")

# import data ####
adm_2 <- st_read("./regions/adm_2.shp")
## load Red lists ----
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
                           
                           # Butten for send GBIF request
                           submitButton("Побудувати буфер"),
                           br(),
                           actionButton("act_get_gbif_data", label = "Отримати GBIF дані"),#  Активная кнопка
                           # Кнопка подтверждения
                           ),
  #### Main map panel for displaying outputs ####
                         mainPanel(
                           leafletOutput("map",  width = "100%", height="85vh"),
                           )
                         )
                       ),
  ## Tab - Попередній перегляд ####
              tabPanel("Попередній перегляд", 
                       
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

# Beckend ####
server = function(input, output, session) {
  
  reaktive_bufered_polygon_wkt <- reactiveVal() # Create a (single) reactive value which can see all time (global variables)
  
  ## create the leaflet map ####
  main_map <- leaflet() %>% addTiles() %>% addSearchOSM() %>% setView(36.39, 49.65, zoom = 11) %>%
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
      
    ) #  %>% addPolygons(data = adm_2)

  output$map <- renderLeaflet({ main_map })

  # create map proxy to make further changes to existing map
  map <- leafletProxy("map", session)
  
  
  
  ## Draw polygon ####
  aoi_poly <- eventReactive(input$map_draw_new_feature,{
    coords <- input$map_draw_new_feature$geometry$coordinates %>% unlist %>% matrix(nc = 2, byrow = T)
    sp_aoi_polygon <- sp::Polygon(coords) %>% list %>% sp::Polygons(ID=1) %>% list %>% sp::SpatialPolygons()
    sf_aoi_polygon <- st_as_sfc(sp_curent_polygon) %>% st_set_crs(4326)
    # sf_aoi_buffered <- st_transform(sf_aoi_polygon, CRS_used_in_calculations) %>%
    #   st_buffer(dist = buff_radius, nQuadSegs = 4) %>%
    #   st_transform(4326)
    # aoi_WKT <- st_as_text(sf_aoi_buffered)
  })
  
  observeEvent(input$map_draw_new_feature, {
    clearShapes(map) # clean map
    # Leaflet ID to edit
    id = input$map_draw_new_feature$properties$"_leaflet_id"
    print("leaflet_id")
    print(id)
    # print(input$map_draw_new_feature$geometry$coordinate)
    # print(input$map_draw_new_feature$geometry$coordinates %>% unlist %>% matrix(nc = 2, byrow = T))
    coords <- input$map_draw_new_feature$geometry$coordinates %>% unlist %>% matrix(nc = 2, byrow = T)
    # print(coords)
    # print(str(coords))
    curent_polygon <- sp::Polygon(coords) %>% list %>% sp::Polygons(ID=1) %>% list %>% sp::SpatialPolygons()
    # print(str(curent_polygon))
    # print(curent_polygon)
    sf_curent_polygon <- st_as_sfc(curent_polygon) %>% st_set_crs(4326) # it work
    curent_polygon_WKT <- st_as_text(sf_curent_polygon) # it work
    
    sf_curent_buffered <- st_transform(sf_curent_polygon, CRS_used_in_calculations) %>%
      # st_buffer(dist = 5000, nQuadSegs = 4) %>%
      st_buffer(dist = as.numeric(input$buffer_radius), nQuadSegs = 4) %>%
      st_transform(4326)

    map %>% addPolygons(data = sf_curent_buffered, layerId = id,   # add buffered polygon to map
                         options = buffered_polygon_options 
                        )
    curent_buffered_WKT <- st_as_text(sf_curent_buffered) # it work
    reaktive_bufered_polygon_wkt(curent_buffered_WKT)    # write curent_buffered_WKT in my custom global reactive value

    # 
    # 
    # aoi_WKT <- st_as_text(st_as_sfc(curent_polygon)) # it work
    # print(curent_polygon_WKT)
    # print(curent_buffered_WKT)
  })
  
  observeEvent(input$map_draw_edited_features, {
    clearShapes(map) # clean map
    # generate new buffer
    coords <- input$map_draw_edited_features$features[[1]]$geometry$coordinates %>% unlist %>% matrix(nc = 2, byrow = T)
    curent_polygon <- sp::Polygon(coords) %>% list %>% sp::Polygons(ID=1) %>% list %>% sp::SpatialPolygons()
    sf_curent_polygon <- st_as_sfc(curent_polygon) %>% st_set_crs(4326)
    sf_curent_buffered <- st_transform(sf_curent_polygon, CRS_used_in_calculations) %>%
      # st_buffer(dist = 5000, nQuadSegs = 4) %>%
      st_buffer(dist = as.numeric(input$buffer_radius), nQuadSegs = 4) %>%
      st_transform(4326)

    map %>% addPolygons(data = sf_curent_buffered, layerId = id,   # add buffered polygon to map
                        options = buffered_polygon_options
    )
    
    curent_buffered_WKT <- st_as_text(sf_curent_buffered) # it work
    reaktive_bufered_polygon_wkt(curent_buffered_WKT)    # write curent_buffered_WKT in my custom global reactive value
    
  })
  
  # Delete buffered polygon with source polygon
  observeEvent(input$map_draw_deleted_features, {
    clearShapes(map) # delet all shapes from map
    reaktive_bufered_polygon_wkt("")    # write curent_buffered_WKT in my custom global reactive value
    # removeShape(map, id_to_del) # TODO it
  })
  
  
  
  
  
  # add bufered polygon on map
  
  # # observe({
  # observeEvent(
  #   
  # map %>% addPolygons(data = buffered_polygon())
  # })
  
  
  ## Red book table ####
  output$redbook_table <- DT::renderDataTable(df_redbook)
  
  observe({   # применяется для доступа к реактивным переменным, распечатки их в консоль и отладки
    # print(input$redbook_finder)
    # print(input$map_center)
    print("reaktive_bufered_polygon_wkt: ")
    print(reaktive_bufered_polygon_wkt())
    print("done")
  })
  
  
}



shinyApp(ui, server)