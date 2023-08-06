library(shiny)
library(leaflet)
library(leaflet.extras)
library(leafem)
library(sf)

# CONFIG Start ####

CRS_used_in_calculations <- 3537 # WGS 84 / North Pole LAEA Europe

buffer_choices = c(
  "немає" = 0,
  "1 км" = 1000,
  "5 км" = 5000,
  "10 км" = 10000,
  "20 км" = 20000
)

# draw_new_shape_options
draw_new_shape_options <- drawShapeOptions(
  # clickable = TRUE,
  weight = 1,
  opacity = 0.8,
  fillOpacity = 0.3,
  color = '#ff0000',
  fillColor = 'blue')

polygon_aoi_options <- pathOptions(color = "#ff0000", weight = 2, opacity = 0.9, fill = FALSE, zIndexOffset = 12)

buffered_polygon_options <- pathOptions(color = "#03F", weight = 2, opacity = 0.9, fill = FALSE, zIndexOffset = 10) 

# CONFIG End

# custom functions start ####
# polygon_bufferisation

# polygon_bufferisation(input_polygon, radius)

polygon_bufferisation <- function(sf_input_polygon, radius){
  sf_polygon_buffered <- st_transform(sf_input_polygon, CRS_used_in_calculations) %>%
    st_buffer(dist = as.numeric(radius), nQuadSegs = 4) %>%
    st_transform(4326) %>% 
    st_union()
  
  return(sf_polygon_buffered)
}

leaf_draw_sf_polyg <- function(geometry_coordinates){
  coords <- geometry_coordinates %>% unlist %>% matrix(nc = 2, byrow = T)
  curent_polygon <- sp::Polygon(coords) %>% list %>% sp::Polygons(ID=1) %>% list %>% sp::SpatialPolygons()
  sf_curent_polygon <- st_as_sfc(curent_polygon) %>% st_set_crs(4326) # it work

  return(sf_curent_polygon)
}

# custom functions end

# Shiny app ####
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
                           ## Select buffer radius ####
                           radioButtons(
                             inputId = "buffer_radius",
                             label = "Буфер довкола області інтересу",
                             choices = buffer_choices,
                              selected = NULL
                             )
                         ),
                         mainPanel(
                           leafletOutput("map",  width = "100%", height="85vh"),
                           )
                       )
              )
           )
      )
                          
server = function(input, output, session) {
  # Create a global reactive value
  ## Create a global reactive value for AOI polygon
  reaktive_aoi_polygon <- reactiveVal()
  
  ## Create a global reactive value for guffered polygon
  reaktive_bufered_polygon <- reactiveVal() # Create a global reactive value for buffered polygon 
  
  ## create the leaflet map ####
  main_map <-  leaflet() %>% addTiles() %>%
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
  
  output$map <- renderLeaflet({ main_map })
  
  # create map proxy to make further changes to existing map
  map <- leafletProxy("map", session)
  
  observeEvent(input$map_draw_new_feature, {
    clearShapes(map) # clean map
    clearMarkers(map) # clean previously loaded markers
    # Leaflet ID to edit
    # id = input$map_draw_new_feature$properties$"_leaflet_id"
    # print("leaflet_id")
    # print(id)
    # coords <- input$map_draw_new_feature$geometry$coordinates %>% unlist %>% matrix(nc = 2, byrow = T)
    # curent_polygon <- sp::Polygon(coords) %>% list %>% sp::Polygons(ID=1) %>% list %>% sp::SpatialPolygons()
    # sf_curent_polygon <- st_as_sfc(curent_polygon) %>% st_set_crs(4326) # it work
   
    sf_curent_polygon <- leaf_draw_sf_polyg(input$map_draw_new_feature$geometry$coordinates)
 
    reaktive_aoi_polygon(sf_curent_polygon)
    
    map %>% addPolygons(data = sf_curent_polygon, #  layerId = id,   # add buffered polygon to map
                        options = polygon_aoi_options )
   
     updateRadioButtons(session = session, inputId = "buffer_radius",
                       choices = buffer_choices,
                       selected = NULL)
  })
  
  observeEvent(input$map_draw_edited_features, {
    clearShapes(map) # clean map
    clearMarkers(map) # clean previously loaded markers
    
    # generate new buffer
    # coords <- input$map_draw_edited_features$features[[1]]$geometry$coordinates %>% unlist %>% matrix(nc = 2, byrow = T)
    # curent_polygon <- sp::Polygon(coords) %>% list %>% sp::Polygons(ID=1) %>% list %>% sp::SpatialPolygons()
    # sf_curent_polygon <- st_as_sfc(curent_polygon) %>% st_set_crs(4326)
    
    sf_curent_polygon <- leaf_draw_sf_polyg(input$map_draw_edited_features$features[[1]]$geometry$coordinates)
    
    reaktive_aoi_polygon(sf_curent_polygon)
    
    map %>% addPolygons(data = sf_curent_polygon, # layerId = id,   # add buffered polygon to map
                        options = polygon_aoi_options )
    
    updateRadioButtons(session = session, inputId = "buffer_radius",
                      choices = buffer_choices,
                      selected = NULL)
    
  })
  

  
  
  # Delete buffered polygon with source polygon
  observeEvent(input$map_draw_deleted_features, {
    clearShapes(map) # delet all shapes from map
    clearMarkers(map) # clean previously loaded markers
    reaktive_aoi_polygon("")
    reaktive_bufered_polygon("")    
    # removeShape(map, id_to_del) # TODO it
    updateRadioButtons(session = session, inputId = "buffer_radius",
                       choices = buffer_choices,
                       selected = NULL)
  })
  
  
  
  observeEvent(input$buffer_radius, {

    # if ( (class(reaktive_aoi_polygon())[2] == "sfc") )  { # TODO Add an explicit object type check. If the object type is not SFC then print this error to the console.
    if (!(is.na(class(reaktive_aoi_polygon())[2] ) ) ) {

      reaktive_bufered_polygon(polygon_bufferisation(reaktive_aoi_polygon(), input$buffer_radius))
      
      clearShapes(map) # delet all shapes from map
      clearMarkers(map) # clean previously loaded markers
      
      map %>% addPolygons(data = reaktive_aoi_polygon(), # layerId = id,   # add  polygon to map
                          options = polygon_aoi_options )

      map %>% addPolygons(data = reaktive_bufered_polygon(), # layerId = id,   # add buffered polygon to map
                        options = buffered_polygon_options )

    } # else { print(" reaktive_aoi_polygon isn't SFC object")  }

    }
  )

  
  observe({   # применяется для доступа к реактивным переменным, распечатки их в консоль и отладки
    print("reaktive_aoi_polygon(): ")
    print(reaktive_aoi_polygon())
    print("class reaktive_aoi_polygon(): ")
    print(class(reaktive_aoi_polygon()) )
    print("class class reaktive_aoi_polygon(): ")
    print(class(class(reaktive_aoi_polygon()) ))
    print("class 1 reaktive_aoi_polygon(): ")
    print(class(reaktive_aoi_polygon())[2] )
    print(Sys.time())
  })
  
}

shinyApp(ui, server)

