# setwd("C:/Mamba/Work/Presentations/2023-03_GBIF_Viewer/all_23-07-03/gbif_shiny_onlineviewer-main")

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
# library(rgbif) # do we still need it?
library(leaflet)
library(leaflet.extras)
library(leafem)
library(DT)
library(data.table)

# Calling custom function
source("scripts/config.R")
source("functions/polygon_bufferisation.R")

# import data ####
## import spatial data ####
Ukr_Obl <- st_read("adm_shp/Ukr_Obl_NEW.shp")
Ukr_Rai <- st_read("adm_shp/Ukr_Raion_NEW.shp")
Ukr_OTG <- st_read("adm_shp/Ukr_OTG_NEW.shp")
Ukr_OTG <- Ukr_OTG[order(Ukr_OTG$OTG_name), ]         # to sort OTG names similarly
# Create vector with OTG
choices_OTG <- Ukr_OTG$shapeID
names(choices_OTG) <- Ukr_OTG$OTG_name
choices_OTG <- choices_OTG[order(names(choices_OTG))] # to sort OTG names similarly

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

# df_redbook <- dplyr::select(df_protected_status, all_of(c("base_name", "scientificName", "nameUk", "ЧКУ", "kingdom", "phylum", "class"))) %>% filter(ЧКУ != "NA")

## load prepared GBIF data ####
load(file = "data/gbif_sf_dataset.Rdata")

## Preparation of GBIF data -----
# Originally these steps have to be done in separate script and performed once a month after downloading

## Delete atlas data
gbif_sf_dataset <- subset(gbif_sf_dataset, gbif_sf_dataset$datasetName != "EBCC Atlas of European Breeding Birds")

## Lists of columns
# for full table (excel)
colnames_set1 <- c("gbifID", "bibliographicCitation", "identifier", "license", "publisher", "references",
                   "rightsHolder", "type", "institutionID", "collectionID", "datasetID", "institutionCode",
                   "collectionCode", "datasetName", "ownerInstitutionCode", "basisOfRecord", "informationWithheld",
                   "occurrenceID", "catalogNumber", "recordNumber", "recordedBy", "recordedByID", "individualCount",
                   "organismQuantity", "organismQuantityType", "sex", "lifeStage", "reproductiveCondition", "behavior",
                   "establishmentMeans", "degreeOfEstablishment", "georeferenceVerificationStatus", "occurrenceStatus",
                   "preparations", "disposition", "associatedReferences", "associatedSequences", "otherCatalogNumbers",
                   "occurrenceRemarks", "organismID", "organismScope", "materialSampleID", "eventID", "parentEventID",
                   "eventDate", "eventTime", "year", "month", "day", "verbatimEventDate", "habitat", "samplingProtocol",
                   "sampleSizeValue", "sampleSizeUnit", "samplingEffort", "fieldNotes", "eventRemarks", "locationID",
                   "waterBody", "locality", "verbatimLocality", "Latitude", "Longitude", "coordinateUncertaintyInMeters",
                   "footprintWKT", "identifiedBy", "dateIdentified", "taxonID", "acceptedNameUsageID",
                   "parentNameUsageID", "scientificName", "kingdom", "phylum", "class", "order", "family", "genus",
                   "genericName", "infragenericEpithet", "specificEpithet", "infraspecificEpithet", "taxonRank",
                   "vernacularName", "taxonomicStatus", "datasetKey", "publishingCountry", "lastInterpreted", "issue",
                   "mediaType", "taxonKey", "acceptedTaxonKey", "kingdomKey", "phylumKey", "classKey", "orderKey",
                   "familyKey", "genusKey", "speciesKey", "species", "acceptedScientificName", "verbatimScientificName",
                   "typifiedName", "iucnRedListCategory")

# for reduced table (to show the output in the application)
colnames_set2 <- c("publisher", "datasetName", "recordedBy", "eventDate", "Latitude", "Longitude", "kingdom",
                   "phylum", "class", "order", "family", "species")
## End Preparation of GBIF data -----

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
                           pickerInput('regions', 'Оберіть область', sort(unique(Ukr_Obl$obl_name)),
                                       selected = c(unique(Ukr_Obl$obl_name)),
                                       options = list(`actions-box` = TRUE), multiple = T),
                           ## Raion selection ####
                           pickerInput('raions', 'Оберіть район',
                                       unique(Ukr_Rai$raion_name),
                                       selected = NULL,
                                       options = list(`actions-box` = TRUE), multiple = T),
                           ## OTG selection ####
                           pickerInput('OTG', 'Оберіть територіальну громаду',
                                       choices = choices_OTG,
                                       selected = NULL,
                                       options = list(`actions-box` = TRUE), multiple = T),
                           ## Upload custom contour ####
                          fileInput("userContours", "Завантажте власний kml/kmz файл",
                                                  multiple = F,
                                                  accept = c('.kml','.kmz')),
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
                           
                           ## Button for sending GBIF request ####
                           actionButton("act_get_gbif_data", label = "Отримати GBIF дані"),
                           
                         ),
                         #### Main map panel for displaying outputs ####
                         mainPanel(
                           leafletOutput("map",  width = "100%", height="85vh"),
                         )
                       )
              ),
              ## Tab - Попередній перегляд ####
              tabPanel("Попередній перегляд",
                       DT::dataTableOutput("gbif_table_set2")
              ),
              ## Tab - Генерування звітів ####              
              tabPanel("Генерування звітів",
                       
              ),
              # ## Tab Червона Книга України - for testing ####              
              # tabPanel("Червона Книга України",
              #          # tableOutput("my_table")
              #          DT::dataTableOutput("redbook_table"),
              # )
  )
)

# Back end ####
server = function(input, output, session) {
  
  # Create a global reactive value
  ## Create a global reactive value for WKT guffered polygon
  reaktive_bufered_polygon <- reactiveVal() # Create a global reactive value for buffered polygon - now SF, not WKT!
  
  reaktive_dissolved_polygon <- reactiveVal()  # Create second reactive value for dissolved polygon - also SF
  
  ## create the leaflet map ####
  main_map <-  leaflet() %>% addTiles() %>%
    # addSearchOSM() %>% 
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
  obl <- reactive(subset(Ukr_Obl, Ukr_Obl$obl_name %in% input$regions))
  obl_bounds <- reactive(obl() %>% st_bbox() %>% as.character()) # to set extent around selected oblasts
  ## conditional selection of raions based on selected oblast ####
  observeEvent(input$regions, {
    clearShapes(map) # clean map
    clearMarkers(map) # clean previously loaded markers
    
    updatePickerInput(session = session, inputId = "raions",
                      choices = sort(subset(unique(Ukr_Rai$raion_name), Ukr_Rai$obl_name %in% input$regions)),
                      selected = NULL) # raions are shown but nothing is selected initially
    
    obl_geom <- st_geometry(obl()) # to extract geometry
    reaktive_bufered_polygon(obl_geom)
    
    obl_buffered <- polygon_bufferisation(obl(), input$buffer_radius)
    obl_buffered_geom <- st_geometry(obl_buffered) # to extract geometry
    reaktive_bufered_polygon(obl_buffered_geom)
    
	map %>%
      addPolygons(data = obl(), weight = 2, fill = F) %>%
      fitBounds(lng1 = obl_bounds()[1], lat1 = obl_bounds()[2], # set view by extent: p1 - top lext, p2 - bottom right
                lng2 = obl_bounds()[3], lat2 = obl_bounds()[4]) # extent is set after selection of oblast
	    #addPolygons(data = obl_buffered, #layerId = id,   # add buffered polygon to map
	     #         options = buffered_polygon_options)
  })
  
  ## create object with selected raion ##
  raion <- reactive(subset(Ukr_Rai, Ukr_Rai$raion_name %in% input$raions))
  raion_bounds <- reactive(raion() %>% st_bbox() %>% as.character()) # to set extent around selected raions
  ## change map based on selected raions ####
  observeEvent(input$raions, {
    clearShapes(map) # clean map
    clearMarkers(map) # clean previously loaded markers
    
    updatePickerInput(session = session, inputId = "OTG",
                      choices = subset(choices_OTG, Ukr_OTG$raion_name %in% input$raions),
                      selected = NULL)

    raion_buffered <- polygon_bufferisation(raion(), input$buffer_radius)
    raion_buffered_geom <- st_geometry(raion_buffered) # to extract geometry
    reaktive_bufered_polygon(raion_buffered_geom)    # write raion_buffered in my custom global reactive value
	
    map %>% 
      addPolygons(data = raion(), weight = 2, fill = F) %>%
      fitBounds(lng1 = raion_bounds()[1], lat1 = raion_bounds()[2],  # set view by extent: p1 - top lext, p2 - bottom right
                lng2 = raion_bounds()[3], lat2 = raion_bounds()[4]) %>%  # to allow map zoom to selected area
      addPolygons(data = raion_buffered, #layerId = id,   # add buffered polygon to map
                  options = buffered_polygon_options)
  })
  
  ## create object with selected OTG ##
  OTG <- reactive(subset(Ukr_OTG, Ukr_OTG$shapeID %in% input$OTG))
  OTG_bounds <- reactive(OTG() %>% st_bbox() %>% as.character()) # to set extent around selected raions
  ## change map based on selected raions ####
  observeEvent(input$OTG, {
    clearShapes(map) # clean map
    clearMarkers(map) # clean previously loaded markers
    
    OTG_buffered <- polygon_bufferisation(OTG(), input$buffer_radius)
    OTG_buffered_geom <- st_geometry(OTG_buffered) # to extract geometry
    reaktive_bufered_polygon(OTG_buffered_geom)    # write raion_buffered in my custom global reactive value
    
    map %>% 
      addPolygons(data = OTG(), weight = 2, fill = F) %>%
      fitBounds(lng1 = OTG_bounds()[1], lat1 = OTG_bounds()[2],  # set view by extent: p1 - top lext, p2 - bottom right
                lng2 = OTG_bounds()[3], lat2 = OTG_bounds()[4]) %>%  # to allow map zoom to selected area
      addPolygons(data = OTG_buffered, #layerId = id,   # add buffered polygon to map
                  options = buffered_polygon_options)
  })
  
  ## Adding custom contours ## 
  observeEvent(input$userContours, {
    clearShapes(map) # clean map
    clearMarkers(map) # clean previously loaded markers
    
    # read spatial object
    uploaded_cont <-  st_read(input$userContours$datapath)
    
    uploaded_cont_buffered <- polygon_bufferisation(uploaded_cont, input$buffer_radius)
    uploaded_cont_buffered_geom <- st_geometry(uploaded_cont_buffered) # to extract geometry
    reaktive_bufered_polygon(uploaded_cont_buffered_geom)    # write raion_buffered in my custom global reactive value
    
    # calculate bounds
    uploaded_cont_bounds <- uploaded_cont_buffered %>% st_bbox() %>% as.character()
    
    # add to map
    map %>%
      addPolygons(data = uploaded_cont, weight = 2, fill = F) %>%
      fitBounds(lng1 = uploaded_cont_bounds[1], lat1 = uploaded_cont_bounds[2],
                lng2 = uploaded_cont_bounds[3], lat2 = uploaded_cont_bounds[4]) %>%
      addPolygons(data = uploaded_cont_buffered, #layerId = id,   # add buffered polygon to map
                  options = buffered_polygon_options)
  })
  
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
    sf_curent_buffered <- polygon_bufferisation(sf_curent_polygon, input$buffer_radius)
    
    map %>% addPolygons(data = sf_curent_buffered, layerId = id,   # add buffered polygon to map
                        options = buffered_polygon_options 
    )
    
    reaktive_bufered_polygon(sf_curent_buffered)    # write curent_buffered_WKT in my custom global reactive value
    
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
    
    reaktive_bufered_polygon(sf_curent_buffered)    # write curent_buffered_WKT in my custom global reactive value
    
  })
  
  # Delete buffered polygon with source polygon
  observeEvent(input$map_draw_deleted_features, {
    clearShapes(map) # delet all shapes from map
    clearMarkers(map) # clean previously loaded markers
    
	reaktive_bufered_polygon("")    # write curent_buffered_WKT in my custom global reactive value
    # removeShape(map, id_to_del) # TODO it
  })
  
  observeEvent(input$act_get_gbif_data, {
    resulted_polygon <- reaktive_bufered_polygon() %>%
      st_union() # dissolve all created polygons to avoid overlaps
    
    reaktive_dissolved_polygon(resulted_polygon)
    
    clearShapes(map) # delet all shapes from map
    
    map %>%
      addPolygons(
        data = reaktive_dissolved_polygon(), #layerId = id,   # add buffered polygon to map
        options = buffered_polygon_options
      )
  })
  
  
  recieved_data <- eventReactive(
    eventExpr = input$act_get_gbif_data,
    valueExpr = {
      st_intersection(gbif_sf_dataset, reaktive_dissolved_polygon()) # use dissolved polygon for occurrence search
    }
  )

  # add request result to webmap
  observe({
    map %>%
      addCircleMarkers(
        data = recieved_data(), lng = ~Longitude, lat = ~Latitude, # add circle markers from dataframe
        # radius = 10,  # static radius
        radius = 2,  # calculation radius
        color = "red",
        popup = ~species # simple popup from field "scientificName"
        # popup = ~paste0("<center>" ,"<b>", scientificName, "</b>", "</center>", "<br>",   # popup with HTML
        #                 "Население: ", population, " чел." )
      )
  }
  )

  # render result of request in tab "Попередній перегляд"
  output$gbif_table_set2 <- DT::renderDataTable(recieved_data()[, colnames_set2])

  # to create larger table for excel output
  output$gbif_table_set1 <- DT::renderDataTable(recieved_data()[, colnames_set1])
  
  ## Red book table ####
  # output$redbook_table <- DT::renderDataTable(df_redbook)
  
  observe({   # применяется для доступа к реактивным переменным, распечатки их в консоль и отладки
    print("reaktive_bufered_polygon: ")
    print(reaktive_bufered_polygon())
    print(paste("class: ", class(reaktive_bufered_polygon()), sep=""))
    print("Dissolved polygon: ")
    print(reaktive_dissolved_polygon())
    print(paste("class: ", class(reaktive_dissolved_polygon()), sep=""))
    print("recieved_data sample: ")
    print(head(select(recieved_data(), recordedBy, eventDate, scientificName)))
    # print("redbook_finder: ")
    # print(input$redbook_finder)
    # print("species_list: ")
    # print(str(species_list()))
    # print(length(species_list()))
    # print("recieved_data: ")
    # print(str(recieved_data()))
    print(paste("number of observations: ", nrow(recieved_data()), sep=""))
    # print(str(df_recieved_data()))
    # print(head(df_recieved_data()))
    # print("Map info: ")
    # print(getMapData(map))
    print("done")
    print(Sys.time())
  })
  
  
}


shinyApp(ui, server)
