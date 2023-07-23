# setwd("C:/Mamba/Work/Presentations/2023-03_GBIF_Viewer/all_23-07-03/gbif_shiny_onlineviewer-main")

# Biodiversity Viewer v.0.1

# use https://mastering-shiny.org/action-layout.html
# https://shiny.rstudio.com/gallery/viscover.html
# https://github.com/XiaodanLyu/viscover/blob/master/inst/shiny-examples/overlay/app.r
gc()
rm(list = ls())

# Import libs ####
# library(tidyverse)
library(dplyr)
require(ggplot2)
library(basemapR)			 
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

library(openxlsx)
library(openxlsx2)
library(rmarkdown)
library(knitr) # to render table in docx
library(lubridate) # for paring date to GBIF cite						  

# Calling custom function
source("scripts/config.R")
source("functions/polygon_bufferisation.R")

## load prepared GBIF data ####
load(file = "data/gbif_dataset_metadata.Rdata")
load(file = "data/gbif_sf_dataset.Rdata")

# Lists of columns
## for full table (excel)
colnames_set1 <- c(
  "gbifID",
  # "bibliographicCitation", "identifier", "license", 
  "publisher", 
  # "references",  "rightsHolder", "type", "institutionID", "collectionID", "datasetID", 
  # "institutionCode", "collectionCode", "datasetName", "ownerInstitutionCode", 
  "basisOfRecord", 
  # "informationWithheld",
  # "occurrenceID", "catalogNumber", "recordNumber", "recordedBy", "recordedByID", "individualCount",
  # "organismQuantity", "organismQuantityType", "sex", "lifeStage", "reproductiveCondition", "behavior",
  # "establishmentMeans", "degreeOfEstablishment", "georeferenceVerificationStatus", "occurrenceStatus",
  # "preparations", "disposition", "associatedReferences", "associatedSequences", "otherCatalogNumbers",
  # "occurrenceRemarks", "organismID", "organismScope", "materialSampleID", "eventID", "parentEventID",
  # "eventDate", "eventTime", 
  "year", 
  # "month", "day", 
  "verbatimEventDate", 
  # "habitat", "samplingProtocol",
  # "sampleSizeValue", "sampleSizeUnit", "samplingEffort", "fieldNotes", "eventRemarks", "locationID",
  # "waterBody", "locality", 
  "verbatimLocality", 
  "Latitude", "Longitude", "coordinateUncertaintyInMeters", "issue",
  # "footprintWKT", "identifiedBy", "dateIdentified", 
  "taxonID", "acceptedNameUsageID",
  # "parentNameUsageID", 
  "scientificName", "kingdom", "phylum", "class", "order", "family", "genus",
  "genericName", "infragenericEpithet", "specificEpithet", "infraspecificEpithet", "taxonRank",
  "vernacularName", "taxonomicStatus", "datasetKey", "publishingCountry",
  "lastInterpreted", # "issue",
  # "mediaType",
  "taxonKey", "acceptedTaxonKey", "kingdomKey", "phylumKey", "classKey", "orderKey",
  "familyKey", "genusKey", "speciesKey", "species", "acceptedScientificName", "verbatimScientificName",
  "typifiedName", 
  "iucnRedListCategory",
  "nameUk", "BernAppendix2", "BernAppendix3", "Bonn", "AEWA", 
  # "IUCN", 
  "BernResolution6", "ЧКУ", 
  "BernAppendix1", "CITES", "EUROBATS",      
  "ACCOBAMS", "BirdsDirective", "HabitatsDirective",
  "Invasive", "ЧС_Полтавська", "ЧС_Чернівецька", 
  "ЧС_Житомирська", "ЧС_Вінницька", "ЧС_Харківська", 
  "ЧС_Чернігівська", "ЧС_Черкаська", "ЧС_Івано_Франківська",
  "ЧС_Рівненська", "ЧС_Одеська", "ЧС_Сумська",    
  "ЧС_Закарпатська", "ЧС_Львівська", "ЧС_Миколаївська", 
  "ЧС_Донецька", "ЧС_Херсонська", "ЧС_Севастополь", 
  "ЧС_Тернопільська", "ЧС_Київ", "ЧС_Волинська",  
  "ЧС_Хмельницька", "ЧС_Запорізька", "ЧС_Кіровоградська",
  "ЧС_Луганська", "ЧС_Київська", "ЧС_Дніпропетровська",
  "matchType", "confidence", "status", "rank"
)

## for reduced table (to show the output in the application)
colnames_set2 <- c(
  "nameUk", "species",
  "year", "Latitude", "Longitude", "kingdom",
  "phylum", "class", "order", "family", 
  "publisher"  )

## for reduced table (to instal to DOCX report)
colnames_set3 <- c("scientificName", "nameUk", "kingdom") 


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

# Dataset DOI
gbif_doi <- gbif_dataset_metadata$doi
gbif_doi_url <- paste0("https://doi.org/", gbif_doi)
string_gbif_doi <- paste0("DOI:", gbif_doi)

# Dataset Date
gbif_dataset_date <- gbif_dataset_metadata$created
parsed_gbif_dataset_date <- ymd_hms(gbif_dataset_date)
formatted_gbif_dataset_date <- format(parsed_gbif_dataset_date, "%d %B %Y")


## Preparation of GBIF data -----
# Originally these steps have to be done in separate script and performed once a month after downloading

## Delete atlas data end select collumns by list
gbif_sf_dataset <- subset(gbif_sf_dataset, gbif_sf_dataset$datasetName != "EBCC Atlas of European Breeding Birds" , select = colnames_set1)


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
                       ),
                       p("GBIF Viewer: an open web-based biodiversity conservation decision-making tool for policy and governance. Спільний проєкт The Habitat Foundation та Української Природоохоронної Групи, за підтримки NLBIF: The Netherlands Biodiversity Information Facility, nlbif2022.014",
                         # class = "footer-class"
                         )
              ),
              ## Tab - Попередній перегляд ####
              tabPanel("Попередній перегляд га мапі",
                       # TODO gbif_table_set2 >> gbif_table_set3
                       sidebarLayout(
                         sidebarPanel(
                           p("Натисніть кнопку <Застосувати фільтри>"),
                           textOutput("nrow_filtred_data_map"),
                           pickerInput("iucn", "IUCN Red List",
                                       # choices = unique(data$iucnRedListCategory),
                                       choices = c(
                                         "Вимерлий (Extinct, EX)" = "EX",
                                         "Вимерлий у природі (Extinct in the Wild, EW)" = "EW",
                                         "У критичній небезпеці (Critically Endangered, CR)" = "CR",
                                         "Зникаючий (Endangered, EN)" = "EN",
                                         "Уразливий (Vulnerable, VU)" = "VU",
                                         "Майже під загрозою (Near Threatened, NT)" = "NT",
                                         "Найменша осторога (Least Concern, LC)" = "LC",
                                         "Відомостей недостатньо (Data Deficient, DD)" = "DD",
                                         "Неоцінений (Not Evaluated, NE)" = "NE"
                                       ),
                                       selected = c("EX", "EW", "CR", "EN", "VU", "NT", "LC", "DD", "NE"),
                                       options = list(`actions-box` = TRUE), multiple = T
                           ),
                           pickerInput("redbook", "Червона Книга України",
                                       choices = c("вразливий", "рідкісний", "зникаючий", "неоцінений", "недостатньо відомий", "зниклий у природі"),
                                       selected = c("вразливий", "рідкісний", "зникаючий", "неоцінений", "недостатньо відомий", "зниклий у природі"),
                                       options = list(`actions-box` = TRUE), multiple = T
                           ),
                           checkboxInput("bern1", "Bern Appendix 1", TRUE),
                           checkboxInput("bern2", "Bern Appendix 2", TRUE),
                           checkboxInput("bern3", "Bern Appendix 3", TRUE),
                           checkboxInput("bern6", "Bern Resolution 6", TRUE),
                           checkboxInput("bonn", "Bonn", TRUE),
                           checkboxInput("aewa", "AEWA", TRUE),
                           checkboxInput("cites", "CITES", TRUE),
                           checkboxInput("eurobats", "EUROBATS", TRUE),
                           checkboxInput("accobams", "ACCOBAMS", TRUE),
                           checkboxInput("birdsdirective", "Birds Directive", TRUE),
                           checkboxInput("habitatsdirective", "Habitats Directive", TRUE),
                           hr(),
                           checkboxInput("invasive", "Інвазивні/інвазійні/чужорідні види", FALSE),
                           actionButton("refresh_filters", "Застосувати фільтри", icon("refresh"), class = "btn-success"),
                           
                         ),
                         
                         
                         mainPanel(
                           leafletOutput("map2",  width = "100%", height="85vh"),
                         )
                       ),
                       p("GBIF Viewer: an open web-based biodiversity conservation decision-making tool for policy and governance. Спільний проєкт The Habitat Foundation та Української Природоохоронної Групи, за підтримки NLBIF: The Netherlands Biodiversity Information Facility, nlbif2022.014",
                         # class = "footer-class"
                       )
                       
              ),
              ## Tab - Попередній перегляд ####
              tabPanel("Попередній перегляд таблиці даних",
			           tags$br(),
                       ## Button for send GBIF request ####
                       downloadButton("downloadData_CSV", "Download CSV"),
                       downloadButton("downloadData_XLSX", "Download XLSX"),
                       textOutput("nrow_filtred_data_tab"),
                       tags$hr(),
                       DT::dataTableOutput("gbif_table_set2"), # TODO gbif_table_set3
					   p("GBIF Viewer: an open web-based biodiversity conservation decision-making tool for policy and governance. Спільний проєкт The Habitat Foundation та Української Природоохоронної Групи, за підтримки NLBIF: The Netherlands Biodiversity Information Facility, nlbif2022.014",
                         # class = "footer-class"
                       )
              ),
              ## Tab - Генерування звітів ####              
              tabPanel("Генерування звітів",
                       tags$br(),
                       downloadButton("downloadData_DOCX", "Download DOCX"),
                       tags$hr(),
                       plotOutput("plot_map"),
                       tags$hr(),
                       textOutput("nrow_filtred_data_doc"),
                       # tags$br(),
                       DT::dataTableOutput("report_table"),
                       p("GBIF Viewer: an open web-based biodiversity conservation decision-making tool for policy and governance. Спільний проєкт The Habitat Foundation та Української Природоохоронної Групи, за підтримки NLBIF: The Netherlands Biodiversity Information Facility, nlbif2022.014",
                         # class = "footer-class"
                       )
              ),

  )
)

# Back end ####
server = function(input, output, session) {
  
  # Create a global reactive value
  ## Create a global reactive value for AOI polygon
  reaktive_aoi_polygon <- reactiveVal()
  
  ## Create a global reactive value for guffered polygon
  reaktive_bufered_polygon <- reactiveVal() # Create a global reactive value for buffered polygon - now SF, not WKT!
  
  # reaktive_dissolved_polygon <- reactiveVal()  # Create second reactive value for dissolved polygon - also SF
  
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
  
  main_map2 <-  leaflet() %>% addTiles()  %>% leafem::addMouseCoordinates()

  output$map2 <- renderLeaflet({
    main_map2
  })

  map2 <- leafletProxy("map2", session)
  
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
    reaktive_aoi_polygon(obl_geom)
    
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
    
    raion_geom <- st_geometry(raion()) # to extract geometry
    reaktive_aoi_polygon(raion_geom)
    
    raion_buffered <- polygon_bufferisation(raion(), input$buffer_radius) %>% 
	st_union() # dissolve all created polygons to avoid overlaps
    raion_buffered_geom <- st_geometry(raion_buffered) # to extract geometry
    reaktive_bufered_polygon(raion_buffered_geom)    # write raion_buffered in my custom global reactive value
	
    map %>% 
      addPolygons(data = raion(), options = polygon_aoi_options) %>%
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
    
    OTG_geom <- st_geometry(OTG()) # to extract geometry
    reaktive_aoi_polygon(OTG_geom)
    
    OTG_buffered <- polygon_bufferisation(OTG(), input$buffer_radius) %>% 
	st_union() # dissolve all created polygons to avoid overlaps
    OTG_buffered_geom <- st_geometry(OTG_buffered) # to extract geometry
    reaktive_bufered_polygon(OTG_buffered_geom)    # write raion_buffered in my custom global reactive value
    
    map %>% 
      addPolygons(data = OTG(), options = polygon_aoi_options) %>%
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
    
    uploaded_cont_geom <- st_geometry(uploaded_cont) # to extract geometry
    reaktive_aoi_polygon(uploaded_cont_geom)
    
    uploaded_cont_buffered <- polygon_bufferisation(uploaded_cont, input$buffer_radius) %>% 
	st_union() # dissolve all created polygons to avoid overlaps
    uploaded_cont_buffered_geom <- st_geometry(uploaded_cont_buffered) # to extract geometry
    reaktive_bufered_polygon(uploaded_cont_buffered_geom)    # write raion_buffered in my custom global reactive value
    
    # calculate bounds
    uploaded_cont_bounds <- uploaded_cont_buffered %>% st_bbox() %>% as.character()
    
    # add to map
    map %>%
      fitBounds(lng1 = uploaded_cont_bounds[1], lat1 = uploaded_cont_bounds[2],
                lng2 = uploaded_cont_bounds[3], lat2 = uploaded_cont_bounds[4]) %>%
      addPolygons(data = reaktive_aoi_polygon(), options = polygon_aoi_options) %>%
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
    reaktive_aoi_polygon("")
	  reaktive_bufered_polygon("")    
    # removeShape(map, id_to_del) # TODO it
  })
  
  
  
  observeEvent(input$act_get_gbif_data, {
    # resulted_polygon <- reaktive_bufered_polygon() # %>%
       # st_union() # dissolve all created polygons to avoid overlaps
    
    # reaktive_dissolved_polygon(resulted_polygon)
    
    # filteredData_bounds <- reaktive_dissolved_polygon %>% st_bbox() %>% as.character() 
    
    
    clearShapes(map) # delet all shapes from map
    # clearShapes(map2) # delet all shapes from map2
    
    
    map %>%
      addPolygons(
        data = reaktive_aoi_polygon(),
        options = polygon_aoi_options
      ) %>%
      addPolygons(
        # data = reaktive_dissolved_polygon(), #layerId = id,   # add buffered polygon to map
        data = reaktive_bufered_polygon(), #layerId = id,   # add buffered polygon to map
        options = buffered_polygon_options
      )
    
    # map2 %>%
    #   clearShapes() %>%
    #   clearMarkers() %>%
    #   fitBounds(
    #     # lng1 = data_bounds[1], lat1 = data_bounds[2], # set view by extent: p1 - top lext, p2 - bottom right
    #     # lng2 = data_bounds[3], lat2 = data_bounds[4]) %>% # extent is set after selection of oblast
    #     lng1 = filteredData_bounds[1], lat1 = filteredData_bounds[2], # set view by extent: p1 - top lext, p2 - bottom right
    #     lng2 = filteredData_bounds[3], lat2 = filteredData_bounds[4]) %>% # extent is set after selection of oblast
    #   addPolygons(
    #     data = reaktive_bufered_polygon(), #layerId = id,   # add buffered polygon to map
    #     options = buffered_polygon_options
    #   )
    
    
    
  })
  
  
  # recieved_data <- eventReactive(
  sf_clipped_data <- eventReactive(

    eventExpr = input$act_get_gbif_data,
    valueExpr = {
      # st_intersection(gbif_sf_dataset, reaktive_dissolved_polygon() ) # use dissolved polygon for occurrence search
      st_intersection(gbif_sf_dataset, reaktive_bufered_polygon() ) # use dissolved polygon for occurrence search
    }
  )

  # add request result to webmap
  observeEvent(input$act_get_gbif_data, {
  # observe({
    map %>%
      addCircleMarkers(
        data = sf_clipped_data(), # lng = ~Longitude, lat = ~Latitude, # add circle markers from dataframe
        # radius = 10,  # static radius
        radius = 2,  # calculation radius
        color = "red",
        popup = ~species # simple popup from field "scientificName"
        # popup = ~paste0("<center>" ,"<b>", scientificName, "</b>", "</center>", "<br>",   # popup with HTML
        #                 "Население: ", population, " чел." )
      )
    
    
  })
  

  
  # render result of request in tab "Попередній перегляд на мапі" ####
  
  sf_filteredData <- reactive({
    input$refresh_filters
    
    isolate(sf_clipped_data() %>%
              filter(iucnRedListCategory %in% input$iucn | 
                       ЧКУ %in% input$redbook | 
                       (input$bern1 & BernAppendix1 == "yes") |
                       (input$bern2 & BernAppendix2 == "yes") |
                       (input$bern3 & BernAppendix3 == "yes") |
                       (input$bern6 & BernResolution6 == "yes") |
                       (input$bonn & Bonn == "yes") |
                       (input$aewa & AEWA == "yes") |
                       (input$cites & CITES == "yes") |
                       (input$eurobats & EUROBATS == "yes") |
                       (input$accobams & ACCOBAMS == "yes") |
                       (input$birdsdirective & BirdsDirective == "yes") |
                       (input$habitatsdirective & HabitatsDirective == "yes") |
                       (input$invasive & Invasive == "yes")
              ) 
    )
  })
  
  filteredData_bounds <- reactive( sf_filteredData() %>% st_bbox() %>% as.character() )
  
  # filteredData_bounds <- reactive( reaktive_bufered_polygon() %>% st_bbox() %>% as.character() )
  
  
  

  
  observe({
    map2 %>%
      clearShapes() %>%
      clearMarkers() %>%
      fitBounds(
        # lng1 = data_bounds[1], lat1 = data_bounds[2], # set view by extent: p1 - top lext, p2 - bottom right
        # lng2 = data_bounds[3], lat2 = data_bounds[4]) %>% # extent is set after selection of oblast
      lng1 = filteredData_bounds()[1], lat1 = filteredData_bounds()[2], # set view by extent: p1 - top lext, p2 - bottom right
      lng2 = filteredData_bounds()[3], lat2 = filteredData_bounds()[4]) %>% # extent is set after selection of oblast
      addPolygons(
        data = reaktive_aoi_polygon(),
        options = polygon_aoi_options
      ) %>%
      addPolygons(
        # data = reaktive_dissolved_polygon(), #layerId = id,   # add buffered polygon to map
        data = reaktive_bufered_polygon(), #layerId = id,   # add buffered polygon to map
        options = buffered_polygon_options
      ) %>%
      addCircleMarkers(data = sf_filteredData(), 
                       radius = 2,
                       color = "red",
                       popup = ~paste0("<center>" ,"<b>", nameUk, "</b>", "</center>", # "<br>",   # popup with HTML 
                                       "<center>", scientificName, "</center>" )
      )
  })
  
  
  # render result of request in tab "Попередній перегляд таблиці даних" ####
  df_filteredData <- reactive(st_drop_geometry(sf_filteredData()))
  output$gbif_table_set2 <- DT::renderDataTable(df_filteredData()[, colnames_set2])
  ## Generate CSV 
  output$downloadData_CSV <- downloadHandler(
    filename = function() {
      paste("GBIF_data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(df_filteredData(), file)
    }
  )
  ## Generate XLSX
  output$downloadData_XLSX <- downloadHandler(
    filename = function() {
      paste("GBIF_data-", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      write_xlsx(df_filteredData(), file, colNames = TRUE) 
    }
  )
  
  string_nrow_filtred_data <- reactive(
    paste0("Кількість знахідок: ", toString(nrow(df_filteredData())) )
    )
  
  output$nrow_filtred_data_map <- renderText({
    string_nrow_filtred_data() 
  })
  
  output$nrow_filtred_data_tab <- renderText({
    string_nrow_filtred_data() 
  })
  
  output$nrow_filtred_data_doc <- renderText({
    string_nrow_filtred_data() 
  })
  
    
  
  ## Create DF_PREPRINT dataframe for Tab - Генерування звітів ####
  
  df_report_table <- reactive(df_filteredData() %>%
    dplyr::select(all_of(colnames_set3)) %>%
    group_by(scientificName,  # настроить корректно групбай чтоб не удаляло лишние поля 
             nameUk,
             kingdom #,
             # RedBookUA,
             # IUCN_Red_List
             ) %>%
    summarise(Amount = n()) %>%
    arrange(kingdom, scientificName) %>%
    dplyr::select(all_of(c("kingdom", "Amount", "nameUk", "scientificName"))) %>%
    # colnames(c("Царство", "Кількість", "Українська назва", "Латинська назва")) %>%
    na.omit()
  )
  
  # df_sorted_report_table <- reactive(df_report_table()[order(df_report_table$kingdom, df_report_table$scientificName), ]) # TODO it not work. Need to include sorting to previous step
  
  # Draw preview report table Генерування звітів
  output$report_table <- DT::renderDataTable(df_report_table())
  # output$report_table <- DT::renderDataTable(df_sorted_report_table())
  
    
  ## Create picture plot_map #### 
  
  output$plot_map = renderPlot({
    ggplot()+
      base_map(bbox = st_bbox(reaktive_bufered_polygon()),
               basemap = 'mapnik',
               increase_zoom = 2) +
      geom_sf(data=sf_filteredData(), aes(color=kingdom),size=2)+ # TODO сюда идут нефильтрованные данные. Возможно надо создать отдельную промежуточную переменную в которую будут писаться фильтрованные данные перед отправкой сюда
      scale_colour_manual(values = kingdom_colors, name=NULL ) +
      geom_sf(data = reaktive_bufered_polygon(), colour = "blue", fill=NA, lwd = 1)+
      geom_sf(data = reaktive_aoi_polygon(), colour = "red", fill = NA, lwd = 1)+ # TODO create global var for AOI
      theme_minimal()+
      theme(axis.text = element_blank())+
      theme(legend.position = "bottom",
            legend.margin=margin())+
      labs(caption = "Basemap attribution: © OpenStreetMap contributors")
  })
  
  # ggplot()+
  #   base_map(bbox = st_bbox(aoi_buffered), 
  #            basemap = 'mapnik', 
  #            increase_zoom = 2) +
  #   geom_sf(data=sf_points, aes(color=kingdom),size=2)+
  #   scale_colour_manual(values = kingdom_colors, name=NULL ) +
  #   geom_sf(data = aoi_buffered, colour = "blue", fill=NA, lwd = 1)+
  #   geom_sf(data = aoi_geometry, colour = "red", fill = NA, lwd = 1)+
  #   theme_minimal()+
  #   theme(axis.text = element_blank())+
  #   theme(legend.position = "bottom",
  #         legend.margin=margin())+
  #   labs(caption = "Basemap attribution: © OpenStreetMap contributors")
  # 
  # dev.off()
  
  ## Generate DOCX for download ####


  ## Generate document
  
    output$downloadData_DOCX <- downloadHandler(
    filename = function() {
      paste("GBIF_data-", Sys.Date(), ".docx", sep="")
    },
    content = function(file) {
      # Code to generate the DOCX file 
      rmarkdown::render(
        input = "templates/report.Rmd",
        output_format = "word_document",
        output_file = file,
        # params = list(df = df_report_table())
      )
    }
  )

  # to create larger table for excel output
  # output$gbif_table_set1 <- DT::renderDataTable(df_filteredData()[, colnames_set1])
  
  ## Red book table ####
  # output$redbook_table <- DT::renderDataTable(df_redbook)
  
  observe({   # применяется для доступа к реактивным переменным, распечатки их в консоль и отладки
    print("reaktive_bufered_polygon: ")
    print(reaktive_bufered_polygon())
    print(paste("class: ", class(reaktive_bufered_polygon()), sep=""))
    print("Dissolved polygon: ")
    # print(reaktive_dissolved_polygon())
    print(reaktive_bufered_polygon())
    # print(paste("class: ", class(reaktive_dissolved_polygon()), sep=""))
    print(paste("class: ", class(reaktive_bufered_polygon()), sep=""))
    # print("recieved_data sample: ")
    # print(head(select(recieved_data(), recordedBy, eventDate, scientificName)))
    # print("redbook_finder: ")
    # print(input$redbook_finder)
    # print("species_list: ")
    # print(str(species_list()))
    # print(length(species_list()))
    # print("recieved_data: ")
    # print(str(recieved_data()))
    # print(paste("number of observations: ", nrow(recieved_data()), sep=""))
    # print(str(df_recieved_data()))
    # print(head(df_recieved_data()))
    # print("Map info: ")
    # print(getMapData(map))
    print("done")
    print(Sys.time())
  })
  
  
}


shinyApp(ui, server)
