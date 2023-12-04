options(encoding = "UTF-8" )

# >>> Buffer building moved to a separate event. <<< 

# Biodiversity Viewer v.0.1

# use https://mastering-shiny.org/action-layout.html
# https://shiny.rstudio.com/gallery/viscover.html
# https://github.com/XiaodanLyu/viscover/blob/master/inst/shiny-examples/overlay/app.r
rm(list = ls())
gc()


# Import libs ####
# library(tidyverse)
library(dplyr)
require(ggplot2)
library(basemapR)
library(shiny)
library(shinyWidgets)
library(shinyalert)
library(shinyjs)
library(sf)
library(sp)
# library(dismo)
# library(rgbif) # do we still need it?
library(leaflet)
library(leaflet.extras)
library(leaflet.esri)
library(leafem)
library(DT)
library(data.table)

library(openxlsx)
library(openxlsx2)
library(rmarkdown)
library(tinytex)
library(knitr) # to render table in docx
library(lubridate) # for paring date to GBIF cite

# Calling custom settings and functions
source("config.R")
source("global_reactive_value.R")
source("custom_functions.R")

## load prepared GBIF data ####
load(file = path_metadata_datadump)
# load(file = path_datadump)

# load(url(url_metadata_datadump))
# options(timeout = 200) # in the case of slow download speed
# load(url(url_datadump))

# load dump from folder (temporary solution!!!)
# load(file = "../name_lookup/outputs/gbif_sf_dataset.Rdata")

# import data ####
## import spatial data ####
adm_1 <- st_read("adm_shp/adm_1.shp")
adm_2 <- st_read("adm_shp/adm_2.shp")
adm_3 <- st_read("adm_shp/adm_3.shp")
adm_3 <- adm_3[order(adm_3$adm_3_name), ]         # to sort OTG names similarly
# Create vector with OTG
choices_OTG <- adm_3$shapeID
names(choices_OTG) <- adm_3$adm_3_name
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

# ## Delete atlas data end select collumns by list
# gbif_sf_dataset <- subset(gbif_sf_dataset, gbif_sf_dataset$datasetName != "EBCC Atlas of European Breeding Birds" , select = colnames_set1)


## End Preparation of GBIF data -----

# Frontend ####

ui = fluidPage(
  shinyjs::useShinyjs(),
  tags$head(
    tags$style(type="text/css",
               "
        #loadmessage {
          position: fixed;
          top: 0px;
          left: 0px;
          width: 100%;
          height: 100%;
          display: flex;
          justify-content: center;
          align-items: center;
          background-color: rgba(0, 0, 0, 0.4);
          z-index: 105;
        }
        .loader {
          border: 16px solid #f3f3f3; /* Light grey */
          border-top: 16px solid #3498db; /* Blue */
          border-bottom: 16px solid green;
          border-radius: 50%;
          width: 80px;
          height: 80px;
          animation: spin 2s linear infinite;
        }
        @keyframes spin {
          0% { transform: rotate(0deg); }
          100% { transform: rotate(360deg); }
          }
        "
    )
  ),
  # App title 
  titlePanel("Biodiversity Viewer"),
  # Tabs
  tabsetPanel(type = "tabs",
              ## Tab main map ####
              tabPanel(txt_interface_tabs_map_title,
                       ### Sidebar layout with input and output definitions ####
                       sidebarLayout(
                         #### Sidebar panel for inputs ####
                         sidebarPanel(
                           ## Oblast selection ####
                           pickerInput('regions', txt_interface_tabs_map_choose_region, sort(unique(adm_1$adm_1_name)),
                                       selected = c(unique(adm_1$adm_1_name)),
                                       options = list(`actions-box` = TRUE), multiple = T),
                           ## Raion selection ####
                           pickerInput('raions', txt_interface_tabs_map_choose_subregion,
                                       unique(adm_2$adm_2_name),
                                       selected = NULL,
                                       options = list(`actions-box` = TRUE), multiple = T),
                           ## OTG selection ####
                           pickerInput('OTG', txt_interface_tabs_map_choose_commune,
                                       choices = choices_OTG,
                                       selected = NULL,
                                       options = list(`actions-box` = TRUE), multiple = T),
                           ## Upload custom contour ####
                          fileInput("userContours", txt_interface_tabs_map_upload_custom_file,
                                                  multiple = F,
                                                  accept = c('.kml','.kmz')),
                           ## Select buffer radius ####
                           radioButtons(
                             inputId = "buffer_radius",
                             label = txt_interface_tabs_map_draw_buffer,
                             choices = buffer_choices,
                             selected = 0 # NULL
                           ),
                           
                           ## Button for generate buffer ####
                           #submitButton("Побудувати / Видалити буфер"),
                           #br(),
                           
                           ## Button for sending GBIF request ####
                           actionButton("act_get_gbif_data", label = txt_interface_tabs_map_getdata_button),
                          
                          ## Place for messages ####
                          textOutput("message_Karta_sidebar"),
                           
                         ),
                         #### Main map panel for displaying outputs ####
                         mainPanel(
                           leafletOutput("map",  width = "100%", height="85vh"),
                         )
                       ),
                       p(txt_about_gbif_viewer_noFormat,
                         # class = "footer-class"
                         )
              ),
              ## Tab - Preview ####
              tabPanel(txt_interface_tabs_filter_title, # "Попередній перегляд на карті"
                       # TODO gbif_table_set2 >> gbif_table_set3
                       sidebarLayout(
                         sidebarPanel(
                           p(txt_interface_tabs_filter_comment), # TODO format this sting
                           textOutput("nrow_filtred_data_map"),
                           # dateRangeInput("dates", label = h3("Date range")), # Field: eventDate
                           # dateRangeInput("inDateRange", label = "Date range input:", 
                           #                start = min(na.omit(gbif_sf_dataset$eventDate)),
                           #                end = Sys.Date() ),
                           pickerInput("redbook", txt_interface_tabs_filter_reddatabookofukraine,
                                       choices = chku_category,
                                       selected = chku_category,
                                       options = list(`actions-box` = TRUE), multiple = TRUE
                           ),
                           pickerInput("iucn", txt_interface_tabs_filter_iucnrl,
                                       choices = txt_interface_tabs_filter_iucnrl_choices,
                                       selected = iucn_category_selected,
                                       options = list(`actions-box` = TRUE), multiple = TRUE
                           ),
                           pickerInput("international_filters", txt_interface_tabs_filter_international,
                                       choices = txt_interface_tabs_filter_international_choices,
                                       selected = vector_conventions,
                                       options = list(`actions-box` = TRUE), multiple = TRUE
                           ),
                           pickerInput("region_redlist_filters", txt_interface_tabs_filter_regionalrl,
                                       choices = txt_interface_tabs_filter_regionalrl_choices,
                                       # selected = vect_region_redlist,    # no choices selected by default
                                       options = list(`actions-box` = TRUE), multiple = TRUE
                           ),
                           hr(),
                           checkboxInput("invasive", invasive_alien_species, FALSE),
                           actionButton("refresh_filters", txt_apply_filters_button, icon("refresh"), class = "btn-success", disabled = ''),
                           
                         ),
                         
                         
                         mainPanel(
                           leafletOutput("map2",  width = "100%", height="85vh"),
                         )
                       ),
                       p(txt_about_gbif_viewer_noFormat
                         # class = "footer-class"
                       )
                       
              ),
              ## Tab - Попередній перегляд ####
              tabPanel(txt_interface_tabs_preview_title,
			           tags$br(),
                       ## Button for send GBIF request ####
                       downloadButton("downloadData_CSV", txt_interface_tabs_preview_downloadCSV_button),
                       downloadButton("downloadData_XLSX", txt_interface_tabs_preview_downloadXLSX_button),
                       textOutput("nrow_filtred_data_tab"),
                       tags$hr(),
                       DT::dataTableOutput("gbif_table_set2"), # TODO gbif_table_set3
					   p(txt_about_gbif_viewer_noFormat
                         # class = "footer-class"
                       )
              ),
              ## Tab - Генерування звітів ####              
              tabPanel(txt_interface_tabs_reports_title,
                       tags$br(),
                       # downloadButton("downloadData_DOCX", "Download DOCX"),
                       # # downloadButton("downloadData_PDF", "Download PDF"), # it don't work
                       # radioButtons("format", txt_interface_tabs_reports_docFormats_button, c('PDF', 'HTML', 'Word'), inline = TRUE),
                       radioButtons("format", txt_interface_tabs_reports_docFormats_button, c('HTML', 'Word'), inline = TRUE),
                       downloadButton('downloadReport'),
                       tags$hr(),
                       h2(txt_report_header),
                       em(txt_report_citation_instruction),
                       strong(textOutput("txt_doi_url") ),
                       plotOutput("plot_map"),
                       tags$hr(),
                       h2(txt_interface_tabs_reports_h2),
                       textOutput("nrow_filtred_data_doc"),
                       DT::dataTableOutput("report_rare_lists_table"),
                       tags$hr(),
                       tags$h3(textOutput("nrow_chku_doc")),
                       DT::dataTableOutput("report_chku_table"),
                       tags$br(),
                       tags$h3(textOutput("nrow_iucn_doc")),
                       DT::dataTableOutput("report_iucn_table"),
                       tags$br(),
                       tags$h3(textOutput("nrow_BernApp_1_doc")),
                       DT::dataTableOutput("report_BernApp_1_table"),
                       tags$br(),
                       tags$h3(textOutput("nrow_BernApp_2_doc")),
                       DT::dataTableOutput("report_BernApp_2_table"),
                       tags$br(),
                       tags$h3(textOutput("nrow_BernApp_3_doc")),
                       DT::dataTableOutput("report_BernApp_3_table"),
                       tags$br(),
                       tags$h3(textOutput("nrow_BernRes_6_doc")),
                       DT::dataTableOutput("report_BernRes_6_table"),
                       tags$br(),
                       tags$h3(textOutput("nrow_Bonn_doc")),
                       DT::dataTableOutput("report_Bonn_table"),
                       tags$br(),
                       tags$h3(textOutput("nrow_AEWA_doc")),
                       DT::dataTableOutput("report_AEWA_table"),
                       tags$br(),
                       tags$h3(textOutput("nrow_EUROBATS_doc")),
                       DT::dataTableOutput("report_EUROBATS_table"),
                       tags$br(),
                       tags$h3(textOutput("nrow_ACCOBAMS_doc")),
                       DT::dataTableOutput("report_ACCOBAMS_table"),
                       tags$br(),
                       tags$h3(textOutput("nrow_BirdDirAnn_I_doc")),
                       DT::dataTableOutput("report_BirdDirAnn_I_table"),
                       tags$br(),
                       tags$h3(textOutput("nrow_BirdDirAnn_II_doc")),
                       DT::dataTableOutput("report_BirdDirAnn_II_table"),
                       tags$br(),
                       tags$h3(textOutput("nrow_HabitatsDirAnn_II_doc")),
                       DT::dataTableOutput("report_HabitatsDirAnn_II_table"),
                       tags$br(),
                       tags$h3(textOutput("nrow_HabitatsDirAnn_IV_doc")),
                       DT::dataTableOutput("report_HabitatsDirAnn_IV_table"),
                       tags$br(),
                       tags$h3(textOutput("nrow_HabitatsDirAnn_V_doc")),
                       DT::dataTableOutput("report_HabitatsDirAnn_V_table"),
                       tags$br(),
                       tags$h3(textOutput("nrow_Invasive_doc")),
                       DT::dataTableOutput("report_Invasive_table"),
                       tags$br(),
                       tags$h3(textOutput("nrow_species_doc")),
                       # tags$br(),
                       DT::dataTableOutput("report_table"),
                       p(txt_about_gbif_viewer_noFormat
                         # class = "footer-class"
                       )
              )

  ),
  conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                   tags$div(id="loadmessage", 
                            tags$div(class="loader")
                            )
                   )
)

# Back end ####
server = function(input, output, session) {
  

  ## create the leaflet map ####
  main_map <-  leaflet() %>%
    addTiles(group = "OpenStreetMap") %>%
    addEsriBasemapLayer(esriBasemapLayers$Imagery,
                        autoLabels = TRUE,
                        group = "ESRI imagery with labels") %>%
    addLayersControl(
      baseGroups = c("ESRI imagery with labels", "OpenStreetMap")
    ) %>%
    # addSearchOSM() %>% 
    leafem::addMouseCoordinates() %>%
    addDrawToolbar(
      polylineOptions = FALSE,
      # polygonOptions = TRUE,
      polygonOptions = drawPolygonOptions(
        showArea = TRUE,
        repeatMode = F,
        shapeOptions = draw_new_shape_options
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
        remove = TRUE)
    )
  
  output$map <- renderLeaflet({
    main_map
  })
  
  # create map proxy to make further changes to existing map
  map <- leafletProxy("map", session)
  
  main_map2 <-  leaflet() %>%
    addTiles(group = "OpenStreetMap") %>%
    addEsriBasemapLayer(esriBasemapLayers$Imagery,
                        autoLabels = TRUE,
                        group = "ESRI imagery with labels") %>%
    addLayersControl(
      baseGroups = c("ESRI imagery with labels", "OpenStreetMap")
    ) %>%
    leafem::addMouseCoordinates()

  output$map2 <- renderLeaflet({
    main_map2
  })

  map2 <- leafletProxy("map2", session)
  
  ## create object with selected oblasts ####
  obl <- reactive(subset(adm_1, adm_1$adm_1_name %in% input$regions))
  obl_bounds <- reactive(obl() %>% st_bbox() %>% as.character()) # to set extent around selected oblasts
  ## conditional selection of raions based on selected oblast ####
  observeEvent(input$regions, {
    clearShapes(map) # clean map
    clearMarkers(map) # clean previously loaded markers
    
    updatePickerInput(session = session, inputId = "raions",
                      choices = sort(subset(unique(adm_2$adm_2_name), adm_2$adm_1_name %in% input$regions)),
                      selected = NULL) # raions are shown but nothing is selected initially
    
    updateRadioButtons(session = session, inputId = "buffer_radius",
                       choices = buffer_choices,
                       selected = 0 )
    
    obl_geom <- st_geometry(obl()) # to extract geometry
    # reaktive_aoi_polygon(obl_geom) # We do not use this to protect against the download of data throughout the country.

    
	map %>%
      addPolygons(data = obl(), weight = 2, fill = F) %>%
      fitBounds(lng1 = obl_bounds()[1], lat1 = obl_bounds()[2], # set view by extent: p1 - top lext, p2 - bottom right
                lng2 = obl_bounds()[3], lat2 = obl_bounds()[4]) # extent is set after selection of oblast
	
	shinyjs::enable("act_get_gbif_data")
  })
  
  ## create object with selected raion ##
  raion <- reactive(subset(adm_2, adm_2$adm_2_name %in% input$raions))
  raion_bounds <- reactive(raion() %>% st_bbox() %>% as.character()) # to set extent around selected raions
  ## change map based on selected raions ####
  observeEvent(input$raions, {
    clearShapes(map) # clean map
    clearMarkers(map) # clean previously loaded markers
    
    updatePickerInput(session = session, inputId = "OTG",
                      choices = subset(choices_OTG, adm_3$adm_2_name %in% input$raions),
                      selected = NULL)
    
    updateRadioButtons(session = session, inputId = "buffer_radius",
                       choices = buffer_choices,
                       selected = 0 )
    
    raion_geom <- st_geometry(raion()) %>% st_union() # to extract geometry
    reaktive_aoi_polygon(raion_geom)
    reaktive_bufered_polygon(raion_geom)
    
    map %>% 
      addPolygons(data = raion(), options = polygon_aoi_options) %>%
      fitBounds(lng1 = raion_bounds()[1], lat1 = raion_bounds()[2],  # set view by extent: p1 - top lext, p2 - bottom right
                lng2 = raion_bounds()[3], lat2 = raion_bounds()[4]) #  to allow map zoom to selected area
    
    shinyjs::enable("act_get_gbif_data")
  })
  
  ## create object with selected OTG ##
  OTG <- reactive(subset(adm_3, adm_3$shapeID %in% input$OTG))
  OTG_bounds <- reactive(OTG() %>% st_bbox() %>% as.character()) # to set extent around selected raions
  ## change map based on selected raions ####
  observeEvent(input$OTG, {
    clearShapes(map) # clean map
    clearMarkers(map) # clean previously loaded markers
    
    updateRadioButtons(session = session, inputId = "buffer_radius",
                       choices = buffer_choices,
                       selected = 0 )
    
    OTG_geom <- st_geometry(OTG())  %>% st_union() # to extract geometry
    reaktive_aoi_polygon(OTG_geom)
    reaktive_bufered_polygon(OTG_geom)
    
    map %>% 
      addPolygons(data = OTG(), options = polygon_aoi_options) %>%
      fitBounds(lng1 = OTG_bounds()[1], lat1 = OTG_bounds()[2],  # set view by extent: p1 - top lext, p2 - bottom right
                lng2 = OTG_bounds()[3], lat2 = OTG_bounds()[4]) # to allow map zoom to selected area
    
    shinyjs::enable("act_get_gbif_data")
    })
  
  ## Adding custom contours ## 
 
  observeEvent(input$userContours, {
    clearShapes(map) # clean map
    clearMarkers(map) # clean previously loaded markers
    
    updateRadioButtons(session = session, inputId = "buffer_radius",
                       choices = buffer_choices,
                       selected = 0 )
    
    temp_dir_1 <- tempdir()

    # read spatial object
    if (grepl("\\.kmz$", input$userContours$datapath)) {
      unzip(zipfile = input$userContours$datapath , exdir = temp_dir_1 )
      
      uploaded_cont <-  st_zm(st_read(file.path(temp_dir_1, "doc.kml"))) # unzipping .kmz, reading the resulted file and removing Z dimension
    } else {
      uploaded_cont <-  st_zm(st_read(input$userContours$datapath)) # reading .kml and removing Z dimension
    }
    
    uploaded_cont_geom <- st_geometry(uploaded_cont) %>% st_union() # to extract geometry
    reaktive_aoi_polygon(uploaded_cont_geom)
    reaktive_bufered_polygon(uploaded_cont_geom)
    
    # calculate bounds
    uploaded_cont_bounds <- uploaded_cont_geom %>% st_bbox() %>% as.character()
    
    # add to map
    map %>%
      fitBounds(lng1 = uploaded_cont_bounds[1], lat1 = uploaded_cont_bounds[2],
                lng2 = uploaded_cont_bounds[3], lat2 = uploaded_cont_bounds[4]) %>%
      addPolygons(data = reaktive_aoi_polygon(), options = polygon_aoi_options)
    
    shinyjs::enable("act_get_gbif_data")
  })
  
  
  
  
  ## Draw polygon ####
  
  observeEvent(input$map_draw_new_feature, {
    clearShapes(map) # clean map
	  clearMarkers(map) # clean previously loaded markers
    
	  sf_curent_polygon <- leaf_draw_sf_polyg(input$map_draw_new_feature$geometry$coordinates)
	  
	  reaktive_aoi_polygon(sf_curent_polygon)
	  reaktive_bufered_polygon(sf_curent_polygon)
	  
    map %>% addPolygons(data = sf_curent_polygon, # layerId = id,   # add buffered polygon to map
                        options = polygon_aoi_options 
    )
    
    updateRadioButtons(session = session, inputId = "buffer_radius",
                       choices = buffer_choices,
                       selected = 0)
    
    shinyjs::enable("act_get_gbif_data")
  })
  
  observeEvent(input$map_draw_edited_features, {
    clearShapes(map) # clean map
    clearMarkers(map) # clean previously loaded markers
	
    sf_curent_polygon <- leaf_draw_sf_polyg(input$map_draw_edited_features$features[[1]]$geometry$coordinates)
    
    reaktive_aoi_polygon(sf_curent_polygon)
    reaktive_bufered_polygon(sf_curent_polygon)
    
    map %>% addPolygons(data = sf_curent_polygon, # layerId = id,   # add buffered polygon to map
                        options = polygon_aoi_options )
    
    updateRadioButtons(session = session, inputId = "buffer_radius",
                       choices = buffer_choices,
                       selected = 0)
    
    shinyjs::enable("act_get_gbif_data")
  })
  
  # Delete buffered polygon with source polygon
  observeEvent(input$map_draw_deleted_features, {
    clearShapes(map) # delete all shapes from map
    clearMarkers(map) # clean previously loaded markers
    reaktive_aoi_polygon()
	  reaktive_bufered_polygon()
	  updateRadioButtons(session = session, inputId = "buffer_radius",
	                     choices = buffer_choices,
	                     selected = 0)
	  
	  shinyjs::disable("act_get_gbif_data")
  })
  
# Build buffer   ####

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
  
  
# clip global GBIF data by reaktive_bufered_polygon() ####
  
  sf_clipped_data <- eventReactive(
    
    eventExpr = input$act_get_gbif_data,
    valueExpr = {
      if(is.null(reaktive_bufered_polygon)){
        #gbif_sf_dataset[0, ]  # works without it
      } else {
        # load(url(url_datadump))
        # load(file = path_datadump)
        # st_intersection(gbif_sf_dataset, reaktive_bufered_polygon()) # use dissolved polygon for occurrence search
        # gbif_sf_dataset %>%
        # filter(st_intersects(geometry, reaktive_bufered_polygon(), sparse = FALSE))
        # wkt <- reaktive_bufered_polygon() %>% st_as_text()
        read_sf(path_datadump_fgb, wkt_filter = reaktive_bufered_polygon() %>% st_as_text() )
      }
    }
  )

# add clipped data and polygons on map1 ####
  
  
  observeEvent(input$act_get_gbif_data, {
    
    if(is.null(reaktive_bufered_polygon())){
      clearShapes(map)
      clearMarkers(map2)
      
      string_message_Karta_sidebar <- reactive(
        paste0(txt_backend_no_polygon)
      )
      output$message_Karta_sidebar <- renderText({
        string_message_Karta_sidebar() 
      })
    } else {
      clearShapes(map) # delete all shapes from map
      # clearShapes(map2) # delete all shapes from map2
      clearMarkers(map2)
      
      
      # if ( (class(reaktive_bufered_polygon())[2] == "sfc") ) {
      #   sf_clipped_data <- st_intersection(gbif_sf_dataset, reaktive_bufered_polygon() )
      # } else {
      #   showModal(modalDialog(
      #     title = "Увага! Зона тінтересу не визначена",
      #     "Оберіть територію інтересу (район чи ОТГ), завантажте файл або намалюйте полігон.",
      #     easyClose = TRUE,
      #     footer = tagList(
      #       modalButton("Закрити"),
      #     )
      #   ))
      # }
      
      
      
      
      map %>%
        addPolygons(
          data = reaktive_aoi_polygon(),
          options = polygon_aoi_options
        ) %>%
        addPolygons(
          data = reaktive_bufered_polygon(), #layerId = id,   # add buffered polygon to map
          options = buffered_polygon_options
        ) %>%
        addCircleMarkers(
          data = sf_clipped_data(), # lng = ~Longitude, lat = ~Latitude, # add circle markers from dataframe
          # radius = 10,  # static radius
          radius = 2,  # calculation radius
          color = "red",
          popup = ~scientificName # simple popup from field "scientificName"
          # popup = ~paste0("<center>" ,"<b>", scientificName, "</b>", "</center>", "<br>",   # popup with HTML
          #                 "Население: ", population, " чел." )
        )
      
      map2 %>%
        # fitBounds(
        #   # lng1 = data_bounds[1], lat1 = data_bounds[2], # set view by extent: p1 - top lext, p2 - bottom right
        #   # lng2 = data_bounds[3], lat2 = data_bounds[4]) %>% # extent is set after selection of oblast
        #   lng1 = filteredData_bounds[1], lat1 = filteredData_bounds[2], # set view by extent: p1 - top lext, p2 - bottom right
        #   lng2 = filteredData_bounds[3], lat2 = filteredData_bounds[4]) %>% # extent is set after selection of oblast
        # addPolygons(
        #   data = reaktive_bufered_polygon(), #layerId = id,   # add buffered polygon to map
        #   options = buffered_polygon_options
        # ) %>%
        addCircleMarkers(
          data = sf_clipped_data(), # lng = ~Longitude, lat = ~Latitude, # add circle markers from dataframe
          # radius = 10,  # static radius
          radius = 2,  # calculation radius
          color = "red",
          popup = ~paste0("<center>" ,"<b>", nameUk, "</b>", "</center>", # "<br>",   # popup with HTML 
                          "<center>", scientificName, "</center>",  
                          "<center>",
                          "<a href=\'",URL_record,"\' target=\'_blank\'>",
                          URL_record,
                          "</a>",
                          "</center>"
          )  # "URL_record"
        )
      
      string_message_Karta_sidebar <- reactive(
        paste0("") # TODO text to config
      )
      output$message_Karta_sidebar <- renderText({
        string_message_Karta_sidebar()
      })
        
      shinyjs::enable("refresh_filters")
      
      # Update filters in Tab Preview #
      updatePickerInput(session = session, inputId = "redbook",
                        choices = chku_category,
                        selected = chku_category)
      
      updatePickerInput(session = session, inputId = "iucn",
                        choices = txt_interface_tabs_filter_iucnrl_choices,
                        selected = iucn_category_selected)
      
      updatePickerInput(session = session, inputId = "international_filters",
                        choices = txt_interface_tabs_filter_international_choices,
                        selected = vector_conventions)
      
      updatePickerInput(session = session, inputId = "region_redlist_filters",
                        choices = txt_interface_tabs_filter_regionalrl_choices,
                        selected = NULL)
      
      updateCheckboxInput(session = session, inputId = "invasive", value = FALSE)
    }
  })
  


  # # add request result to webmap
  # observeEvent(input$act_get_gbif_data, {
  # # observe({
  #   map %>%
  #     addCircleMarkers(
  #       data = sf_clipped_data(), # lng = ~Longitude, lat = ~Latitude, # add circle markers from dataframe
  #       # radius = 10,  # static radius
  #       radius = 2,  # calculation radius
  #       color = "red",
  #       popup = ~scientificName # simple popup from field "scientificName"
  #       # popup = ~paste0("<center>" ,"<b>", scientificName, "</b>", "</center>", "<br>",   # popup with HTML
  #       #                 "Население: ", population, " чел." )
  #     )
  #   }
  # )
  

  
  # render result of request in tab "Попередній перегляд на мапі" ####
  intern_filt_present <- reactive( vector_conventions %in% input$international_filters )
  
  region_filt_present <- reactive( vect_region_redlist %in% input$region_redlist_filters )
  
  sf_filteredData <- reactive({
    input$refresh_filters
    
    isolate(sf_clipped_data() %>%
              filter(iucnRedListCategory %in% input$iucn | 
                       (ЧКУ %in% input$redbook) | 
                       (intern_filt_present()[1] & BernAppendix1 == "yes" ) |
                       (intern_filt_present()[2] & BernAppendix2 == "yes" ) |
                       (intern_filt_present()[3] & BernAppendix3 == "yes" ) |
                       (intern_filt_present()[4] & BernResolution6 == "yes" ) |
                       (intern_filt_present()[5] & Bonn == "yes") |
                       (intern_filt_present()[6] & AEWA == "yes") |
                       (intern_filt_present()[7] & EUROBATS == "yes") |
                       (intern_filt_present()[8] & ACCOBAMS == "yes") |
                       (intern_filt_present()[9] & BirdsDirectiveAnnex_I == "yes") |
                       (intern_filt_present()[10] & BirdsDirectiveAnnex_IІ == "yes") |
                       (intern_filt_present()[11] & HabitatsDirectiveAnnex_II == "yes") |
                       (intern_filt_present()[12] & HabitatsDirectiveAnnex_IV == "yes") |
                       (intern_filt_present()[13] & HabitatsDirectiveAnnex_V == "yes") |
                       (region_filt_present()[1] & ЧС_Вінницька == "yes") |
                       (region_filt_present()[2] & ЧС_Волинська == "yes") |
                       (region_filt_present()[3] & ЧС_Дніпропетровська == "yes") |
                       (region_filt_present()[4] & ЧС_Донецька == "yes") |
                       (region_filt_present()[5] & ЧС_Житомирська == "yes") |
                       (region_filt_present()[6] & ЧС_Закарпатська == "yes") |
                       (region_filt_present()[7] & ЧС_Запорізька == "yes") |
                       (region_filt_present()[8] & ЧС_Івано_Франківська == "yes") |
                       (region_filt_present()[9] & ЧС_Київська == "yes") |
                       (region_filt_present()[10] & ЧС_Кіровоградська == "yes") |
                       (region_filt_present()[11] & ЧС_Луганська == "yes") |
                       (region_filt_present()[12] & ЧС_Львівська == "yes") |
                       (region_filt_present()[13] & ЧС_Миколаївська == "yes") |
                       (region_filt_present()[14] & ЧС_Одеська == "yes") |
                       (region_filt_present()[15] & ЧС_Полтавська == "yes") |
                       (region_filt_present()[16] & ЧС_Рівненська == "yes") |
                       (region_filt_present()[17] & ЧС_Сумська == "yes") |
                       (region_filt_present()[18] & ЧС_Тернопільська == "yes") |
                       (region_filt_present()[19] & ЧС_Черкаська == "yes") |
                       (region_filt_present()[20] & ЧС_Чернівецька == "yes") |
                       (region_filt_present()[21] & ЧС_Чернігівська == "yes") |
                       (region_filt_present()[22] & ЧС_Харківська == "yes") |
                       (region_filt_present()[23] & ЧС_Херсонська == "yes") |
                       (region_filt_present()[24] & ЧС_Хмельницька == "yes") | 
                       (region_filt_present()[25] & ЧС_Київ == "yes") |
                       (region_filt_present()[26] & ЧС_Севастополь == "yes") |
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
        data = reaktive_bufered_polygon(), #layerId = id,   # add buffered polygon to map
        options = buffered_polygon_options
      ) %>%
      addCircleMarkers(data = sf_filteredData(), 
                       radius = 2,
                       color = "red",
                       popup = ~paste0("<center>" ,"<b>", nameUk, "</b>", "</center>", # "<br>",   # popup with HTML 
                                       "<center>", scientificName, "</center>",  
                                       "<center>",
                                       "<a href=\'",URL_record,"\' target=\'_blank\'>",
                                       URL_record,
                                       "</a>",
                                       "</center>"
                                       )
      )
  })
  
  
  # render result of request in tab "Попередній перегляд таблиці даних" ####
  df_filteredData <- reactive(st_drop_geometry(sf_filteredData())  %>%
                                dplyr::select(all_of(colnames_set1)) %>%
                                arrange("kingdom", "class", "family", "scientificName")
                              )
  
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
    paste0(txt_backend_number_of_records, toString(nrow(df_filteredData())) )
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
  
  output$txt_doi_url <- renderText({
    paste0("GBIF.org ", formatted_gbif_dataset_date, " GBIF Occurrence Download ", gbif_doi_url )
  })
  
  

  
  
  
  ## Generate report table for Tab - Генерування звітів ####
  
  observeEvent(input$refresh_filters, {
    ### Create Summary table ####
    pre_df_rare_lists <- data.frame(
      "Характеристика" = character(),
      "Кількість_видів" = numeric()
    )
    
    ### General species table ####
    df_report_table <- df_filteredData() %>%
      dplyr::select(all_of(colnames_set3)) %>%
      group_by(scientificName,  # настроить корректно групбай чтоб не удаляло лишние поля 
               nameUk,
               family,
               class,
               kingdom #,
               # ЧКУ,
               # iucnRedListCategory
      ) %>%
      summarise(Amount = n()) %>%
      arrange(kingdom, class, family, scientificName) %>%
      dplyr::select(all_of(c("kingdom", "class", "family", "Amount", "nameUk", "scientificName"))) %>% 
      dplyr::rename(all_of( rename_species_field ) ) %>%
      # colnames(c("Царство", "Кількість", "Українська назва", "Латинська назва")) %>%
      na.omit()
    
    
    tab_filtred_report(df_report_table)
    nrow_report(nrow(df_report_table))
    
    
    if (  !is.null(nrow_report()) && !is.na(nrow_report()) && !is.nan(nrow_report()) && nrow_report() > 0   ) {
      pre_df_rare_lists[nrow(pre_df_rare_lists) + 1,] = c(txt_backend_report_total, nrow_report())
      
      output$nrow_species_doc <- renderText({
        txt_backend_species_list
      })
      
      ## Draw preview report table Генерування звітів - загальний список видів
      output$report_table <- DT::renderDataTable(tab_filtred_report()) 
      
    } else {
      tab_filtred_report(NULL)
      nrow_report(NULL)
      output$nrow_species_doc <- renderText({NULL})
      output$report_table <- DT::renderDataTable(NULL)
    }     
    
    ### ChKU species table ####
    
    chku_tab <- subset(df_filteredData(), df_filteredData()$ЧКУ %in% chku_category , 
                       select = c("kingdom", "class", "family", "nameUk", "scientificName", "ЧКУ") 
    ) %>%
      group_by(kingdom, class, family, nameUk, scientificName,  ЧКУ) %>%
      summarise(Amount = n()) %>%
      arrange(kingdom, class, family, scientificName) %>%
      select( -c("Amount") ) %>%
      dplyr::rename(all_of( rename_chku_fields ) ) %>%
      na.omit()
    
    tab_filtred_chku(chku_tab)
    nrow_chku(nrow(chku_tab))
    
    
    if (  !is.null(nrow_chku()) && !is.na(nrow_chku()) && !is.nan(nrow_chku()) && nrow_chku() > 0   ) {
      
      output$nrow_chku_doc <- renderText({
        txt_backend_species_rdbuk
      })
      
      output$report_chku_table <- DT::renderDataTable(tab_filtred_chku())
      
      pre_df_rare_lists[nrow(pre_df_rare_lists ) + 1,] = c(txt_interface_tabs_filter_reddatabookofukraine,
                                                           nrow_chku())
      
    } else {
      tab_filtred_chku(NULL)
      nrow_chku(NULL)
      output$nrow_chku_doc <- renderText({NULL})
      output$report_chku_table <- DT::renderDataTable(NULL)
    }
    
    ### IUCN species table ####
    
    iucn_tab <- subset(df_filteredData(), df_filteredData()$iucnRedListCategory %in% iucn_category_selected , 
                       select = c("kingdom", "class", "family", "nameUk", "scientificName", "iucnRedListCategory") 
    ) %>%
      group_by(kingdom, class, family, nameUk, scientificName,  iucnRedListCategory) %>%
      summarise(Amount = n()) %>%
      arrange(kingdom, class, family, scientificName) %>%
      select( -c("Amount") ) %>%
      dplyr::rename(all_of( rename_iucn_fields ) ) %>%
      na.omit()
    
    tab_filtred_iucn(iucn_tab)
    nrow_iucn(nrow(iucn_tab))
    
    
    if (  !is.null(nrow_iucn()) && !is.na(nrow_iucn()) && !is.nan(nrow_iucn()) && nrow_iucn() > 0   ) {
      
      output$nrow_iucn_doc <- renderText({
        txt_backend_species_iucnrl
      })
      
      output$report_iucn_table <- DT::renderDataTable(tab_filtred_iucn())
      
      pre_df_rare_lists[nrow(pre_df_rare_lists ) + 1,] = c(txt_interface_tabs_filter_iucnrl,
                                                           nrow_iucn())
      
    } else {
      tab_filtred_iucn(NULL)
      nrow_iucn(NULL)
      output$nrow_iucn_doc <- renderText({NULL})
      output$report_iucn_table <- DT::renderDataTable(NULL)
    }
    
    ### BernApp_1 species table ####
    
    BernApp_1_tab <- subset(df_filteredData(), df_filteredData()$BernAppendix1 == "yes" , 
                            select = c("kingdom", "class", "family", "nameUk", "scientificName") 
    ) %>%
      group_by(kingdom, class, family, nameUk, scientificName) %>%
      summarise(Amount = n()) %>%
      arrange(kingdom, class, family, scientificName) %>%
      select( -c("Amount") ) %>%
      dplyr::rename(all_of( rename_convention_fields ) ) %>%
      na.omit()
    
    tab_filtred_BernApp_1(BernApp_1_tab)
    nrow_BernApp_1(nrow(BernApp_1_tab))
    
    
    if (  !is.null(nrow_BernApp_1()) && !is.na(nrow_BernApp_1()) && !is.nan(nrow_BernApp_1()) && nrow_BernApp_1() > 0   ) {
      
      output$nrow_BernApp_1_doc <- renderText({
        txt_backend_species_bern1
      })
      
      output$report_BernApp_1_table <- DT::renderDataTable(tab_filtred_BernApp_1())
      
      pre_df_rare_lists[nrow(pre_df_rare_lists ) + 1,] = c(names(txt_interface_tabs_filter_international_choices[1]),
                                                           nrow_BernApp_1())
      
    } else {
      tab_filtred_BernApp_1(NULL)
      nrow_BernApp_1(NULL)
      output$nrow_BernApp_1_doc <- renderText({NULL})
      output$report_BernApp_1_table <- DT::renderDataTable(NULL)
    }
    
    ### BernApp_2 species table ####
    
    BernApp_2_tab <- subset(df_filteredData(), df_filteredData()$BernAppendix2 == "yes" , 
                            select = c("kingdom", "class", "family", "nameUk", "scientificName") 
    ) %>%
      group_by(kingdom, class, family, nameUk, scientificName) %>%
      summarise(Amount = n()) %>%
      arrange(kingdom, class, family, scientificName) %>%
      select( -c("Amount") ) %>%
      dplyr::rename(all_of( rename_convention_fields ) ) %>%
      na.omit()
    
    tab_filtred_BernApp_2(BernApp_2_tab)
    nrow_BernApp_2(nrow(BernApp_2_tab))
    
    
    if (  !is.null(nrow_BernApp_2()) && !is.na(nrow_BernApp_2()) && !is.nan(nrow_BernApp_2()) && nrow_BernApp_2() > 0   ) {
      
      output$nrow_BernApp_2_doc <- renderText({
        txt_backend_species_bern2
      })
      
      output$report_BernApp_2_table <- DT::renderDataTable(tab_filtred_BernApp_2())
      
      pre_df_rare_lists[nrow(pre_df_rare_lists ) + 1,] = c(names(txt_interface_tabs_filter_international_choices[2]),
                                                           nrow_BernApp_2())
      
    } else {
      tab_filtred_BernApp_2(NULL)
      nrow_BernApp_2(NULL)
      output$nrow_BernApp_2_doc <- renderText({NULL})
      output$report_BernApp_2_table <- DT::renderDataTable(NULL)
    }
    
    ### BernApp_3 species table ####
    
    BernApp_3_tab <- subset(df_filteredData(), df_filteredData()$BernAppendix3 == "yes" , 
                            select = c("kingdom", "class", "family", "nameUk", "scientificName") 
    ) %>%
      group_by(kingdom, class, family, nameUk, scientificName) %>%
      summarise(Amount = n()) %>%
      arrange(kingdom, class, family, scientificName) %>%
      select( -c("Amount") ) %>%
      dplyr::rename(all_of( rename_convention_fields ) ) %>%
      na.omit()
    
    tab_filtred_BernApp_3(BernApp_3_tab)
    nrow_BernApp_3(nrow(BernApp_3_tab))
    
    
    if (  !is.null(nrow_BernApp_3()) && !is.na(nrow_BernApp_3()) && !is.nan(nrow_BernApp_3()) && nrow_BernApp_3() > 0   ) {
      
      output$nrow_BernApp_3_doc <- renderText({
        txt_backend_species_bern3
      })
      
      output$report_BernApp_3_table <- DT::renderDataTable(tab_filtred_BernApp_3())
      
      pre_df_rare_lists[nrow(pre_df_rare_lists ) + 1,] = c(names(txt_interface_tabs_filter_international_choices[3]),
                                                           nrow_BernApp_3())
      
    } else {
      tab_filtred_BernApp_3(NULL)
      nrow_BernApp_3(NULL)
      output$nrow_BernApp_3_doc <- renderText({NULL})
      output$report_BernApp_3_table <- DT::renderDataTable(NULL)
    }
    
    ### BernRes_6 species table ####
    
    BernRes_6_tab <- subset(df_filteredData(), df_filteredData()$BernResolution6  == "yes" , 
                            select = c("kingdom", "class", "family", "nameUk", "scientificName") 
    ) %>%
      group_by(kingdom, class, family, nameUk, scientificName) %>%
      summarise(Amount = n()) %>%
      arrange(kingdom, class, family, scientificName) %>%
      select( -c("Amount") ) %>%
      dplyr::rename(all_of( rename_convention_fields ) ) %>%
      na.omit()
    
    tab_filtred_BernRes_6(BernRes_6_tab)
    nrow_BernRes_6(nrow(BernRes_6_tab))
    
    
    if (  !is.null(nrow_BernRes_6()) && !is.na(nrow_BernRes_6()) && !is.nan(nrow_BernRes_6()) && nrow_BernRes_6() > 0   ) {
      
      output$nrow_BernRes_6_doc <- renderText({
        txt_backend_species_BernRes_6
      })
      
      output$report_BernRes_6_table <- DT::renderDataTable(tab_filtred_BernRes_6())
      
      pre_df_rare_lists[nrow(pre_df_rare_lists ) + 1,] = c(names(txt_interface_tabs_filter_international_choices[4]),
                                                           nrow_BernRes_6())
      
    } else {
      tab_filtred_BernRes_6(NULL)
      nrow_BernRes_6(NULL)
      output$nrow_BernRes_6_doc <- renderText({NULL})
      output$report_BernRes_6_table <- DT::renderDataTable(NULL)
    }
    
    ### Bonn species table ####
    
    Bonn_tab <- subset(df_filteredData(), df_filteredData()$Bonn  == "yes" , 
                       select = c("kingdom", "class", "family", "nameUk", "scientificName") 
    ) %>%
      group_by(kingdom, class, family, nameUk, scientificName) %>%
      summarise(Amount = n()) %>%
      arrange(kingdom, class, family, scientificName) %>%
      select( -c("Amount") ) %>%
      dplyr::rename(all_of( rename_convention_fields ) ) %>%
      na.omit()
    
    tab_filtred_Bonn(Bonn_tab)
    nrow_Bonn(nrow(Bonn_tab))
    
    
    if (  !is.null(nrow_Bonn()) && !is.na(nrow_Bonn()) && !is.nan(nrow_Bonn()) && nrow_Bonn() > 0   ) {
      
      output$nrow_Bonn_doc <- renderText({
        txt_backend_species_Bonn
      })
      
      output$report_Bonn_table <- DT::renderDataTable(tab_filtred_Bonn())
      
      pre_df_rare_lists[nrow(pre_df_rare_lists ) + 1,] = c(names(txt_interface_tabs_filter_international_choices[5]),
                                                           nrow_Bonn())
      
    } else {
      tab_filtred_Bonn(NULL)
      nrow_Bonn(NULL)
      output$nrow_Bonn_doc <- renderText({NULL})
      output$report_Bonn_table <- DT::renderDataTable(NULL)
    }
    
    ### AEWA species table ####
    
    AEWA_tab <- subset(df_filteredData(), df_filteredData()$AEWA  == "yes" , 
                       select = c("kingdom", "class", "family", "nameUk", "scientificName") 
    ) %>%
      group_by(kingdom, class, family, nameUk, scientificName) %>%
      summarise(Amount = n()) %>%
      arrange(kingdom, class, family, scientificName) %>%
      select( -c("Amount") ) %>%
      dplyr::rename(all_of( rename_convention_fields ) ) %>%
      na.omit()
    
    tab_filtred_AEWA(AEWA_tab)
    nrow_AEWA(nrow(AEWA_tab))
    
    
    if (  !is.null(nrow_AEWA()) && !is.na(nrow_AEWA()) && !is.nan(nrow_AEWA()) && nrow_AEWA() > 0   ) {
      
      output$nrow_AEWA_doc <- renderText({
        txt_backend_species_AEWA
      })
      
      output$report_AEWA_table <- DT::renderDataTable(tab_filtred_AEWA())
      
      pre_df_rare_lists[nrow(pre_df_rare_lists ) + 1,] = c(names(txt_interface_tabs_filter_international_choices[6]),
                                                           nrow_AEWA())
      
    } else {
      tab_filtred_AEWA(NULL)
      nrow_AEWA(NULL)
      output$nrow_AEWA_doc <- renderText({NULL})
      output$report_AEWA_table <- DT::renderDataTable(NULL)
    }
    
    ### EUROBATS species table ####
    
    EUROBATS_tab <- subset(df_filteredData(), df_filteredData()$EUROBATS == "yes" , 
                           select = c("kingdom", "class", "family", "nameUk", "scientificName") 
    ) %>%
      group_by(kingdom, class, family, nameUk, scientificName) %>%
      summarise(Amount = n()) %>%
      arrange(kingdom, class, family, scientificName) %>%
      select( -c("Amount") ) %>%
      dplyr::rename(all_of( rename_convention_fields ) ) %>%
      na.omit()
    
    tab_filtred_EUROBATS(EUROBATS_tab)
    nrow_EUROBATS(nrow(EUROBATS_tab))
    
    
    if (  !is.null(nrow_EUROBATS()) && !is.na(nrow_EUROBATS()) && !is.nan(nrow_EUROBATS()) && nrow_EUROBATS() > 0   ) {
      
      output$nrow_EUROBATS_doc <- renderText({
        txt_backend_report_eurobats
      })
      
      output$report_EUROBATS_table <- DT::renderDataTable(tab_filtred_EUROBATS())
      
      pre_df_rare_lists[nrow(pre_df_rare_lists ) + 1,] = c(names(txt_interface_tabs_filter_international_choices[7]),
                                                           nrow_EUROBATS())
      
    } else {
      tab_filtred_EUROBATS(NULL)
      nrow_EUROBATS(NULL)
      output$nrow_EUROBATS_doc <- renderText({NULL})
      output$report_EUROBATS_table <- DT::renderDataTable(NULL)
    }
    
    ### ACCOBAMS species table ####
    
    ACCOBAMS_tab <- subset(df_filteredData(), df_filteredData()$ACCOBAMS == "yes" , 
                           select = c("kingdom", "class", "family", "nameUk", "scientificName") 
    ) %>%
      group_by(kingdom, class, family, nameUk, scientificName) %>%
      summarise(Amount = n()) %>%
      arrange(kingdom, class, family, scientificName) %>%
      select( -c("Amount") ) %>%
      dplyr::rename(all_of( rename_convention_fields ) ) %>%
      na.omit()
    
    tab_filtred_ACCOBAMS(ACCOBAMS_tab)
    nrow_ACCOBAMS(nrow(ACCOBAMS_tab))
    
    
    if (  !is.null(nrow_ACCOBAMS()) && !is.na(nrow_ACCOBAMS()) && !is.nan(nrow_ACCOBAMS()) && nrow_ACCOBAMS() > 0   ) {
      
      output$nrow_ACCOBAMS_doc <- renderText({
        txt_backend_report_accobams
      })
      
      output$report_ACCOBAMS_table <- DT::renderDataTable(tab_filtred_ACCOBAMS())
      
      pre_df_rare_lists[nrow(pre_df_rare_lists ) + 1,] = c(txt_accobams_fullname,
                                                           nrow_ACCOBAMS())
      
    } else {
      tab_filtred_ACCOBAMS(NULL)
      nrow_ACCOBAMS(NULL)
      output$nrow_ACCOBAMS_doc <- renderText({NULL})
      output$report_ACCOBAMS_table <- DT::renderDataTable(NULL)
    }
    
    ### BirdDirAnn_I species table ####
    
    BirdDirAnn_I_tab <- subset(df_filteredData(), df_filteredData()$BirdsDirectiveAnnex_I == "yes" , 
                               select = c("kingdom", "class", "family", "nameUk", "scientificName") 
    ) %>%
      group_by(kingdom, class, family, nameUk, scientificName) %>%
      summarise(Amount = n()) %>%
      arrange(kingdom, class, family, scientificName) %>%
      select( -c("Amount") ) %>%
      dplyr::rename(all_of( rename_convention_fields ) ) %>%
      na.omit()
    
    tab_filtred_BirdDirAnn_I(BirdDirAnn_I_tab)
    nrow_BirdDirAnn_I(nrow(BirdDirAnn_I_tab))
    
    
    if (  !is.null(nrow_BirdDirAnn_I()) && !is.na(nrow_BirdDirAnn_I()) && !is.nan(nrow_BirdDirAnn_I()) && nrow_BirdDirAnn_I() > 0   ) {
      
      output$nrow_BirdDirAnn_I_doc <- renderText({
        txt_backend_report_birdDirectiveApp1
      })
      
      output$report_BirdDirAnn_I_table <- DT::renderDataTable(tab_filtred_BirdDirAnn_I())
      
      pre_df_rare_lists[nrow(pre_df_rare_lists ) + 1,] = c(names(txt_interface_tabs_filter_international_choices[9]),
                                                           nrow_BirdDirAnn_I())
      
    } else {
      tab_filtred_BirdDirAnn_I(NULL)
      nrow_BirdDirAnn_I(NULL)
      output$nrow_BirdDirAnn_I_doc <- renderText({NULL})
      output$report_BirdDirAnn_I_table <- DT::renderDataTable(NULL)
    }
    
    ### BirdDirAnn_II species table ####
    
    BirdDirAnn_II_tab <- subset(df_filteredData(), df_filteredData()$BirdsDirectiveAnnex_IІ == "yes" , 
                                select = c("kingdom", "class", "family", "nameUk", "scientificName") 
    ) %>%
      group_by(kingdom, class, family, nameUk, scientificName) %>%
      summarise(Amount = n()) %>%
      arrange(kingdom, class, family, scientificName) %>%
      select( -c("Amount") ) %>%
      dplyr::rename(all_of( rename_convention_fields ) ) %>%
      na.omit()
    
    tab_filtred_BirdDirAnn_II(BirdDirAnn_II_tab)
    nrow_BirdDirAnn_II(nrow(BirdDirAnn_II_tab))
    
    
    if (  !is.null(nrow_BirdDirAnn_II()) && !is.na(nrow_BirdDirAnn_II()) && !is.nan(nrow_BirdDirAnn_II()) && nrow_BirdDirAnn_II() > 0   ) {
      
      output$nrow_BirdDirAnn_II_doc <- renderText({
        txt_backend_report_birdDirectiveApp2
      })
      
      output$report_BirdDirAnn_II_table <- DT::renderDataTable(tab_filtred_BirdDirAnn_II())
      
      pre_df_rare_lists[nrow(pre_df_rare_lists ) + 1,] = c(names(txt_interface_tabs_filter_international_choices[10]),
                                                           nrow_BirdDirAnn_II())
      
    } else {
      tab_filtred_BirdDirAnn_II(NULL)
      nrow_BirdDirAnn_II(NULL)
      output$nrow_BirdDirAnn_II_doc <- renderText({NULL})
      output$report_BirdDirAnn_II_table <- DT::renderDataTable(NULL)
    }
    
    ### HabitatsDirAnn_II species table ####
    
    HabitatsDirAnn_II_tab <- subset(df_filteredData(), df_filteredData()$HabitatsDirectiveAnnex_II  == "yes" , 
                                    select = c("kingdom", "class", "family", "nameUk", "scientificName") 
    ) %>%
      group_by(kingdom, class, family, nameUk, scientificName) %>%
      summarise(Amount = n()) %>%
      arrange(kingdom, class, family, scientificName) %>%
      select( -c("Amount") ) %>%
      dplyr::rename(all_of( rename_convention_fields ) ) %>%
      na.omit()
    
    tab_filtred_HabitatsDirAnn_II(HabitatsDirAnn_II_tab)
    nrow_HabitatsDirAnn_II(nrow(HabitatsDirAnn_II_tab))
    
    
    if (  !is.null(nrow_HabitatsDirAnn_II()) && !is.na(nrow_HabitatsDirAnn_II()) && !is.nan(nrow_HabitatsDirAnn_II()) && nrow_HabitatsDirAnn_II() > 0   ) {
      
      output$nrow_HabitatsDirAnn_II_doc <- renderText({
        txt_backend_report_habitatDirectiveApp2
      })
      
      output$report_HabitatsDirAnn_II_table <- DT::renderDataTable(tab_filtred_HabitatsDirAnn_II())
      
      pre_df_rare_lists[nrow(pre_df_rare_lists ) + 1,] = c(names(txt_interface_tabs_filter_international_choices[11]),
                                                           nrow_HabitatsDirAnn_II())
      
    } else {
      tab_filtred_HabitatsDirAnn_II(NULL)
      nrow_HabitatsDirAnn_II(NULL)
      output$nrow_HabitatsDirAnn_II_doc <- renderText({NULL})
      output$report_HabitatsDirAnn_II_table <- DT::renderDataTable(NULL)
    }
    
    ### HabitatsDirAnn_IV species table ####
    
    HabitatsDirAnn_IV_tab <- subset(df_filteredData(), df_filteredData()$HabitatsDirectiveAnnex_IV  == "yes" , 
                                    select = c("kingdom", "class", "family", "nameUk", "scientificName") 
    ) %>%
      group_by(kingdom, class, family, nameUk, scientificName) %>%
      summarise(Amount = n()) %>%
      arrange(kingdom, class, family, scientificName) %>%
      select( -c("Amount") ) %>%
      dplyr::rename(all_of( rename_convention_fields ) ) %>%
      na.omit()
    
    tab_filtred_HabitatsDirAnn_IV(HabitatsDirAnn_IV_tab)
    nrow_HabitatsDirAnn_IV(nrow(HabitatsDirAnn_IV_tab))
    
    
    if (  !is.null(nrow_HabitatsDirAnn_IV()) && !is.na(nrow_HabitatsDirAnn_IV()) && !is.nan(nrow_HabitatsDirAnn_IV()) && nrow_HabitatsDirAnn_IV() > 0   ) {
      
      output$nrow_HabitatsDirAnn_IV_doc <- renderText({
        txt_backend_report_habitatDirectiveApp4
      })
      
      output$report_HabitatsDirAnn_IV_table <- DT::renderDataTable(tab_filtred_HabitatsDirAnn_IV())
      
      pre_df_rare_lists[nrow(pre_df_rare_lists ) + 1,] = c(names(txt_interface_tabs_filter_international_choices[12]),
                                                           nrow_HabitatsDirAnn_IV())
      
    } else {
      tab_filtred_HabitatsDirAnn_IV(NULL)
      nrow_HabitatsDirAnn_IV(NULL)
      output$nrow_HabitatsDirAnn_IV_doc <- renderText({NULL})
      output$report_HabitatsDirAnn_IV_table <- DT::renderDataTable(NULL)
    }
    
    ### HabitatsDirAnn_V species table ####
    
    HabitatsDirAnn_V_tab <- subset(df_filteredData(), df_filteredData()$HabitatsDirectiveAnnex_V  == "yes" , 
                                   select = c("kingdom", "class", "family", "nameUk", "scientificName") 
    ) %>%
      group_by(kingdom, class, family, nameUk, scientificName) %>%
      summarise(Amount = n()) %>%
      arrange(kingdom, class, family, scientificName) %>%
      select( -c("Amount") ) %>%
      dplyr::rename(all_of( rename_convention_fields ) ) %>%
      na.omit()
    
    tab_filtred_HabitatsDirAnn_V(HabitatsDirAnn_V_tab)
    nrow_HabitatsDirAnn_V(nrow(HabitatsDirAnn_V_tab))
    
    
    if (  !is.null(nrow_HabitatsDirAnn_V()) && !is.na(nrow_HabitatsDirAnn_V()) && !is.nan(nrow_HabitatsDirAnn_V()) && nrow_HabitatsDirAnn_V() > 0   ) {
      
      output$nrow_HabitatsDirAnn_V_doc <- renderText({
        txt_backend_report_habitatDirectiveApp5
      })
      
      output$report_HabitatsDirAnn_V_table <- DT::renderDataTable(tab_filtred_HabitatsDirAnn_V())
      
      pre_df_rare_lists[nrow(pre_df_rare_lists ) + 1,] = c(names(txt_interface_tabs_filter_international_choices[13]),
                                                           nrow_HabitatsDirAnn_V())
      
    } else {
      tab_filtred_HabitatsDirAnn_V(NULL)
      nrow_HabitatsDirAnn_V(NULL)
      output$nrow_HabitatsDirAnn_V_doc <- renderText({NULL})
      output$report_HabitatsDirAnn_V_table <- DT::renderDataTable(NULL)
    }
    
    ### Invasive species table ####
    
    Invasive_tab <- subset(df_filteredData(), df_filteredData()$Invasive  == "yes" , 
                           select = c("kingdom", "class", "family", "nameUk", "scientificName") 
    ) %>%
      group_by(kingdom, class, family, nameUk, scientificName) %>%
      summarise(Amount = n()) %>%
      arrange(kingdom, class, family, scientificName) %>%
      select( -c("Amount") ) %>%
      dplyr::rename(all_of( rename_convention_fields ) ) %>%
      na.omit()
    
    tab_filtred_Invasive(Invasive_tab)
    nrow_Invasive(nrow(Invasive_tab))
    
    
    if (  !is.null(nrow_Invasive()) && !is.na(nrow_Invasive()) && !is.nan(nrow_Invasive()) && nrow_Invasive() > 0   ) {
      
      output$nrow_Invasive_doc <- renderText({
        txt_backend_report_invasive_alien_species
      })
      
      output$report_Invasive_table <- DT::renderDataTable(tab_filtred_Invasive())
      
      pre_df_rare_lists[nrow(pre_df_rare_lists ) + 1,] = c(txt_backend_report_invasive_alien_species,
                                                           nrow_Invasive())
      
    } else {
      tab_filtred_Invasive(NULL)
      nrow_Invasive(NULL)
      output$nrow_Invasive_doc <- renderText({NULL})
      output$report_Invasive_table <- DT::renderDataTable(NULL)
    }
    
    
    ### Summary table to reactive value ####
    df_rare_lists(pre_df_rare_lists)
    
    ## Draw preview report table Зведена статистика по природоохорним перелікам
    output$report_rare_lists_table <- DT::renderDataTable(df_rare_lists())
    
  })
  
  
  
  
    
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
  

  output$downloadReport <- downloadHandler(
    filename = function() {
      # paste('my-report', sep = '.', switch(
      paste("GBIF_data-", Sys.Date(), ".", sep="", switch(
          # input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
        input$format, HTML = 'html', Word = 'docx'
      ))
    },
    content = function(file) {
      src_template <- normalizePath('templates/report.Rmd')
      src_logo_nlbif <- normalizePath('templates/logo-nlbif.png')
      src_logo_hf <- normalizePath('templates/the_habitat_foundation_logo.png')
      src_logo_uncg <- normalizePath('templates/uncg_logo.png')
      # src_folder <- normalizePath('templates')

      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src_template, 'report.Rmd', overwrite = TRUE)
      file.copy(src_logo_nlbif, 'logo-nlbif.png', overwrite = TRUE)
      file.copy(src_logo_hf, 'the_habitat_foundation_logo.png', overwrite = TRUE)
      file.copy(src_logo_uncg, 'uncg_logo.png', overwrite = TRUE)
      # file.copy(normalizePath(
      #   c(
      #     "templates/report.Rmd", 
      #     "templates/logo-nlbif.png", 
      #     "templates/the_habitat_foundation_logo.png", 
      #     "templates/uncg_logo.png"
      #     )
      #   ), 
      # owd, overwrite = TRUE)

      # library(rmarkdown)
      out <- render('report.Rmd', switch(
        input$format,
        # PDF = pdf_document(), HTML = html_document(), Word = word_document()
        HTML = html_document(), Word = word_document()
      ))
      file.rename(out, file)
    }
  )
  
  # It don't work
  # # output$downloadData_PDF <- downloadHandler(
  # #   filename = function() {
  # #     paste("GBIF_data-", Sys.Date(), ".pdf", sep="")
  # #   },
  # #   content = function(file) {
  # #     # Code to generate the PDF file 
  # #     rmarkdown::render(
  # #       input = "templates/report.Rmd",
  # #       output_format = pdf_document(),
  # #       output_file = file,
  # #     )
  # #   }
  # # )

  # to create larger table for excel output
  # output$gbif_table_set1 <- DT::renderDataTable(df_filteredData()[, colnames_set1])
  
  ## Red book table ####
  # output$redbook_table <- DT::renderDataTable(df_redbook)
  
  ## On session ended ####
  session$onSessionEnded(function() {
    rm(list = ls())
    gc()
  })
  
  observe({   # применяется для доступа к реактивным переменным, распечатки их в консоль и отладки
    print("reaktive_bufered_polygon: ")
    print(reaktive_bufered_polygon())
    print(paste("class: ", class(reaktive_bufered_polygon()), sep=""))
    print("Dissolved polygon: ")
    # print(reaktive_dissolved_polygon())
    print(reaktive_bufered_polygon())
    # print(paste("class: ", class(reaktive_dissolved_polygon()), sep=""))
    print(paste("class: ", class(reaktive_bufered_polygon()), sep=""))
    # print("reaktive_aoi_polygon(): ")
    # print(reaktive_aoi_polygon())
    # print("class reaktive_aoi_polygon(): ")
    # print(class(reaktive_aoi_polygon()) )
    # print("class class reaktive_aoi_polygon(): ")
    # print(class(class(reaktive_aoi_polygon()) ))
    # print("class 1 reaktive_aoi_polygon(): ")
    # print(class(reaktive_aoi_polygon())[2] )
    # print("recieved_data sample: ")
    # print(head(select(recieved_data(), recordedBy, eventDate, scientificName)))
    # print(paste("number of observations: ", nrow(recieved_data()), sep=""))
    # print(str(df_recieved_data()))
    # print(head(df_recieved_data()))
    # print("Map info: ")
    # print(getMapData(map))
    # print("input$redbook: ")
    # print(class(input$redbook))
    # print(str(input$redbook))
    # print(input$redbook)
    # print(kilkist_chku())
    # print("tab_filtred_chku :")
    # print(str(tab_filtred_chku()))
    print("done")
    print(Sys.time())
  })
  
  
}


shinyApp(ui, server)
