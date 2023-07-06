# preview DOCX report
# search by taxonID
# search points by the name from protected status lists in rayons, export preview table as CSV and XLSX

setwd("C:/Users/admin/Documents/GitHub/gbif_shiny_onlineviewer/gbif_app_v1")

# Biodiversity Viewer v.0.1

# use https://mastering-shiny.org/action-layout.html
# https://shiny.rstudio.com/gallery/viscover.html
# https://github.com/XiaodanLyu/viscover/blob/master/inst/shiny-examples/overlay/app.r

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
library(rgbif)
library(leaflet)
library(leaflet.extras)
library(leafem)
library(DT)
library(data.table)
library(openxlsx)
library(openxlsx2)
library(rmarkdown)
library(knitr)

# TODO add function polygon_bufferisation(polygon, radius)

# Calling custom function
source("scripts/config.R")
source("functions/polygon_bufferisation.R")

# import data ####
## import spatial data ####
# Ukr_0 <- st_read("regions/gadm41_UKR_0.shp")
# Ukr_1 <- st_read("regions/gadm41_UKR_1.shp")
# Ukr_2 <- st_read("regions/gadm41_UKR_2.shp")
Ukr_Obl <- st_read("adm_shp/Ukr_Obl_NEW.shp")
Ukr_Rai <- st_read("adm_shp/Ukr_Raion_NEW.shp")
# Ukr_OTG <- st_read("adm_shp/Ukr_OTG_NEW.shp")

## load Red lists ####
load(file = "dictionaries/df_protected_status.Rdata")
load(file = "dictionaries/red_book_vrazlyvyi.Rdata")
load(file = "dictionaries/red_book_ridkisnyi.Rdata")
load(file = "dictionaries/red_book_znykaiuchyi.Rdata")
load(file = "dictionaries/red_book_znyklyi.Rdata")
load(file = "dictionaries/red_book_znyklyi_v_pryrodi.Rdata")
load(file = "dictionaries/red_book_nedostatno_vidomyi.Rdata")
load(file = "dictionaries/red_book_neotsinenyi.Rdata")

# df_redbook <- dplyr::select(df_protected_status, all_of(c("base_name", "scientificName", "nameUk", "ЧКУ", "kingdom", "phylum", "class"))) %>% filter(ЧКУ != "NA")

# Frontend ####
ui = fluidPage(
  tags$style(
    ".footer-class {
      color: '#BBBBBB';
      position: absolute;
      bottom: 0;
      left: 5;
    }
    # #element {
    #   color: red;
    # }
    "
  ),
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
                           pickerInput('regions', 'Оберіть область', 
                                       unique(Ukr_Obl$obl_name),
                                       selected = c(unique(Ukr_Obl$obl_name)),
                                       options = list(`actions-box` = TRUE), multiple = T),
                           ## Raion selection ####
                           pickerInput('raions', 'Оберіть район',
                                       unique(Ukr_Rai$raion_name),
                                       selected = NULL,
                                       options = list(`actions-box` = TRUE), multiple = T),
                           # ## OTG selection ####
                           # pickerInput('OTG', 'Оберіть територіальну громаду',
                           #             unique(Ukr_OTG$shapeID),
                           #             # unique(Ukr_OTG$OTG_name),
                           #             selected = NULL,
                           #             options = list(`actions-box` = TRUE), multiple = T),
                           

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
                           leafletOutput("map",  width = "100%", height="83vh"),
                         )
                       ),
                       p("GBIF Viewer: an open web-based biodiversity conservation decision-making tool for policy and governance. Спільний проєкт The Habitat Foundation та Української Природоохоронної Групи, за підтримки NLBIF: The Netherlands Biodiversity Information Facility, nlbif2022.014",
                         # class = "footer-class"
                         )
              ),
              ## Tab - Попередній перегляд ####
              tabPanel("Попередній перегляд", 
                       tags$br(),
                       ## Button for send GBIF request ####
                       downloadButton("downloadData_CSV", "Download CSV"),
                       downloadButton("downloadData_XLSX", "Download XLSX"),
                       textOutput("nrow_clipped_data"),
                       tags$hr(),
                       DT::dataTableOutput("gbif_table"),
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
                       DT::dataTableOutput("report_table"),
                       p("GBIF Viewer: an open web-based biodiversity conservation decision-making tool for policy and governance. Спільний проєкт The Habitat Foundation та Української Природоохоронної Групи, за підтримки NLBIF: The Netherlands Biodiversity Information Facility, nlbif2022.014",
                         # class = "footer-class"
                       )
              ),
              # ## Tab Червона Книга України - for testing ####              
              # tabPanel("Червона Книга України",
              #          # tableOutput("my_table")
              #          DT::dataTableOutput("redbook_table"),
              # )
  ),
  # p("GBIF Viewer: an open web-based biodiversity conservation decision-making tool for policy and governance. Спільний проєкт The Habitat Foundation та Української Природоохоронної Групи, за підтримки NLBIF: The Netherlands Biodiversity Information Facility, nlbif2022.014", 
  #   class = "footer-class")
)

# Back end ####
server = function(input, output, session) {
  
  # Create a global reactive value
  ## Create a global reactive value for WKT buffered polygon
  
  reaktive_bufered_polygon <- reactiveVal() # Create a global reactive value for buffered polygon - now SF, not WKT!
  reaktive_bufered_polygon_wkt <- reactiveVal() # Create a global reactive value for WKT buffered polygon
  
  ## create the leaflet map ####
  main_map <-  leaflet() %>% addTiles() %>% #setView(36.39, 49.65, zoom = 11) %>% # to set view around Homilsha forest
    # addSearchOSM() %>% 
    # fitBounds(lng1 = 22.14, lat1 = 52.35,  # set view by extent: p1 - top lext, p2 - bottom right # extent is set later
    #           lng2 = 40.22, lat2 = 33.68) %>% # to allow map zoom to selected area
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
                      choices = subset(unique(Ukr_Rai$raion_name), Ukr_Rai$obl_name %in% input$regions),
                      selected = NULL) # raions are shown but nothing is selected initially
    
    # obl_geom <- st_geometry(obl()) # to extract geometry
    # reaktive_bufered_polygon(obl_geom)

    
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
    
    raion_buffered <- polygon_bufferisation(raion(), input$buffer_radius) %>% 
      # st_combine() %>%
      st_union() # dissolve all created polygons to avoid overlaps
    
    map %>% 
      addPolygons(data = raion(), weight = 2, fill = F) %>%
      fitBounds(lng1 = raion_bounds()[1], lat1 = raion_bounds()[2],  # set view by extent: p1 - top lext, p2 - bottom right
                lng2 = raion_bounds()[3], lat2 = raion_bounds()[4]) %>%  # to allow map zoom to selected area
      addPolygons(data = raion_buffered, #layerId = id,   # add buffered polygon to map
                  options = buffered_polygon_options)
    
    # raion_buffered_WKT <- st_bbox(raion_buffered) %>% st_as_sfc() %>% st_as_text()
    # raion_buffered_geom <- st_geometry(raion_buffered) # to extract geometry
    # reaktive_bufered_polygon(raion_buffered_geom)    # write raion_buffered in my custom global reactive value
    reaktive_bufered_polygon(raion_buffered)    # write raion_buffered in my custom global reactive value
    
    raion_buffered_WKT <- st_as_text(raion_buffered) # it work
    reaktive_bufered_polygon_wkt(raion_buffered_WKT)    # write curent_buffered_WKT in my custom global reactive value
    
  })
  
  
  ## Draw polygon ####
  
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
    
    reaktive_bufered_polygon(sf_curent_buffered)
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
    
    reaktive_bufered_polygon(sf_curent_buffered)
    curent_buffered_WKT <- st_as_text(sf_curent_buffered) # it work
    reaktive_bufered_polygon_wkt(curent_buffered_WKT)    # write curent_buffered_WKT in my custom global reactive value
    
  })
  
  # Delete buffered polygon with source polygon
  observeEvent(input$map_draw_deleted_features, {
    clearShapes(map) # delet all shapes from map
    clearMarkers(map) # clean previously loaded markers
    reaktive_bufered_polygon("")
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
                                   occ_search(taxonKey = species_list(),
                                              # scientificName = species_list(),
                                              return = "data",
                                              hasCoordinate = T,
                                              country = "UA",
                                              geometry = reaktive_bufered_polygon_wkt(),
                                              geom_big = "bbox",
                                              limit = query_limit) 
                                 }
  )

  df_recieved_data <- reactive(rbindlist(lapply(recieved_data(), function(x) x$data), fill = TRUE, use.names = TRUE) %>% 
                                 dplyr::select(all_of(fields_list_to_DF_PREVIEW)) %>% 
                                 rename(Latitude = decimalLatitude, Longitude = decimalLongitude) # TODO add join Ua name and calc fields with link
    )
  
  # Convert to SF object
  sf_recieved_by_extent_data <- reactive(st_as_sf(df_recieved_data(), dim = "XY", remove = FALSE, na.fail = F, 
                        coords = c("Longitude", "Latitude"), crs = "+proj=longlat +datum=WGS84 +no_defs") )

  # clip by polygon
  
  sf_clipped_data <- reactive(st_intersection(sf_recieved_by_extent_data(), reaktive_bufered_polygon()) )
  

  

  # add request result to webmap
  observe({
    map %>%
      addCircleMarkers(data = sf_clipped_data(), # lng = ~Longitude, lat = ~Latitude, # add circle markers from dataframe
                       # radius = 10,  # static radius
                       radius = 2,  # calculation radius
                       color = "red",
                       popup = ~scientificName # simple popup from field "scientificName"
                       # popup = ~paste0("<center>" ,"<b>", scientificName, "</b>", "</center>", "<br>",   # popup with HTML
                       #                 "Население: ", population, " чел." )
      )
  }
  )
  
  # TODO add converting sf_clipped_data() to dataframe before export and 
  
  df_clipped_data <- reactive(st_drop_geometry(sf_clipped_data()))

  # render result of request in tab "Попередній перегляд"
  ## Generate table preview
  output$gbif_table <- DT::renderDataTable(df_clipped_data())
  ## Generate CSV 
  output$downloadData_CSV <- downloadHandler(
    filename = function() {
      paste("GBIF_data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(df_clipped_data(), file)
    }
  )
  ## Generate XLSX
  output$downloadData_XLSX <- downloadHandler(
    filename = function() {
      paste("GBIF_data-", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      write_xlsx(df_clipped_data(), file, colNames = TRUE) 
    }
  )
  
  string_nrow_clipped_data <- reactive(toString(nrow(df_clipped_data())))
  
  output$nrow_clipped_data <- renderText({
    paste0("Кількість знахідок: ", string_nrow_clipped_data() )
  })
  
  
  
  ## Create DF_PREPRINT dataframe for Tab - Генерування звітів ####
  
  df_report_table <- reactive(df_clipped_data() %>%
    dplyr::select(all_of(fields_list_to_DF_PREPRINT)) %>%
    group_by(scientificName,  # настроить корректно групбай чтоб не удаляло лишние поля или джойнить их DF_REPORT
             kingdom #,
             # NameUA,          # TODO to config
             # RedBookUA,
             # IUCN_Red_List
             ) %>%
    summarise(Amount = n()) %>%
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
      geom_sf(data=sf_clipped_data(), aes(color=kingdom),size=2)+
      scale_colour_manual(values = kingdom_colors, name=NULL ) +
      geom_sf(data = reaktive_bufered_polygon(), colour = "blue", fill=NA, lwd = 1)+
      # geom_sf(data = aoi_geometry, colour = "red", fill = NA, lwd = 1)+ # TODO create global var for AOI
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
    
    
  
  
  observe({   # применяется для доступа к реактивным переменным, распечатки их в консоль и отладки
    # print("reaktive_bufered_polygon_wkt: ")
    # print(reaktive_bufered_polygon_wkt())
    print("raion_bounds:")
    print(raion_bounds())
    print("redbook_finder: ")
    print(input$redbook_finder)
    print("species_list: ")
    print(str(species_list()))
  	print(length(species_list()))
    # print("recieved_data: ")
    # print(str(recieved_data()))
  	print("start: ")
  	print(Sys.time())
	  print("df_recieved_data: ")
    # print(str(df_recieved_data()))
    # print(head(df_recieved_data()))
	  print("nrow df_recieved_data:")
    print(nrow(df_recieved_data()))
    print("class sf_clipped_data: ")
    print(class(sf_clipped_data()))
    print("nrow df_clipped_data:")
    print(nrow(df_clipped_data()))
    # print("Map info: ")
    # print(getMapData(map))
    print("done")
	  print(Sys.time())
  })
  
  
  
}



shinyApp(ui, server)
