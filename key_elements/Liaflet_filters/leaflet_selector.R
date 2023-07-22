

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
library(sf)
library(dplyr)
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
          leafletOutput("map",  width = "100%", height="83vh"),
        )
      ),
    )
  )
)

server <- function(input, output, session) {
  
  filteredData <- reactive({
    input$refresh_filters
    
    isolate(data %>%
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
  
  data_bounds <- data %>% st_bbox() %>% as.character()
  
  # data_bounds <- reactive(filteredData() %>% st_bbox() %>% as.character())
  
  
  
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
      fitBounds(
        lng1 = data_bounds[1], lat1 = data_bounds[2], # set view by extent: p1 - top lext, p2 - bottom right
        lng2 = data_bounds[3], lat2 = data_bounds[4]) %>% # extent is set after selection of oblast
                # lng1 = data_bounds()[1], lat1 = data_bounds()[2], # set view by extent: p1 - top lext, p2 - bottom right
                # lng2 = data_bounds()[3], lat2 = data_bounds()[4]) %>% # extent is set after selection of oblast
      addCircleMarkers(data = filteredData(), 
                       radius = 2,
                       color = "red",
                       popup = ~paste0("<center>" ,"<b>", nameUk, "</b>", "</center>", # "<br>",   # popup with HTML 
                                       "<center>", scientificName, "</center>" )
      )
  })
  
  
  # observe({
  # #   print("data_bounds:")
  # #   print(data_bounds())
  #     # print("input$bern2 :")
  #     # print(input$bern2)
  # })
  
}

shinyApp(ui, server)



