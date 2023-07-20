

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
                      # selected = c(unique(data$iucnRedListCategory)),
                      selected = c("EX", "EW", "CR", "EN", "VU", "NT", "LC", "DD", "NE"),
                      options = list(`actions-box` = TRUE), multiple = T
          ),
          pickerInput("redbook", "Червона Книга України",
                      # choices = unique(data$ЧКУ),
                      choices = c("вразливий", "рідкісний", "зникаючий", "неоцінений", "недостатньо відомий", "зниклий у природі"),
                      # selected = c(unique(data$ЧКУ)),
                      selected = c("вразливий", "рідкісний", "зникаючий", "неоцінений", "недостатньо відомий", "зниклий у природі"),
                      options = list(`actions-box` = TRUE), multiple = T
                      ),
          # # TODO checkboxGroupInput with one point change to checkboxInput
          # checkboxInput("bern1", "Bern Appendix 1", TRUE), 
          # checkboxInput("bern2", "Bern Appendix 2", TRUE),
          # checkboxInput("bern3", "Bern Appendix 3", TRUE),
          # checkboxInput("bern6", "Bern Resolution 6", TRUE),
          # checkboxInput("bonn", "Bonn", TRUE), 
          # checkboxInput("aewa", "AEWA", TRUE), 
          # checkboxInput("cites", "CITES", TRUE), 
          # checkboxInput("eurobats", "EUROBATS", TRUE), 
          # checkboxInput("accobams", "ACCOBAMS", TRUE), 
          # checkboxInput("birdsdirective", "Birds Directive", TRUE), 
          # checkboxInput("habitatsdirective", "Habitats Directive", TRUE), 
          # hr(),
          # checkboxInput("invasive", "Invasive", FALSE), 

          
          checkboxGroupInput(
            inputId = "bern1",
            label = "",
            choices = c("Bern Appendix 1" = "yes"),
            selected = c("yes")
          ),checkboxGroupInput(
            inputId = "bern2",
            label = "",
            choices = c("Bern Appendix 2" = "yes"),
            selected = c("yes")
          ),
          checkboxGroupInput(
            inputId = "bern3",
            label = "",
            choices = c("Bern Appendix 3" = "yes"),
            selected = c("yes")
          ),
          checkboxGroupInput(
            inputId = "bern6",
            label = "",
            choices = c("Bern Resolution 6" = "yes"),
            selected = c("yes")
          ),
          checkboxGroupInput(
            inputId = "bonn",
            label = "",
            choices = c("Bonn" = "yes"),
            selected = c("yes")
          ),
          checkboxGroupInput(
            inputId = "aewa",
            label = "",
            choices = c("AEWA" = "yes"),
            selected = c("yes")
          ),
          checkboxGroupInput(
            inputId = "cites",
            label = "",
            choices = c("CITES" = "yes"),
            selected = c("yes")
          ),
          checkboxGroupInput(
            inputId = "eurobats",
            label = "",
            choices = c("EUROBATS" = "yes"),
            selected = c("yes")
          ),
          checkboxGroupInput(
            inputId = "accobams",
            label = "",
            choices = c("ACCOBAMS" = "yes"),
            selected = c("yes")
          ),
          checkboxGroupInput(
            inputId = "birdsdirective",
            label = "",
            choices = c("Birds Directive" = "yes"),
            selected = c("yes")
          ),
          checkboxGroupInput(
            inputId = "habitatsdirective",
            label = "",
            choices = c("Habitats Directive" = "yes"),
            selected = c("yes")
          ),
          hr(), # "horisontal line"
          checkboxGroupInput(
            inputId = "invasive",
            label = "",
            choices = c("Invasive" = "yes"),
            # selected = c("yes")
          ),

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
    # data[data$year >= input$range[1] & data$year <= input$range[2],] 
    data[data$iucnRedListCategory %in% input$iucn | 
           data$ЧКУ %in% input$redbook  | 
           data$BernAppendix1 %in% input$bern1 |
           data$BernAppendix2 %in% input$bern2 | 
           data$BernAppendix3 %in% input$bern3 | 
           data$BernResolution6 %in% input$bern6 | 
           data$Bonn %in% input$bonn | 
           data$AEWA %in% input$aewa | 
           data$CITES %in% input$cites | 
           data$EUROBATS %in% input$eurobats | 
           data$ACCOBAMS %in% input$accobams | 
           data$BirdsDirective %in% input$birdsdirective | 
           data$HabitatsDirective %in% input$habitatsdirective | 
           data$Invasive %in% input$invasive
         ,]

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
  
  
  observe({
  #   print("data_bounds:")
  #   print(data_bounds())
      print("input$bern2 :")
      print(input$bern2)
  })
  
}

shinyApp(ui, server)



