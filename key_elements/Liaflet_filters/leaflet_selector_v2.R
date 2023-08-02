
load("~/GitHub/gbif_shiny_onlineviewer/key_elements/Liaflet_filters/data.Rdata")

library(shiny)
library(shinyWidgets)
library(sf)
library(dplyr)
library(leaflet)

vector_conventions <- c(
  "Bern Appendix 1",
  "Bern Appendix 2",
  "Bern Appendix 3",
  "Bern Resolution 6",
  "Bonn",
  "AEWA",
  "CITES",
  "EUROBATS",
  "ACCOBAMS",
  "Birds Directive",
  "Habitats Directive"
)

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
          pickerInput("redbook", "Червона Книга України",
                      choices = c("вразливий", "рідкісний", "зникаючий", "неоцінений", "недостатньо відомий", "зниклий у природі"),
                      selected = c("вразливий", "рідкісний", "зникаючий", "неоцінений", "недостатньо відомий", "зниклий у природі"),
                      options = list(`actions-box` = TRUE), multiple = TRUE
          ),
          pickerInput("iucn", "Червоний список IUCN",
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
                      options = list(`actions-box` = TRUE), multiple = TRUE
          ),
          pickerInput("international_filters", "Міжнародні конвенції та угоди",
                      choices = c(
                        "Бернська конвенція. Додаток 1" = "Bern Appendix 1",
                        "Бернська конвенція. Додаток 2" = "Bern Appendix 2",
                        "Бернська конвенція. Додаток 3" = "Bern Appendix 3",
                        "Бернська конвенція. Резолюцію 6" = "Bern Resolution 6",
                        "Конвенція про збереження мігруючих видів диких тварин (Боннська конвенція)" = "Bonn",
                        "Угода про збереження афро-євразійських мігруючих водно-болотних птахів (AEWA)" = "AEWA",
                        "Конвенція про міжнародну торгівлю видами дикої фауни і флори, що перебувають під загрозою зникнення (CITES)" = "CITES",
                        "Угода про збереження популяцій європейських кажанів (EUROBATS)" = "EUROBATS",
                        "Угода про збереження китоподібних Чорного моря, Середземного моря та прилеглої акваторії Атлантичного океану (ACCOBAMS)" = "ACCOBAMS",
                        "Пташина директива ЄС" = "Birds Directive",
                        "Оселищна директива ЄС" = "Habitats Directive"
                      ),
                      selected = vector_conventions,
                      options = list(`actions-box` = TRUE), multiple = TRUE
          ),
          hr(),
          checkboxInput("invasive", "Інвазивні / інвазійні / чужорідні види", FALSE),
          actionButton("refresh_filters", "Застосувати фільтри", icon("refresh"), class = "btn-success"),
        ),
        
        mainPanel(
          leafletOutput("map2",  width = "100%", height="83vh"),
        )
      ),
    )
  )
)

server <- function(input, output, session) {
  
  intern_filt_present <- reactive( vector_conventions %in% input$international_filters )
  
  filteredData <- reactive({
    input$refresh_filters
    
    isolate(data %>%
              filter(iucnRedListCategory %in% input$iucn | 
                       ЧКУ %in% input$redbook | 
                       (intern_filt_present()[1] & BernAppendix1 == "yes" ) |
                       (intern_filt_present()[2] & BernAppendix2 == "yes" ) |
                       (intern_filt_present()[3] & BernAppendix3 == "yes" ) |
                       (intern_filt_present()[4] & BernResolution6 == "yes" ) |
                       (intern_filt_present()[5] & Bonn == "yes") |
                       (intern_filt_present()[6] & AEWA == "yes") |
                       (intern_filt_present()[7] & CITES == "yes") |
                       (intern_filt_present()[8] & EUROBATS == "yes") |
                       (intern_filt_present()[9] & ACCOBAMS == "yes") |
                       (intern_filt_present()[10] & BirdsDirective == "yes") |
                       (intern_filt_present()[11] & HabitatsDirective == "yes")|
                       (input$invasive & Invasive == "yes")

              ) 
    )
  })
  
  data_bounds <- data %>% st_bbox() %>% as.character()
  
  # data_bounds <- reactive(filteredData() %>% st_bbox() %>% as.character())
  
  main_map2 <-  leaflet() %>% addTiles()
  
  output$map2 <- renderLeaflet({
    main_map2
  })
  
  # create map proxy to make further changes to the existing map
  map2 <- leafletProxy("map2", session)
  
  observe({
    map2 %>%
      clearShapes() %>%
      clearMarkers() %>%
      fitBounds(
        lng1 = data_bounds[1], lat1 = data_bounds[2], # set view by extent: p1 - top lext, p2 - bottom right
        lng2 = data_bounds[3], lat2 = data_bounds[4] # extent is set after the selection of oblast
      ) %>%
      addCircleMarkers(data = filteredData(), 
                       radius = 2,
                       color = "red",
                       popup = ~paste0("<center>" ,"<b>", nameUk, "</b>", "</center>", # "<br>",   # popup with HTML 
                                       "<center>", scientificName, "</center>")
      )
  })
  
  
  observe({   # применяется для доступа к реактивным переменным, распечатки их в консоль и отладки
    print("input_international_filters: ")
    print(input$international_filters)
    print(if ("Bern Appendix 2" %in% input$international_filters) "yes" else "no" )
    print("international_filters_present:")
    print(intern_filt_present())

  })
  
}

shinyApp(ui, server)
