
rm(list = ls())
gc()

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
  # "CITES",
  "EUROBATS",
  "ACCOBAMS",
  "Birds Directive",
  "Habitats Directive"
)

vect_region_redlist <- c(
  "ЧС_Вінницька",
  "ЧС_Волинська",
  "ЧС_Дніпропетровська",
  "ЧС_Донецька",
  "ЧС_Житомирська",
  "ЧС_Закарпатська",
  "ЧС_Запорізька",
  "ЧС_Івано_Франківська",
  "ЧС_Київська",
  "ЧС_Кіровоградська",
  "ЧС_Луганська",
  "ЧС_Львівська",
  "ЧС_Миколаївська",
  "ЧС_Одеська",
  "ЧС_Полтавська",
  "ЧС_Рівненська",
  "ЧС_Сумська",
  "ЧС_Тернопільська",
  "ЧС_Черкаська",
  "ЧС_Чернівецька",
  "ЧС_Чернігівська",
  "ЧС_Харківська",
  "ЧС_Херсонська",
  "ЧС_Хмельницька", 
  "ЧС_Київ",
  "ЧС_Севастополь"
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
                        # "Конвенція про міжнародну торгівлю видами дикої фауни і флори, що перебувають під загрозою зникнення (CITES)" = "CITES",
                        "Угода про збереження популяцій європейських кажанів (EUROBATS)" = "EUROBATS",
                        "Угода про збереження китоподібних Чорного моря, Середземного моря та прилеглої акваторії Атлантичного океану (ACCOBAMS)" = "ACCOBAMS",
                        "Пташина директива ЄС" = "Birds Directive",
                        "Оселищна директива ЄС" = "Habitats Directive"
                      ),
                      selected = vector_conventions,
                      options = list(`actions-box` = TRUE), multiple = TRUE
          ),
          pickerInput("region_redlist_filters", "Обласні червоні списки",
                      choices = c(
                        "Вінницька обл." = "ЧС_Вінницька",
                        "Волинська обл." = "ЧС_Волинська",
                        "Дніпропетровська обл." = "ЧС_Дніпропетровська",
                        "Донецька обл." = "ЧС_Донецька",
                        "Житомирська обл." = "ЧС_Житомирська",
                        "Закарпатська обл." = "ЧС_Закарпатська",
                        "Запорізька обл." = "ЧС_Запорізька",
                        "Івано-Франківська обл." = "ЧС_Івано_Франківська",
                        "Київська обл." = "ЧС_Київська",
                        "Кіровоградська обл." = "ЧС_Кіровоградська",
                        "Луганська обл." = "ЧС_Луганська",
                        "Львівська обл." = "ЧС_Львівська",
                        "Миколаївська обл." = "ЧС_Миколаївська",
                        "Одеська обл." = "ЧС_Одеська",
                        "Полтавська обл." = "ЧС_Полтавська",
                        "Рівненська обл." = "ЧС_Рівненська",
                        "Сумська обл." = "ЧС_Сумська",
                        "Тернопільська обл." = "ЧС_Тернопільська",
                        "Черкаська обл." = "ЧС_Черкаська",
                        "Чернівецька обл." = "ЧС_Чернівецька",
                        "Чернігівська обл." = "ЧС_Чернігівська",
                        "Харківська обл." = "ЧС_Харківська",
                        "Херсонська обл." = "ЧС_Херсонська",
                        "Хмельницька обл." = "ЧС_Хмельницька", 
                        "м. Київ" = "ЧС_Київ",
                        "м. Севастополь" = "ЧС_Севастополь"
                      ),
                      # selected = vect_region_redlist,
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
  
  region_filt_present <- reactive( vect_region_redlist %in% input$region_redlist_filters )
  
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
                       # (intern_filt_present()[7] & CITES == "yes") |
                       (intern_filt_present()[8] & EUROBATS == "yes") |
                       (intern_filt_present()[9] & ACCOBAMS == "yes") |
                       (intern_filt_present()[10] & BirdsDirective == "yes") |
                       (intern_filt_present()[11] & HabitatsDirective == "yes") |
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
    # print("input_international_filters: ")
    # print(input$international_filters)
    # print(if ("Bern Appendix 2" %in% input$international_filters) "yes" else "no" )
    # print("international_filters_present:")
    # print(intern_filt_present())
    print("region_filt_present :")
    print(region_filt_present())
    
    

  })
  
}

shinyApp(ui, server)
