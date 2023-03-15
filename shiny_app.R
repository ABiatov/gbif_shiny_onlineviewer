# Biodiversity Viewer v.0.1

# use https://mastering-shiny.org/action-layout.html
# https://shiny.rstudio.com/gallery/viscover.html
# https://github.com/XiaodanLyu/viscover/blob/master/inst/shiny-examples/overlay/app.r

# Import libs ####
# library(tidyverse)
library(dplyr)
library(shiny)
library(sf)
library(sp)
library(rgeos)
# library(dismo)
library(rgbif)
library(leaflet)
library(leaflet.extras)
library(DT)

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
                             )
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
  ## create the leaflet map ####
  main_map <- leaflet() %>% addTiles() %>% addSearchOSM() %>% setView(36.39, 49.65, zoom = 11) %>%
    addDrawToolbar(
      polylineOptions = FALSE,
      polygonOptions = TRUE,
      rectangleOptions = TRUE,
      circleOptions = FALSE,
      markerOptions = FALSE,
      circleMarkerOptions = FALSE,
      # markerOptions = drawMarkerOptions(markerIcon = myMarkerIcon(2)),
      singleFeature = TRUE,
      editOptions = editToolbarOptions()
    )  %>% addPolygons(data = adm_2)
  
  output$map <- renderLeaflet({ main_map })
  
  ## Draw polygon ####
  bb.poly <- eventReactive(input$map_draw_new_feature,{
    coords <- input$map_draw_new_feature$geometry$coordinates %>%
      unlist %>% matrix(nc = 2, byrow = T)
    if(nrow(coords) >= 4) {
      bb.poly <- sp::Polygon(coords) %>% list %>% sp::Polygons(ID=1) %>% list %>% sp::SpatialPolygons()
    }
  })
  
  observeEvent(input$map_draw_new_feature, {
    # print("New Feature")
    print(input$map_draw_new_feature)
    # print(str(bb.poly())) # class 'SpatialPolygons' [package "sp"]
    # print(st_as_text(bb.poly())) # Error
  })
  
  
  ## Red book table ####
  output$redbook_table <- DT::renderDataTable(df_redbook)
  
  observe({   # применяется для доступа к реактивным переменным, распечатки их в консоль и отладки
    # print(input$redbook_finder)
    # print(input$map_center)
    # print(bb.poly)
    # print(str(bb.poly)) # function
    # print(input$map_draw_new_feature)
    # print(str(input$map_draw_new_feature))
    # print(input$map_draw_new_feature$geometry)
    # print(input$map_draw_new_feature$geometry$coordinates) # list of coordinates
    # print(str(input$map_draw_new_feature$geometry$coordinates))
    # print(paste("x: ", x())) # обращение к реактивной переменно: x()
    # print(selected_zone)
    print("done")
  })
  
  
}



shinyApp(ui, server)