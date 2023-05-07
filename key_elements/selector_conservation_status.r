# setwd("C:/Mamba/Work/Presentations/2023-03_GBIF_Viewer/all_23-03-29/gbif_shiny_onlineviewer-get_data_from_gbif")

# Category selection

{
library(dplyr)
library(shiny)
library(shinyWidgets)
library(rgbif)
library(data.table)
}

load(file = "../dictionaries/red_book_vrazlyvyi.Rdata")
load(file = "../dictionaries/red_book_ridkisnyi.Rdata")
load(file = "../dictionaries/red_book_znykaiuchyi.Rdata")
load(file = "../dictionaries/red_book_znyklyi_v_pryrodi.Rdata")
load(file = "../dictionaries/red_book_znyklyi.Rdata")
load(file = "../dictionaries/red_book_nedostatno_vidomyi.Rdata")
load(file = "../dictionaries/red_book_neotsinenyi.Rdata")

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
                           ## Select Conservation status UI ####
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
                           ## Button for send GBIF request ####
                           #actionButton("act_get_gbif_data", label = "Отримати GBIF дані"), # на эту кнопку повесить запрос данных из GBIF
                         ),
                         #### Main map panel for displaying outputs ####
                         mainPanel(
                           
                         )
                       )
              ),
              ## Tab - Попередній перегляд ####
              tabPanel("Попередній перегляд", 
              #         DT::dataTableOutput("gbif_table")
              ),
              ## Tab - Генерування звітів ####              
              tabPanel("Генерування звітів",
                       
              ),
              ## Tab Червона Книга України - for testing ####              
              tabPanel("Червона Книга України",
                       # tableOutput("my_table")
              #         DT::dataTableOutput("redbook_table"),
              )
  )
)

# Back end ####
server = function(input, output, session) {
  
  ## Select Conservation status SERVER ####
  species_list <- eventReactive(eventExpr = input$redbook_finder,
                                  valueExpr = {
                                    vectodata:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABIAAAASCAYAAABWzo5XAAAAWElEQVR42mNgGPTAxsZmJsVqQApgmGw1yApwKcQiT7phRBuCzzCSDSHGMKINIeDNmWQlA2IigKJwIssQkHdINgxfmBBtGDEBS3KCxBc7pMQgMYE5c/AXPwAwSX4lV3pTWwAAAABJRU5ErkJggg==rs_list <- list(red_book_vrazlyvyi, red_book_ridkisnyi,
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
  
  
  
  ## OUTPUT #### 
  
  observe({   # применяется для доступа к реактивным переменным, распечатки их в консоль и отладки
    print("redbook_finder: ")
    print(input$redbook_finder)
    print("species_list: ")
    # print(species_list())
    print(str(species_list()))
    print(length(species_list()))
    #print("recieved_data: ")
    #print(str(recieved_data()))
    print("done")
  })
  
  
}



shinyApp(ui, server)
