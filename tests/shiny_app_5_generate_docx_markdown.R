library(shiny)
library(dplyr)
library(DT)

library(rmarkdown)
library(knitr)

ui <- shinyUI(fluidRow(
  column(10,
         h2('Data Output'),
         DT::DTOutput("images"),
         downloadButton("report", "Generate report")
  )
))

server <- shinyServer(function(input, output, session) {
  dat <- reactiveValues(images = data_frame("photographer" = c('A','B'),
                                            "organisation" = 'The Org',
                                            "location" = 'The Place',
                                            "address" = c('2 some place', '3 that place'))
  )
  
  output$images <- DT::renderDT(dat$images,
                                server = FALSE,
                                rownames = FALSE,
                                class = 'dt-body nowrap',
                                selection = 'single', editable = TRUE,
                                extensions = c('Buttons','Responsive'),
                                options = list(pageLength = 5,
                                               dom = 'Blfrtip',
                                               buttons = c('csv', 'excel')))
  
  
  date <- format(Sys.Date(), "%Y-%m-%d")
  text_with_date <- paste("Звіт згенеровано", date)
  text_with_date
  
  numeric_value <- 10
  
  output$report <- downloadHandler(
    filename = function() {
      paste("GBIF_data-", Sys.Date(), ".docx", sep="")
    },
    content = function(file) {
      # Code to generate the DOCX file
      
      rmarkdown::render(
        input = "templates/report.Rmd",
        output_format = "word_document",
        output_file = file,
        # params = list(df = dat$images)
      )
      
      
    }
  )
})

shinyApp(ui = ui, server = server)
