library(shiny)

## Only run examples in interactive R sessions
if (interactive()) {
  
  ui <- fluidPage(
    title = 'Download a PDF report',
    sidebarLayout(
      sidebarPanel(
        helpText(),
        selectInput('x', 'Build a regression model of mpg against:',
                    choices = names(mtcars)[-1]),
        radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'),
                     inline = TRUE),
        downloadButton('downloadReport')
      ),
      mainPanel(
        plotOutput('regPlot')
      )
    )
  )
  
  server <- function(input, output) {
    output$contents <- renderTable({
      file <- input$file1
      ext <- tools::file_ext(file$datapath)
      
      req(file)
      validate(need(ext == "csv", "Please upload a csv file"))
      
      read.csv(file$datapath, header = input$header)
    })
  }
  
  shinyApp(ui, server)
}
