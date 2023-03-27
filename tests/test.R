library(shiny)
library(ggplot2)

## Only run examples in interactive R sessions
if (interactive()) {
  
  ## App 1: Sample usage
  shinyApp(
    ui = fluidPage(
      column(4,
             numericInput("x", "Value", 5),
             br(),
             actionButton("button", "Show")
      ),
      column(8, tableOutput("table"))
    ),
    server = function(input, output) {
      # Take an action every time button is pressed;
      # here, we just print a message to the console
      observeEvent(input$button, {
        cat("Showing", input$x, "rows\n")
      })
      # The observeEvent() above is equivalent to:
      # observe({
      #    cat("Showing", input$x, "rows\n")
      #   }) %>%
      #   bindEvent(input$button)
      
      # Take a reactive dependency on input$button, but
      # not on any of the stuff inside the function
      df <- eventReactive(input$button, {
        head(cars, input$x)
      })
      output$table <- renderTable({
        df()
      })
    }
  )
}  


