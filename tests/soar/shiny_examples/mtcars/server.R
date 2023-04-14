# load libraries, scripts, data
shinyServer(function(input, output) {
# make user specific variables

  output$text <- renderText({
    input$title
  })
 
  output$plot <- renderPlot({
    x <- mtcars[ , input$x]
    y <- mtcars[ , input$y]
    plot(x, y, pch = 16)
  })

})