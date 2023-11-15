library(shiny)
library(ggplot2)

data("iris")

# Frontend ####
ui = fluidPage(
  textInput("text", "Plot Title"), # set keys
  tableOutput("table"), # set keys
  plotOutput("plot") # set keys
)

# Beckend ####
server = function(input, output, session) {
  output$plot = renderPlot({
    ggplot()+
      geom_point(data = iris, aes(x = iris$Petal.Length, y = iris$Sepal.Width, col = iris$Species))+
      ggtitle(input$text) # name of plot
  })
  
  output$table = renderTable({
    head(iris)
  })
}

shinyApp(ui, server)
