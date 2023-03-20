# created by this video https://youtu.be/SKC9en1e5as

# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(palmerpenguins)
# unique(penguins$species)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Пингвины Палмера"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(selectInput("species",
                                 "Выбор вида пингвина",
                                 choices = c("Adelie",
                                             "Gentoo",
                                             "Chinstrap")),
                     numericInput("num", 
                                  "Выберите количество строк", 
                                  value = 5)

        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("plot"),
           tableOutput("table")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  my_tab <- reactiveVal() # Create a (single) reactive value which can see all time (global variables)
  
  observeEvent(input$species, {  # observe reactive input input$species
    filtred_data <- subset(penguins, species == input$species)
    my_tab(filtred_data) # write result in my custom reactive value
  })
  
  output$plot <- renderPlot({ggplot(my_tab(), aes(bill_length_mm, bill_depth_mm)) + # using my reactive value my_tab() in code
    geom_point()})
  output$table <- renderTable({head(my_tab(), n = input$num)})
  
  
  # show variables for debugging in terminal
  observe({
    print("my tab:")
    print(head(my_tab()))
  })
  
}



# Run the application 
shinyApp(ui = ui, server = server)
