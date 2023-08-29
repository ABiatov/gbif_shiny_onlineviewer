library(shiny)
load("~/GitHub/gbif_shiny_onlineviewer/name_lookup/outputs/gbif_sf_dataset.Rdata")

## Only run this example in interactive R sessions
if (interactive()) {
  ui <- fluidPage(
    tags$head(
      tags$style(type="text/css",
        "
        #loadmessage {
          position: fixed;
          top: 0px;
          left: 0px;
          width: 100%;
          height: 100%;
          display: flex;
          justify-content: center;
          align-items: center;
          background-color: rgba(0, 0, 0, 0.4);
          z-index: 105;
        }
        #loadmessagetext {
          color: #ffff00;
        }

        "
      )
    ),
    sidebarPanel(
      selectInput("plotType", "Plot Type",
                  c(Scatter = "scatter", Histogram = "hist")
      ),
      # Only show this panel if the plot type is a histogram
      conditionalPanel(
        condition = "input.plotType == 'hist'",
        selectInput(
          "breaks", "Breaks",
          c("Sturges", "Scott", "Freedman-Diaconis", "[Custom]" = "custom")
        ),
        # Only show this panel if Custom is selected
        conditionalPanel(
          condition = "input.breaks == 'custom'",
          sliderInput("breakCount", "Break Count", min = 1, max = 50, value = 10)
        )
      )
    ),
    mainPanel(
      plotOutput("plot")
    ),
    
    conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                     tags$div(
                       tags$h3("Зачекайте будьласка, дані завантажуються...", id="loadmessagetext")
                       ,id="loadmessage")
    )
    
  )
  
  server <- function(input, output) {
    # x <- rnorm(1000000)
    x <- gbif_sf_dataset$Longitude
    # y <- rnorm(1000000)
    y <- gbif_sf_dataset$Latitude
    
    output$plot <- renderPlot({
      if (input$plotType == "scatter") {
        plot(x, y)
      } else {
        breaks <- input$breaks
        if (breaks == "custom") {
          breaks <- input$breakCount
        }
        
        hist(x, breaks = breaks)
      }
    })
  }
  
  shinyApp(ui, server)
}

