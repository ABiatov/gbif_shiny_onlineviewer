shinyUI(fluidPage(
  titlePanel("mtcars data"),
  sidebarLayout(
    sidebarPanel(
      textInput("title", "Plot title:",
                value = "x v y"),
      selectInput("x", "Choose an x var:",
                  choices = names(mtcars),
                  selected = "disp"),
      selectInput("y", "Choose a y var:",
                  choices = names(mtcars),
                  selected = "mpg")
    ),
    mainPanel(
      h3(textOutput("text")),
      plotOutput("plot")
    )
  )
))