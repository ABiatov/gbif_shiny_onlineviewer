# conspect of https://youtu.be/jwnWnoNcMOA

library(shiny)
require(colourpicker)

# UI ####
ui <- fluidPage(
#   ## HTML Marking  ####
#   # headers h1 - h6
#   h1("Header level 1"),
#   h2("Header level 2"),
#   h3("Header level 3"),
#   h4("Header level 4"),
#   strong("Strong text"),
#   em("kursive text"),
#   br(),
#   "simple text",
#   hr(),
#   ## HTML Builder functions
#  h1("HTML Builder functions"),
#  p("paragraph"),
#  a("a() creation links"),
#  br("перенос строки"),
#  hr("horisontal line"),
#   # 
# #  div("блочный элемент"),
#   span("строчный элемент"),
#   pre("предварительно форматированный текст (моноширный блок)"),
#   code("Моноширный шрифт (отдельные строки кода)")
# #  img("вставка изображения"),
# #  tags("среда для вспомогательных тегов из HTML5"),
# #  tag("для тегов которые не входят в HTML5")

## Sidebar Layout ####
  # sidebarLayout(
  #   sidebarPanel("This is the sidebar"),
  #   mainPanel("Main panel goes here")
  # )
## Inputs ####  
  h2("Text input"), 
  textInput(inputId = "name", 
            label = "Введите ваше имя",  
            width = "30%", # can be integer
            placeholder = "Enter your name"
            # value = "Anton" для указания значения по умолчанию вместо placeholder
            ), 
  h2("Numeric input"), 
  numericInput(inputId = "num_rows", 
               label = "Количество строк", 
               width = "100",
               value = 7, 
               min = 0, 
               max = 100,
               step = 2 # шаг измерений
               ),
  # CheckboxInput
  checkboxInput(
    inputId = "agree", 
    label = "Я прочитал и согласен с правилами",
    value = FALSE,
    width = 400
  ),

  # sliderInput
  sliderInput(
    inputId = "slider",
    label = "Выбор числа",
    value = c(12, 38), 
    min = 0, 
    max = 50,
    step = 0.5,
    animate = FALSE, # диапазон може сам ползти по слайдеру 
    width = 600
  ),

  # radioButtons
  radioButtons(
    inputId = "radio",
    label = "Любимое время",
    choices = c("Утро", "День", "Вечер", "Ночь"),
    selected = "День"
  ),

  # selectInput можно работать с большим выпадающим списком, работает подсказака при вводе
  # можно использовать при выборе набора охранных статусов
  selectInput(
    inputId = "select",
    label = "Любимое время",
    choices = c("Утро", "День", "Вечер", "Ночь"),
    selected = "День",
    multiple = TRUE,
    width = 200
  ),
  
# select color. Use library colourpicker
  colourInput(
    inputId = "col",
    label = "couse colour",
    value = "green"
  ),

## Outputs ####
#  h2("Объект будет размещен здесь"), *Outpu(outputId = "name_id"),
#  h2("Место для графика"), plotOutput(outputId = "plot")
  plotOutput("my_plot"),
  tableOutput("my_table")
)

# Server ####
server <- function(input, output) {
  # code for generation outputs
  output$my_table <- renderTable(head(iris, n = input$num_rows)) # can be renderText(), renderPlot(), renderTable() etc.
  output$my_plot <- renderPlot({plot(rnorm(input$num_rows))})
  
  x <- reactive({
    input$num_rows + 1
  })
  observe({   # применяется для доступа к реактивным переменным, распечатки их в консоль и отладки
    print(input$num_rows)
    print(paste("x: ", x())) # обращение к реактивной переменно: x()
    print(input$name)
    print(input$agree)
    print(paste("radio: ", input$radio))
    print(paste("select: ", input$select))
    print(paste("colour: ", input$col))
  })
  # 
}

shinyApp(
  ui = ui,
  server = server
)

