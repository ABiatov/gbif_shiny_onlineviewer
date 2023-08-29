library(shiny)
library(plotly)

if (interactive()) {
  shinyApp(
    ui = basicPage(
      actionButton("show", "Show modal dialog")
    ),
    server = function(input, output) {
      observeEvent(input$show, {
        showModal(modalDialog(
          title = "Зона тінтересу не визначена",
          "Оберіть територію інтересу (район чи ОТГ), завантажте файл або намалюйте полігон.",
          easyClose = TRUE,
          footer = tagList(
            modalButton("Закрити"),
          )
        ))
      })
    }
  )
}
  