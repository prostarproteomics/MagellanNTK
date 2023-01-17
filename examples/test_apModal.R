
#if (interactive()) {
library(shiny)
library(shinyBS)

ui <- fluidPage(
  tagList(
    apModal_ui("tbl")
    
    
  )
)
server <- function(input, output) {
  apModal_server(
    id = "tbl",
    title = "test",
    uiContent = p("test")
  )}
shinyApp(ui, server)
# }