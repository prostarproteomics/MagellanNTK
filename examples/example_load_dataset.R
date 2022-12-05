library(shiny)

data(iris)
ui <- fluidPage(
  mod_Load_Dataset_ui("load"),
  
  uiOutput('dataloaded')
)

server <- function(input, output) {
  rv <- reactiveValues(dataset = NULL)
  rv$dataset <-mod_Load_Dataset_server(id = "load")
  
  
  output$dataloaded <- renderUI({
    req(rv$dataset)
    h3('Dataset is loaded.')
  })
  
  }

shinyApp(ui, server)
