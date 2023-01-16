
#if (interactive()){

  library(shiny)
  options(shiny.fullstacktrace = TRUE)
  
  
  ui <- fluidPage(
    mod_openfile_ui("demo")
  )
  
  server <- function(input, output, session) {
    data(data_na)
    mod_openfile_server("demo", path = reactive({NULL}))
  }
  
  shinyApp(ui, server)
#}