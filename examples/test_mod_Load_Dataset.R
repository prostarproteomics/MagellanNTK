
if (interactive()){

  library(shiny)
  options(shiny.fullstacktrace = TRUE)
  
  
  ui <- fluidPage(
    mod_Load_Dataset_ui("demo")
  )
  
  server <- function(input, output, session) {
    data(data_na)
    mod_Load_Dataset_server("demo")
  }
  
  shinyApp(ui, server)
}