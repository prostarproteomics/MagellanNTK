if (interactive()){
  
  library(shiny)
  library(shinyFiles)
  options(shiny.fullstacktrace = TRUE)
  
  
  ui <- fluidPage(
    Load_Workflow_ui("demo")
  )
  
  server <- function(input, output) {
    Load_Workflow_server("demo")
  }
  
  shinyApp(ui, server)
}
