if(interactive()){
  library(shiny)
  library(shinydashboard)
  library(shinyjs)
  options(shiny.fullstacktrace = TRUE)
  
  app <- shinyApp(ui, server)
}