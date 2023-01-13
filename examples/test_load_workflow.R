#if (interactive()){
  
  library(shiny)
  library(shinyFiles)
  library(shinyTree)
  options(shiny.fullstacktrace = TRUE)
  
  path <- '/home/samuel/Github/MagellanNTK/inst/Workflow_examples'
  ui <- dashboardPage(
    dashboardHeader(title="MagellanNTK"),
    dashboardSidebar(),
    dashboardBody(
      mod_load_workflow_ui("demo"),
      uiOutput('show')
    )
  )
  
  server <- function(input, output) {
    
    tmp <- mod_load_workflow_server("demo")
    
    output$show <- renderUI({
      req(tmp$folder())
      tagList(
        h3(tmp$folder()),
        h3(tmp$workflow())
      )
      
    })
  }
  
  shinyApp(ui, server)
#}
