if (interactive()){
  
  library(shiny)
  library(shinyjs)
  library(shinydashboard)
  library(shinyWidgets)
  library(shinyFiles)
  options(shiny.fullstacktrace = TRUE)
  
  path <- '/home/samuel/Github/MagellanNTK/inst/Workflow_examples'
  
  
  ui <- dashboardPage(
    dashboardHeader(title="MagellanNTK"),
    dashboardSidebar(),
    dashboardBody(
      uiOutput("openFileUI"),
      uiOutput('show')
    )
  )
  
  server <- function(input, output) {
    
    rv <- reactiveValues(
      workflow = NULL,
      folder = NULL
    )
    
    tmp <- mod_load_workflow_server("demo")
    
    observe({
      rv$workflow <- tmp$workflow()
      rv$folder <- tmp$folder()
      
    })
    
    output$openFileUI <- renderUI({
      tmp <- mod_load_workflow_server(id = "demo", 
                                      mode = reactive({"dev"})
                                      )
      
      mod_load_workflow_ui("demo")
    })
    
    output$show <- renderUI({
      tagList(
        h3(rv$folder),
        h3(rv$workflow)
      )
      
    })
  }
  
  shinyApp(ui, server)
}