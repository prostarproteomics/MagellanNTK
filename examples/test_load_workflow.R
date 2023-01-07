#if (interactive()){
  
  library(shiny)
  library(shinyFiles)
  library(shinyTree)
  options(shiny.fullstacktrace = TRUE)
  
  
  ui <- fluidPage(
    tagList(
      Load_Workflow_ui("demo"),
    uiOutput('show')
    )
  )
  
  server <- function(input, output) {
    rv <- reactiveVal()
    
    rv <- Load_Workflow_server("demo")
    
    output$show <- renderUI({
      browser()
      tagList(
        h3(rv()$folder),
        h3(rv()$workflow)
      )
      
    })
  }
  
  shinyApp(ui, server)
#}
