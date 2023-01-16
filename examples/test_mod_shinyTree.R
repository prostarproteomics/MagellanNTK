#if (interactive()){
  
  library(shiny)
  library(shinyTree)
  options(shiny.fullstacktrace = TRUE)
  
  
  ui <- fluidPage(
    tagList(
      mod_shinyTree_ui("tree"),
      uiOutput('show_selected')
    )
  )
  
  server <- function(input, output) {
    
    rv <- reactiveValues(
      selected = NULL
    )
    
   path <- "/home/samuel/Github/MagellanNTK/inst/Workflows_examples/workflows"
   rv$selected <- mod_shinyTree_server("tree", path = reactive({path}))
   
   output$show_selected <- renderUI({
     h3(rv$selected())
   })
  }
  
  shinyApp(ui, server)
#}
