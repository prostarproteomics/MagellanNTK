#' @title Export dataset Shiyny app
#' 
#' @description  A shiny Module.
#' 
#' @param id xxx
#' 
#' @name mod_export_dataset
#' 
#' @examplesIf interactive()
#' data(lldata)
#' shiny::runApp(export_dataset(lldata))
#' 
#' @return A list
#' 
NULL




#' @export 
#' @rdname mod_export_dataset
#' @import shiny
#' 
export_dataset_ui <- function(id){
  ns <- NS(id)
  tagList(
    h3(style="color: blue;", 'Export dataset (default)'),
    actionButton(ns('export_btn'), 'Export')
  )
}


#' @rdname mod_export_dataset
#' 
#' @export
#' @importFrom shinyjs info 
#' @importFrom shiny moduleServer reactiveValues observeEvent
#' 
export_dataset_server <- function(id, dataIn){
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    
    ## -- Open a MSnset File --------------------------------------------
    observeEvent(input$export_btn, ignoreInit = TRUE, {
      
      
    })
    
  })
  
}




#' @rdname mod_export_dataset
#' 
#' @export
#' @importFrom shiny fluidPage tagList textOutput reactiveValues observeEvent
#' shinyApp
#' 
export_dataset <- function(data){
  ui <- fluidPage(
    tagList(
      export_dataset_ui("export")
    )
  )
  
  server <- function(input, output, session) {
    
    export_dataset_server("export",
      dataIn = reactive({data})
    )
  }
  
  app <- shiny::shinyApp(ui, server)
}


