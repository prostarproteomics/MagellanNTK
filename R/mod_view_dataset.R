#' @title mod_open_demo_dataset_ui and mod_open_demo_dataset_server
#' 
#' @description  A shiny Module.
#' 
#' @param id xxx
#' @param obj
#' 
#' @name view_dataset
#'
#' @keywords internal
#' 
NULL




#' @export 
#' @rdname view_dataset
#' @importFrom shiny NS tagList 
#' @import shinyjs
#' @import DaparViz
#' 
view_dataset_ui <- function(id){
  ns <- NS(id)
  tagList(
    h3(style="color: blue;", '-- Default demo dataset module --')
  )
}


#' @rdname view_dataset
#'  
#' @export
#' @importFrom BiocGenerics get
#' @importFrom utils data
#' @importFrom shinyjs info
#' 
view_dataset_server <- function(id, obj = NULL){
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    rv.openDemo <- reactiveValues(
      dataRead = NULL,
      dataOut = NULL
    )
    
    reactive({rv.openDemo$dataOut })
  })
  
}




###################################################################
##                                                               ##
##                                                               ##
###################################################################

library(shiny)

ui <- view_dataset_ui("demo")


server <- function(input, output, session) {
  rv <- reactiveValues(
    obj = NULL
  )
  
  rv$obj <- view_dataset_server("demo")
  
}

shinyApp(ui = ui, server = server)
