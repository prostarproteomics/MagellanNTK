#' @title mod_open_demo_dataset_ui and mod_open_demo_dataset_server
#' 
#' @description A shiny Module.
#' 
#' @param id xxx
#' 
#' @name infos_dataset
#'
#' @keywords internal
#' 
NULL




#' @export 
#' @rdname infos_dataset
#' @importFrom shiny NS tagList 
#' @import shinyjs
#' 
infos_dataset_ui <- function(id){
  ns <- NS(id)
  tagList(
    h3(style="color: blue;", 'Default info dataset module')
  )
}


#' @rdname infos_dataset
#' 
#' @export
#' @importFrom BiocGenerics get
#' @importFrom utils data
#' @importFrom shinyjs info
#' 
infos_dataset_server <- function(id, 
                                 obj = reactive({NULL})){
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    
  })
  
}




###################################################################
##                                                               ##
##                                                               ##
###################################################################

library(shiny)

ui <- infos_dataset_ui("infos")


server <- function(input, output, session) {
  data(Exp1_R25_prot, package='DaparToolshedData')
  obj <- Exp1_R25_prot
  infos_dataset_server("infos", obj = reactive({obj}))
}

shinyApp(ui = ui, server = server)
