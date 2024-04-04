#' @title mod_open_demo_dataset_ui and mod_open_demo_dataset_server
#' 
#' @description  A shiny Module.
#' 
#' @param id xxx
#' @param obj An instance of class xxx
#' @param ... Additional parameters
#' 
#' @name view_dataset
#'
#' @keywords internal
#' 
#' @examples
#' if(interactive()){
#' shiny::runApp(view_dataset(sub_R25))}
#' 
#' 
NULL




#' @export 
#' @rdname view_dataset
#' @importFrom shiny NS tagList h3
#' @import shinyjs
#' 
view_dataset_ui <- function(id){
  ns <- NS(id)
  tagList(
    h3(style="color: blue;", 'Default view_dataset module')
  )
}


#' @rdname view_dataset
#'  
#' @export
#' @importFrom shiny moduleServer reactiveValues reactive
#' 
view_dataset_server <- function(id, obj = NULL, ...){
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    rv.openDemo <- reactiveValues(
      dataRead = NULL,
      dataOut = NULL
    )
    
    reactive({rv.openDemo$dataOut})
  })
  
}



#' @export
#' @rdname view_dataset
#' @importFrom shiny shinyApp reactiveValues reactive
#' 
view_dataset <- function(obj, ...){

ui <- view_dataset_ui("demo")


server <- function(input, output, session) {
  rv <- reactiveValues(
    obj = NULL
  )
  
  rv$obj <- view_dataset_server("demo",
    obj = reactive({obj}),
    ...
    )
  
}

app <- shinyApp(ui = ui, server = server)
}
