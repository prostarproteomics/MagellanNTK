#' @title convert_dataset_ui and convert_dataset_server
#' 
#' @description  A shiny Module.
#' 
#' @param id xxx
#' 
#' @name mod_convert_dataset
#'
#' @keywords internal
#' 
#' @examples 
#' if (interactive()){
#' shiny::runApp(convert_dataset())
#' }
#' 
#' @return A list
#' 
NULL




#' @export 
#' @rdname mod_convert_dataset
#' @import shiny
#' 
convert_dataset_ui <- function(id){
  ns <- NS(id)
  tagList(
    h3(style="color: blue;", '-- Default convert dataset module --')
  )
}


#' @rdname mod_convert_dataset
#' 
#' @export
#' @importFrom shinyjs info 
#' @importFrom shiny moduleServer reactiveValues observeEvent
#' 
convert_dataset_server <- function(id){
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    rv.open <- reactiveValues(
      dataRead = NULL,
      dataOut = NULL
    )
    
    reactive({rv.open$dataOut})
  })
  
}






#' @rdname mod_convert_dataset
#' 
#' @export
#' @importFrom shiny fluidPage tagList textOutput reactiveValues observeEvent
#' shinyApp
#' 
convert_dataset <- function(){
  ui <- fluidPage(
    tagList(
      convert_dataset_ui("qf_file"),
      textOutput('res')
    )
  )
  
  server <- function(input, output, session) {
    rv <- reactiveValues(
      obj = NULL,
      result = NULL
    )
    
    
    rv$result <- convert_dataset_server("qf_file")
    
    observeEvent(req(rv$result()), {
      rv$obj <- rv$result()
      print(rv$obj)
    })
    
  }
  
  app <- shiny::shinyApp(ui, server)
}


