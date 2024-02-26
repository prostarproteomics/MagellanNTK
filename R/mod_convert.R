#' @title   mod_choose_pipeline_ui and mod_choose_pipeline_server
#' @description  A shiny Module.
#' 
#' @param id shiny id
#'
#' 
#' @name mod_Convert
#' @author Samuel Wieczorek
#' 
NULL


#' @rdname mod_Convert
#' @export
#' 
convert_conf <- function(){
  
}



#' @rdname mod_Convert
#'
#' @keywords internal
#' @export
#'
#' @import shiny
#' @import shiny
#' @return NA
#'
convert_ui <- function(id) {
  ns <- NS(id)
  h3(style="color: blue;", 'Default convert module')
}



#' @export
#' @import shiny
#' @rdname mod_Convert
#'
convert_server <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    return(reactive({NULL}))
  })

}



#---------------------------------------------

ui <- fluidPage(
  convert_ui("qf_file")
)


server <- function(input, output, session) {
  convert_server("qf_file")
}

shinyApp(ui, server)
