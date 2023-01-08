
#' @title xxxx
#' 
#' @description xx
#' 
#' @param id xxxx
#' @param path xxx
#' 
#' @name default_plugin
#' 
NULL



#' @rdname default_plugin
#' @export
#' 
mod_openfile_ui <- function(id){
  ns <- NS(id)
  tagList(
    h3('Default open file plugin')
  )
}


#' @rdname default_plugin
#' @export
#' 
mod_openfile_server <- function(id,  path = reactive({NULL})){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}






#' @export
#' @rdname default_plugin
#' 
mod_plots_ui <- function(id){
  ns <- NS(id)
  tagList(
    h3('Default plot plugin')
  )
}

#' @export
#' @rdname default_plugin
#' 
mod_plots_server <- function(id, object){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
    
  })
}








#' @export
#' @rdname default_plugin
#' 
mod_export_ui <- function(id){
  ns <- NS(id)
  tagList(
    h3('This is the default export plugin')
  )
}

#' @export
#' @rdname default_plugin
#' 
mod_export_server <- function(id, object){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
    
  })
}









#' @export
#' @rdname default_plugin
#' 
mod_convert_ui <- function(id){
  ns <- NS(id)
  tagList(
    h3('This is the default convert plugin')
  )
}

#' @export
#' @rdname default_plugin
#' 
mod_convert_server <- function(id, object){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
    
  })
}

