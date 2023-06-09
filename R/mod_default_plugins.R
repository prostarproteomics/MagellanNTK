
#' @title xxxx
#' 
#' @description xx
#' 
#' @param id xxxx
#' @param path xxx
#' @param object xxx
#' 
#' 
#' @name default_plugin
#' 
NULL



#' @rdname default_plugin
#' @export
#' 
default_openfile_ui <- function(id){
  ns <- NS(id)
  tagList(
    h3('Default open file plugin')
  )
}


#' @rdname default_plugin
#' @export
#' 
default_openfile_server <- function(id,  path = reactive({NULL})){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    rv <- reactiveValues(
      dataOut = NULL
    )
    
    
    return(reactive({rv$dataOut}))
  })
}






#' @export
#' @rdname default_plugin
#' 
default_EDA_ui <- function(id){
  ns <- NS(id)
  tagList(
    h3('Default EDA plugin')
  )
    
}

#' @export
#' @rdname default_plugin
#' 
default_EDA_server <- function(id, object){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    apModal_server(
      id = "tbl",
      title = "test",
      uiContent = p("test")
    )
  })
}








#' @export
#' @rdname default_plugin
#' 
default_export_ui <- function(id){
  ns <- NS(id)
  tagList(
    h3('This is the default export plugin')
  )
}

#' @export
#' @rdname default_plugin
#' 
default_export_server <- function(id, object){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
    
  })
}

