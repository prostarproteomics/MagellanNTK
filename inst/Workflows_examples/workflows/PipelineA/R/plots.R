
#' @export
custom_plots_ui <- function(id){
  ns <- NS(id)
  tagList(
    h('Default plot plugin')
  )
}

#' @export
custom_plots_server <- function(id, object){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
  
    
  })
}