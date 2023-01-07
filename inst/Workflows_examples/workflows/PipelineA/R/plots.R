
#' @export
mod_plots_ui <- function(id){
  ns <- NS(id)
  tagList(
    plotOutput(ns('plot1'))
  )
}

#' @export
mod_plots_server <- function(id, object){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
  output$plot1 <- renderPlot({
    
    hist(object)
  })
    
  })
}