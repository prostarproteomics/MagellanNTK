
options(shiny.maxRequestSize=300*1024^2) 
options(encoding = "UTF-8")
options(shiny.fullstacktrace = TRUE)
require(compiler)
enableJIT(3)

verbose <- FALSE



#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' 
#' @noRd
#'
app_server <- function( input, output, session ) {
  
  verbose <- FALSE 
  rv <- reactiveValues(
    dataIn = NULL,
    pipeline = NULL,
    package = NULL
  )

  
  observeEvent(req(input$choosePipeline),{
    rv$pipeline$server(dataIn = reactive({rv$dataIn}))
  })
  
  
  output$showUI <- renderUI({
    req(rv$pipeline)
    rv$pipeline$ui()
  })
  
  # observeEvent(input$send,{
  #   if (input$send%%2 != 0)
  #     rv$dataIn <- hl
  #   else
  #     rv$dataIn <- NULL
  # })
}
