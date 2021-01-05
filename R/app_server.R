
options(shiny.maxRequestSize=300*1024^2) 
options(encoding = "UTF-8")
options(shiny.fullstacktrace = T)
require(compiler)
enableJIT(3)

verbose <- F

AddItemToDataset <- function(dataset, name){
  addAssay(dataset, 
           dataset[[length(dataset)]], 
           name=name)
}


#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom utils data
#' 
#' @noRd
app_server <- function( input, output, session ) {
  # Get a QFeatures dataset for example
  data(hlpsms, package='QFeatures')
  hl <- QFeatures::readQFeatures(hlpsms, ecol = 1:10, name = "psms")
  require(MSPipelines)
  
  verbose <- F 
  rv <- reactiveValues(
    dataIn = NULL,
    pipeline = NULL,
    package = NULL
  )
  
  output$choosePipeline_ui <- renderUI({
    .choices <- c('None'='')
    if (!is.null(rv$package)){
      library(rv$package, character.only = TRUE)
      .choices <- c('None'='',  Pipelines())
    }
    
    selectInput('choosePipeline', 'Select a pipeline', 
                choices = .choices)
  })

  observeEvent(input$validPackage, {rv$package <- input$choosePackage  })
  
  observeEvent(req(input$choosePipeline),{
    rv$pipeline <- base::get(input$choosePipeline)$new('App')
    rv$pipeline$server(dataIn = reactive({rv$dataIn}))
  })
  
  
  output$showUI <- renderUI({
    req(rv$pipeline)
    rv$pipeline$ui()
  })
  
  observeEvent(input$send,{
    if (input$send%%2 != 0)
      rv$dataIn <- hl
    else
      rv$dataIn <- NULL
  })
}
