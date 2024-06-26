#' @title Shiny example module `Pipeline A`
#'
#' @description
#' This module contains the configuration informations for the corresponding pipeline.
#' It is called by the nav_pipeline module of the package MagellanNTK
#' This documentation is for developpers who want to create their own pipelines nor processes
#' to be managed with `MagellanNTK`.
#' 
#' @param verbose A `boolean` that indicates whether to show some infos in the console
#' and add the shiny module for debugging
#'
#' @rdname example_mod_pipeline
#'
#' @author Samuel Wieczorek
#' 
#' @importFrom utils data
#' 
#' @export
#' 
example_mod_Pipeline <- function(verbose = FALSE){
#----------------------------------------------------------------------
ui <- fluidPage(
  tagList(
    uiOutput('UI'),
    uiOutput('debugInfos_ui')
  )
)

#----------------------------------------------------------------------
server <- function(input, output){

  dirpath <- system.file('extdata/module_examples', package='MagellanNTK')
  for (l in list.files(path = dirpath, pattern = ".R", recursive = TRUE))
    source(file.path(dirpath, l), local=FALSE)$value
  
  rv <- reactiveValues(
    dataIn = MagellanNTK::data1,
    dataOut = NULL
  )
  
  observe({
     rv$dataOut <- mod_nav_server(id = 'PipelineDemo',
                                  dataIn = reactive({rv$dataIn}),
                                  layout = c('v', 'h'),
                                  verbose = verbose
                                  )
    
    output$UI <- renderUI({mod_nav_ui('PipelineDemo')})
  })
  
  output$debugInfos_ui <- renderUI({
    req(verbose)
    # Just for example purpose
    mod_Debug_Infos_ui('debug_infos_app')
  })
  
  
  mod_Debug_Infos_server(id = 'debug_infos_app',
                         title = 'Infos from shiny app',
                         rv.dataIn = reactive({rv$dataIn}),
                         dataOut = reactive({rv$dataOut$dataOut()}))
}



shinyApp(ui, server)
}
