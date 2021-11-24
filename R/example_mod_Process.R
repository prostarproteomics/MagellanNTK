

#' @title Shiny example module `Process`
#'
#' @description
#' This module contains the configuration informations for the corresponding pipeline.
#' It is called by the nav_pipeline module of the package Magellan.
#' This documentation is for developpers who want to create their own pipelines nor processes
#' to be managed with `Magellan`.
#' 
#' @param verbose xxx
#'
#' @rdname example_mod_pipeline
#'
#' @author Samuel Wieczorek
#' 
#' @importFrom utils data
#' 
#' @export
#' 
example_mod_Process <- function(verbose = FALSE){
  
  ui <- fluidPage(
  tagList(
    uiOutput('UI'),
    uiOutput('debugInfos_ui')
  )
)


#----------------------------------------------------------------------
server <- function(input, output){
  
  dirpath <- system.file('module_examples', package='Magellan')
  for (l in list.files(path = dirpath, pattern = ".R", recursive = TRUE))
    source(file.path(dirpath, l), local=FALSE)$value
  
  rv <- reactiveValues(
    dataIn = Magellan::feat1,
    dataOut = NULL
  )
  
  
  output$debugInfos_ui <- renderUI({
    req(verbose)
    # Just for example purpose
    mod_Debug_Infos_ui('debug_infos')
  })
  
  mod_Debug_Infos_server(id = 'debug_infos',
                         title = 'Infos from shiny app',
                         rv.dataIn = reactive({rv$dataIn}),
                         dataOut = reactive({rv$dataOut$dataOut()})
  )
  
  observe({
    rv$dataOut <- mod_nav_server(id = 'Process1',
                                 dataIn = reactive({rv$dataIn}),
                                 timelines = c('h'),
                                 verbose = verbose
                                 )
  }, priority=1000)
  
  
  output$UI <- renderUI({mod_nav_ui('Process1')})
}


shinyApp(ui, server)

}
