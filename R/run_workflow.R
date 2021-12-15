

#' @title Shiny example module `Process`
#'
#' @description
#' This module contains the configuration informations for the corresponding pipeline.
#' It is called by the nav_pipeline module of the package Magellan.
#' This documentation is for developpers who want to create their own pipelines nor processes
#' to be managed with `Magellan`.
#' 
#' @param id xxx
#' @param verbose A `boolean` that indicates whether to show some infos in the console
#' and add the shiny module for debugging
#' 
#' @param tl.layout Additional parameters for mod_nav 
#'
#' @rdname example_mod_pipeline
#'
#' @author Samuel Wieczorek
#' 
#' @importFrom utils data
#' @import shiny
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' dirpath <- system.file('module_examples', package='Magellan')
#' for (l in list.files(path = dirpath, pattern = ".R", recursive = TRUE))
#'  source(file.path(dirpath, l), local=FALSE)$value
#'
#' run_workflow('Process1')
#' 
#' run_workflow('PipelineA', layout = c('h', 'h'))
#' }
#' 
run_workflow <- function(id,
                         verbose = FALSE,
                         tl.layout = NULL
                         ){
  
  if(missing(id)){
    warning("'id' is required.")
    return(NULL)
  }
  if (!Found_Mod_Funcs(id)){
    return(NULL)
  }
  
  
  ui <- fluidPage(
    tagList(
      uiOutput('UI'),
      uiOutput('debugInfos_ui')
    )
  )
  
  
  #----------------------------------------------------------------------
  server <- function(input, output){
    
    
    
    rv <- reactiveValues(
      dataOut = NULL
    )
    
    output$UI <- renderUI({mod_nav_ui(id)})
    
    dataIn <- mod_Load_Dataset_server('exemple')
    
    output$debugInfos_ui <- renderUI({
      req(verbose)
      mod_Debug_Infos_ui('debug_infos')
    })
    
    
    
    observeEvent(req(dataIn()), {
      rv$dataOut <- mod_nav_server(id = id,
                                   verbose = verbose,
                                   dataIn = reactive({dataIn()}),
                                   tl.layout = tl.layout
                                   )
      
      mod_Debug_Infos_server(id = 'debug_infos',
                             title = 'Infos from shiny app',
                             rv.dataIn = reactive({dataIn()}),
                             dataOut = reactive({rv$dataOut$dataOut()})
      )
      })
    
    observe( {
      req(rv$dataOut)
      rv$dataOut$dataOut()$value
      mod_Save_Dataset_server(id = 'exemple', 
                            dataIn = reactive({rv$dataOut$dataOut()$value}))
    })

  }
  
  
  shinyApp(ui, server)
  
}
