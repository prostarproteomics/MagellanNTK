#' @title The ui() function of the module `mod_nav`
#' 
#' @param id A `character(1)` which defines the id of the module. It is the same
#' as for the server() function.
#' 
#' @rdname mod_nav
#' 
#' @author Samuel Wieczorek
#' 
#' @export
#'
mod_nav_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    uiOutput(ns('nav_mod_ui')),
    mod_Debug_Infos_ui(ns('debug_infos'))
  )
}








#' @title The server() function of the module `mod_nav`
#' 
#' @description The module navigation can be launched via a Shiny app.
#' 
#' @param id A `character(1)` which defines the id of the module. It is the same
#' as for the ui() function.
#' 
#' @param nav.mode A `character(1)` indicating the type of workflow. It can be
#' either 'process' (for a simple workflow) or 'pipeline' (for a composed 
#' workflow). Default is NULL: a value is necessary.
#' 
#' @param dataIn The dataset
#' 
#' @param is.enabled A `boolean`. This variable is a remote command to specify
#' if the corresponding module is enabled/disabled in the calling module of upper level.
#' For example, if this module is part of a pipeline and the pipeline calculates
#' that it is disabled (i.e. skipped), then this variable is set to TRUE. Then,
#' all the widgets will be disabled. If not, the enabling/disabling of widgets
#' is deciding by this module. 
#' 
#' @param remoteReset It is a remote command to reset the module. A boolen that 
#' indicates is the pipeline has been reseted by a program of higher level
#' Basically, it is the program which has called this module
#' 
#' @param is.skipped xxx
#' 
#' @return A list of four items:
#' * dataOut xxx
#' * steps.enabled xxxxx
#' * status A vector of `integer(1)` of the same length than the config$steps
#'   vector
#' * reset xxxx
#' 
#' @export
#' 
#' @rdname mod_nav
#' 
#' @examples
#' \dontrun{
#' library(shiny)
#' library(shinyBS)
#' ui <- fluidPage(
#'   mod_nav_ui('Protein_Description')
#' )
#' server <- function(input, output){
#'   mod_nav_server(id = 'Protein_Description',
#'   nav.mode = 'process',
#'   dataIn = reactive({feat1})
#'   )
#' }
#' shinyApp(ui, server)
#' }
#' 
#' @importFrom stats setNames
#' 
mod_nav_server <- function(id,
                           nav.mode = NULL,
                           dataIn = reactive({NULL}),
                           is.enabled = reactive({TRUE}),
                           remoteReset = reactive({FALSE}),
                           is.skipped = reactive({FALSE})
                           ){
  
  verbose <- FALSE
  
  ###-------------------------------------------------------------###
  ###                                                             ###
  ### ------------------- MODULE SERVER --------------------------###
  ###                                                             ###
  ###-------------------------------------------------------------###
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    observe({
      if (is.null(nav.mode) || !(nav.mode %in% c('process', 'pipeline'))){
        warning("'nav.mode' must be either 'process' or 'pipeline'.")
        return(NULL)
      }
    }, priority = 10000)
    

    source(system.file('extdata/general_nav_funcs.R', package='Magellan'), local = TRUE)$value
    
    
    switch (nav.mode,
            pipeline = {
              source(system.file('extdata/pipeline_funcs.R', package='Magellan'), local = TRUE)$value
              
              },
            process = {
              source(system.file('extdata/process_funcs.R', package='Magellan'), local = TRUE)$value
              
              }
        )

    
    
    # Launch the renderUI function for the user interface of the module
    # Apparently, the renderUI() cannot be stored in the expression
    output$nav_mod_ui <- renderUI({
      tagList(
        uiOutput(ns('pipeline_ui')),
        uiOutput(ns('process_ui'))
      )
    })

    mod_Debug_Infos_server(id = 'debug_infos',
                           title = paste0('Infos from pipeline : ', id),
                           config = reactive({rv$config}),
                           rv.dataIn = reactive({rv$dataIn}),
                           dataIn = reactive({dataIn()}),
                           dataOut = reactive({dataOut}),
                           steps.status = reactive({rv$steps.status}),
                           current.pos = reactive({ rv$current.pos}),
                           steps.enabled = reactive({rv$steps.enabled}),
                           is.enabled = reactive({is.enabled()}))
    
    # The return value of the nav_process module server
    # The item 'dataOut' has been updated by the module process and it is returned to the
    # function that has called this nav_process module (it can be a module, a Shiny app or another nav module
    # for example, nav_pipeline)
    list(dataOut = reactive({dataOut}),
         steps.enabled = reactive({rv$steps.enabled}),
         status = reactive({rv$steps.status})
    )
    
    
  })
  
}