#' @title Shiny example module `Pipeline A`
#'
#' @description
#' This module contains the configuration informations for the corresponding pipeline.
#' It is called by the nav_pipeline module of the package Magellan.
#' This documentation is for developpers who want to create their own pipelines nor processes
#' to be managed with `Magellan`.
#' 
#' @param id xxx
#'
#' @rdname example_module_pipeline
#'
#' @author Samuel Wieczorek
#' 
mod_PipelineA_ui <- function(id){
  ns <- NS(id)
}



#' @param id xxx
#'
#' @param dataIn The dataset
#'
#' @param steps.enabled A vector of boolean which has the same length of the steps
#' of the pipeline. xxx
#'
#' @param remoteReset It is a remote command to reset the module. A boolean that
#' indicates is the pipeline has been reseted by a program of higher level
#' Basically, it is the program which has called this module
#'
#' @param steps.status xxx
#' 
#' @rdname example_module_pipeline
#'
#' @import shiny
#' @importFrom stats setNames
#'
mod_PipelineA_server <- function(id,
                                 dataIn = reactive({NULL}),
                                 steps.enabled = reactive({NULL}),
                                 remoteReset = reactive({FALSE}),
                                 steps.status = reactive({NULL})){

  config <- list(
    name = 'PipelineA',
    parent = NULL,
    steps = c('Description', 'Process1', 'Process2', 'Process3'),
    mandatory = c(TRUE, FALSE, FALSE, TRUE)
  )
  
  ###-------------------------------------------------------------###
  ###                                                             ###
  ### ------------------- MODULE SERVER --------------------------###
  ###                                                             ###
  ###-------------------------------------------------------------###
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    rv <- reactiveValues(
      dataIn = NULL,
      steps.status = NULL,
      steps.reset = NULL,
      steps.enabled = NULL
    )
    
    # Returned value of the process
    dataOut <- reactiveValues(
      trigger = NULL,
      value = NULL
    )
    
    observeEvent(steps.status(), { rv$steps.status <- steps.status()})
    
    # Initialization of the module
    observeEvent(steps.enabled(), ignoreNULL = TRUE, {
      if (is.null(steps.enabled()))
        rv$steps.enabled <- setNames(rep(FALSE, rv$length), rv$config$steps)
      else
        rv$steps.enabled <- steps.enabled()
    })
    
    
    # Return value of module
    # DO NOT MODIFY THIS PART
    list(config = reactive({config$ll.UI <- setNames(lapply(config$steps,
                                                            function(x){
                                                              do.call('uiOutput', list(ns(x)))
                                                            }),
                                                     paste0('screen_', config$steps)
    )
    config}),
         dataOut = reactive({dataOut})
         #status = reactive({rv$status})
    )
    
  }
  )
}
