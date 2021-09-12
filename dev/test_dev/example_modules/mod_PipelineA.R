
mod_PipelineA_ui <- function(id){
  ns <- NS(id)
}


#' @title xxx
#'
#' @description
#' This module contains the configuration informations for the corresponding pipeline.
#' It is called by the nav_pipeline module of the package Magellan
#'
#' @param id xxx
#'
#' @param dataIn The dataset
#'
#' @param steps.enabled A vector of boolen which has the same length of the steps
#' of the pipeline. xxx
#'
#' @param remoteReset It is a remote command to reset the module. A boolen that
#' indicates is the pipeline has been reseted by a program of higher level
#' Basically, it is the program which has called this module
#'
#' @param status xxx
#'
#' @author Samuel Wieczorek
#'
#' @export
#'
#' @import shiny
#'
mod_PipelineA_server <- function(id,
                                 dataIn = reactive({NULL}),
                                 steps.enabled = reactive({NULL}),
                                 remoteReset = reactive({FALSE}),
                                 status = reactive({NULL})){

  config <- list(
    name = 'PipelineA',
    steps = c('Description', 'ProcessA'),
    mandatory = c(TRUE, FALSE)
    
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
       status = NULL,
      reset = NULL,
      steps.enabled = NULL
    )
    
    # Returned value of the process
    dataOut <- reactiveValues(
      trigger = NULL,
      value = NULL
    )
    
    observeEvent(status(), { rv$status <- status()})
    
    # Initialization of the module
    observeEvent(steps.enabled(), ignoreNULL = TRUE, {
      if (is.null(steps.enabled()))
        rv$steps.enabled <- setNames(rep(FALSE, rv.process$length), rv.process$config$steps)
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
