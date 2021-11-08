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
#' @rdname example_mod_pipelineA
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
#' of the pipeline. This information is used to enable/disable the widgets. It is not
#' a communication variable between the caller and this module, thus there is no
#' corresponding output variable
#'
#'
#' @param remoteReset It is a remote command to reset the module. A boolean that
#' indicates is the pipeline has been reseted by a program of higher level
#' Basically, it is the program which has called this module
#'
#' @param steps.status xxx
#' 
#' @rdname example_mod_pipelineA
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
    # Name of the process
    name = 'PipelineA',
    # Name of the pipeline it belongs to
    # In this case, the module is the last one and do note have any parent.
    parent = NULL,
    # List of all steps of the process
    # Here, each step is a workflow
    steps = c('Description', 'Process1', 'Process2', 'Save'),
    # A vector of boolean indicating if the steps are mandatory or not.
    mandatory = c(TRUE, FALSE, FALSE, TRUE)
  )
  
  
  # Contrary to the simple workflow, there is no widget in this module
  # because all the widgets are provided by the simple workflows.
  
  ###-------------------------------------------------------------###
  ###                                                             ###
  ### ------------------- MODULE SERVER --------------------------###
  ###                                                             ###
  ###-------------------------------------------------------------###
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Insert necessary code which is hosted by Magellan
    # DO NOT MODIFY THIS LINE
    eval(parse(text = ComposedeWorflowCoreCode(steps = config$steps )))
    
    
    # Insert necessary code which is hosted by Magellan
    # DO NOT MODIFY THIS LINE
    eval(parse(text = Module_Return_Func()))
    }
  )
}
