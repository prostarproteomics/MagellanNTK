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
#' @rdname example_module_process_step0
#'
#' @author Samuel Wieczorek
#' 
mod_PipelineA_Description_ui <- function(id){
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
#' @param remoteReset It is a remote command to reset the module. A boolean that
#' indicates is the pipeline has been reseted by a program of higher level
#' Basically, it is the program which has called this module
#' 
#' @rdname example_module_process_step0
#' 
#' @importFrom stats setNames
#' 
mod_PipelineA_Description_server <- function(id,
                                             dataIn = reactive({NULL}),
                                             steps.enabled = reactive({NULL}),
                                             remoteReset = reactive({FALSE}),
                                             current.pos = reactive({1})
                                             ){

  config <- list(
    # Name of the pipeline it belongs to
    parent = 'PipelineA',
    # Name of the process
    name = 'Description',
    # List of all steps of the process
    steps = c('Description'),
    # A vector of boolean indicating if the steps are mandatory or not.
    mandatory = c(TRUE)
  )
  
  # Define default selected values for widgets
  # By default, this list is empty for the Description module
  # but it can be customized
  widgets.default.values <- list()
  
  ###-------------------------------------------------------------###
  ###                                                             ###
  ### ------------------- MODULE SERVER --------------------------###
  ###                                                             ###
  ###-------------------------------------------------------------###
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Insert necessary code which is hosted by Magellan
    # DO NOT MODITY THIS LINE
    eval(parse(text = ModuleCoreCode(widgets = names(widgets.default.values),
                                     steps = config$steps )))
    
     
    ###### ------------------- Code for Description (step 0) -------------------------    #####
    output$Description <- renderUI({
      tagList(
        includeMarkdown(system.file("scripts/module_examples/md/", 
                                    paste0(config$parent, '_', config$name, ".md"), 
                                    package="Magellan")),
        
        uiOutput(ns('datasetDescription')),
        
        # Insert validation button
        uiOutput(ns('Description_validationBtn_ui'))
      )
    })

    # Insert necessary code which is hosted by Magellan
    # DO NOT MODITY THIS LINE
    eval(parse(text = Module_Return_Func()))

  }
  )
}
