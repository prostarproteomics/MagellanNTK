#' @title Shiny example process module.
#'
#' @description
#' This module contains the configuration informations for the corresponding pipeline.
#' It is called by the nav_pipeline module of the package Magellan
#' 
#' The name of the server and ui functions are formatted with keywords separated by '_', as follows:
#' * first string `mod`: indicates that it is a Shiny module
#' * `pipeline name` is the name of the pipeline to which the process belongs
#' * `process name` is the name of the process itself
#' 
#' This convention is important because Magellan call the different
#' server and ui functions by building dynamically their name.
#' 
#' In this example, `mod_PipelineA_ProcessA_ui()` and `mod_PipelineA_ProcessA_server()` define
#' the code for the process `ProcessA` which is part of the pipeline called `PipelineA`.
#' 
#' @param id xxx
#' 
#' @rdname example_module_process1
#' 
#' @author Samuel Wieczorek
#'
mod_PipelineA_Process1_ui <- function(id){
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
#' @rdname example_module_process1
#' 
#' @importFrom stats setNames rnorm

mod_PipelineA_Process1_server <- function(id,
                                          dataIn = reactive({NULL}),
                                          steps.enabled = reactive({NULL}),
                                          remoteReset = reactive({FALSE}),
                                          current.pos = reactive({1})
){
  
  # This list contains the basic configuration of the process
  config <- list(
    # Name of the process
    name = 'Process1',
    # Name of the pipeline it belongs to
    parent = 'PipelineA',
    # List of all steps of the process
    steps = c('Description', 'Step1', 'Step2', 'Step3', 'Step4', 'Save'),
    # A vector of boolean indicating if the steps are mandatory or not.
    mandatory = c(TRUE, FALSE, TRUE, TRUE, FALSE, TRUE)
  )
  
  # Define default selected values for widgets
  widgets.default.values <- list(
    Step1_select1 = 1,
    Step1_select2 = NULL,
    Step1_select3 = 1,
    Step1_btn1 = NULL,
    Step2_select1 = 1,
    Step2_select2 = 1
  )
  
  
  ###-------------------------------------------------------------###
  ###                                                             ###
  ### ------------------- MODULE SERVER --------------------------###
  ###                                                             ###
  ###-------------------------------------------------------------###
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    eval(parse(text = ModuleCoreCode(widgets = names(widgets.default.values),
                                     steps = config$steps
                                     )
               )
         )


    ###### ------------------- Code for Description (step 0) -------------------------    #####
    output$Description <- renderUI({
      tagList(
        includeMarkdown(paste0("md/", paste0(config$parent, '_', config$name, ".md"))),
        uiOutput(ns('datasetDescription')),
        
        # Insert validation button
        uiOutput(ns('Description_validationBtn_ui'))
      )
    })
    

    
    
    
    ###### ------------------- Code for step 1 -------------------------    #####
    
    # >>>> -------------------- STEP 1 : Global UI ------------------------------------
    output$Step1 <- renderUI({
      wellPanel(
        # uiOutput for all widgets in this UI
        # This part is mandatory
        uiOutput(ns('Step1_btn1_ui')),
        uiOutput(ns('Step1_select1_ui')),
        uiOutput(ns('Step1_select2_ui')),
        uiOutput(ns('Step1_select3_ui')),
        # Insert validation button
        uiOutput(ns('Step1_validationBtn_ui')),
        
        # Additional code
        plotOutput(ns('showPlot'))
      )
    })
    
    
    
    # >>> START: Definition of the widgets
    # This part must be customized by the developer of a new module
    widget_Step1_select1 <- reactive({
      selectInput(ns('Step1_select1'),
                  'Select 1 in renderUI',
                  choices = 1:4,
                  selected = rv.widgets$Step1_select1,
                  width = '150px')
    })
    
    
    widget_Step1_select2 <- reactive({
      selectInput(ns('Step1_select2'), 
                  'Select 2 in renderUI',
                  choices = 1:4,
                  selected = rv.widgets$Step1_select2,
                  width = '150px')
    })
    
    widget_Step1_select3 <- reactive({
      selectInput(ns('Step1_select3'), 
                  'Select 3 in renderUI',
                  choices = 1:4,
                  selected = rv.widgets$Step1_select3,
                  width = '150px')
    })
    
    
    widget_Step1_btn1 <- reactive({
      actionButton(ns('Step1_btn1'),
                   'Step1_btn1',
                   class = btn_success_color)
    })
    
    # >>> END: Definition of the widgets
    
    
    
    

    output$showPlot <- renderPlot({
      plot(as.matrix(dataIn()[[1]]))
    })
    
    
    
    #-------------------------- Code for step 2 ------------------------------
    
    output$Step2 <- renderUI({
      wellPanel(
        uiOutput(ns('Step2_select1_ui')),
        uiOutput(ns('Step2_select2_ui')),
        
        # Insert validation button
        uiOutput(ns('Step2_validationBtn_ui'))
      )
    })
    
    
    
    widget_Step2_select1 <- reactive({
      selectInput(ns('Step2_select1'),
                  'Step2_select1 in renderUI',
                  choices = 1:4,
                  selected = rv.widgets$Step2_select1,
                  width = '150px')
    })
    
    widget_Step2_select2 <- reactive({
      selectInput(ns('Step2_select2'),
                  'Step2_select2 in renderUI',
                  choices = 1:4,
                  selected = rv.widgets$Step2_select2,
                  width = '150px')
    })
    

    
    #------------- Code for step 3 ---------------
    
    output$Step3 <- renderUI({
       tagList(
        # Insert validation button
        uiOutput(ns('Step3_validationBtn_ui'))
      )
    })
    

    
    
    
    # Return value of module
    # DO NOT MODIFY THIS PART
    list(config = reactive({
      config$ll.UI <- setNames(lapply(config$steps,
                                      function(x){
                                        do.call('uiOutput', list(ns(x)))
                                      }),
                               paste0('screen_', config$steps)
      )
      config
    }),
    dataOut = reactive({dataOut})
    #steps.status = reactive({rv$steps.status})
    )
    
  }
  )
}
