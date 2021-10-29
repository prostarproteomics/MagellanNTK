#' @title Shiny example process module.
#'
#' @description
#' This module contains the configuration informations for the corresponding pipeline.
#' It is called by the nav_pipeline module of the package Magellan
#' 
#' @seealso example_module_process1
#' 
#' @param id xxx
#' 
#' @rdname example_module_process3
#' 
#' @author Samuel Wieczorek
#'
mod_PipelineA_Process3_ui <- function(id){
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
#' @rdname example_module_process3
#' 
#' @importFrom stats setNames rnorm

mod_PipelineA_Process3_server <- function(id,
                                          nav.mode = 'process',
                                          dataIn = reactive({NULL}),
                                          steps.enabled = reactive({NULL}),
                                          remoteReset = reactive({FALSE})
){
  
  # This list contains the basic configuration of the process
  config <- list(
    # Name of the process
    name = 'Process3',
    # Name of the pipeline it belongs to
    parent = 'PipelineA',
    # List of all steps of the process
    steps = c('Description', 'Step1', 'Step2', 'Step3'),
    # A vector of boolean indicating if the steps are mandatory or not.
    mandatory = c(TRUE, FALSE, TRUE, TRUE)
  )
  
  # Define default selected values for widgets
  widgets.default.values <- list(
    Step1_select1 = 1,
    Step1_select2 = NULL,
    Step1_select3 = 1,
    Step2_select2_1 = 1,
    Step2_select2_2 = 1
  )
  
  
  ###-------------------------------------------------------------###
  ###                                                             ###
  ### ------------------- MODULE SERVER --------------------------###
  ###                                                             ###
  ###-------------------------------------------------------------###
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
    # Declaration of the variables that will contain the values of the widgets
    # To avoid confusion, the first string is the name of the step while the second is the name
    # of the widget
    rv.widgets <- reactiveValues(
      Step1_select1 = widgets.default.values$Step1_select1,
      Step1_select2 = widgets.default.values$Step1_select2,
      Step1_select3 = widgets.default.values$Step1_select3,
      Step2_select2_1 = widgets.default.values$Step2_select2_1,
      Step2_select2_2 = widgets.default.values$Step2_select2_2
    )
    
    
    # Reactive values during the run of the process
    rv <- reactiveValues(
      # Stores the object given in input of the process
      dataIn = NULL,
      # A vector of boolean indicating the status (UNDONE, SKIPPED or VALIDATED) of the steps
      steps.status = NULL,
      # xxx
      reset = NULL,
      # A vector of boolean indicating if the steps are enabled or disabled
      steps.enabled = NULL
    )
    
    
    # Returned value of the process
    # * The trigger variable is used to trigger an event that can be catched by the 
    #   Shiny functions observe() and observeEvent()
    # * The value variable contains the object to return to the instance that has called the process.
    dataOut <- reactiveValues(
      trigger = NULL,
      value = NULL
    )
    
    
    # Initialization of the module
    observeEvent(steps.enabled(), ignoreNULL = TRUE, {
      if (is.null(steps.enabled()))
        rv$steps.enabled <- setNames(rep(FALSE, rv$length), 
                                     rv$config$steps)
      else
        rv$steps.enabled <- steps.enabled()
    })
    
    # Set all the widgets to their default value after the remote Reset()
    observeEvent(remoteReset(), {
      lapply(names(rv.widgets), function(x){
        rv.widgets[[x]] <- widgets.default.values[[x]]
      })
    })
    
    ###### ------------------- Code for Description (step 0) -------------------------    #####
    ###### -----        This step does not have to be modified ------ ##
    
    
    
    
    output$Description <- renderUI({
      tagList(
        includeMarkdown(paste0("md/", paste0(config$parent, '_', config$name, ".md"))),
        uiOutput(ns('datasetDescription')),
        uiOutput(ns('validationBtn_ui'))
      )
    })
    
    
    output$validationBtn_ui <- renderUI({
      if (isTRUE(rv$steps.enabled['Description'])  )
        actionButton(ns('btn_validate_Description'),
                     paste0('Start ', config$name),
                     class = btn_success_color)
      else
        shinyjs::disabled(
          actionButton(ns('btn_validate_Description'),
                       paste0('Start ', config$name),
                       class = btn_success_color)
        )
    })
    
    observeEvent(input$btn_validate_Description, ignoreInit = TRUE, ignoreNULL = TRUE, {
      rv$dataIn <- dataIn()
      rv$steps.status['Description'] <- global$VALIDATED
      dataOut$trigger <- Magellan::Timestamp()
      dataOut$value <- rv$dataIn
    })
    
    
    
    ###### ------------------- Code for step 1 -------------------------    #####
    
    
    # ObserveEvent of the widgets
    observeEvent(input$select1, {rv.widgets$Step1_select1 <- input$select1})
    observeEvent(input$select2, {rv.widgets$Step1_select2 <- input$select2})
    observeEvent(input$select3, {rv.widgets$Step1_select3 <- input$select3})
    observeEvent(input$select2_1, {rv.widgets$Step2_select1 <- input$select2_1})
    observeEvent(input$select2_2, {rv.widgets$Step2_select2 <- input$select2_2})
    
    
    
    
    output$test1 <-renderUI({
      #rv$steps.enabled
      rv.widgets$select1
      if (rv$steps.enabled['Step1'])
        selectInput(ns('select1'), 'Select 1 in renderUI',
                    choices = 1:4,
                    selected = rv.widgets$Step1_select1,
                    width = '150px')
      else
        shinyjs::disabled(
          selectInput(ns('select1'), 'Select 1 in renderUI',
                      choices = 1:4,
                      selected = rv.widgets$Step1_select1,
                      width = '150px')
        )
    })
    
    
    
    output$test2 <-renderUI({
      
      rv$steps.enabled
      if (rv$steps.enabled['Step1'])
        selectInput(ns('select2'), 'Select 2 in renderUI',
                    choices = 1:3,
                    selected = rv.widgets$Step1_select2,
                    width = '150px')
      else
        shinyjs::disabled(
          selectInput(ns('select2'), 'Select 2 in renderUI',
                      choices = 1:4,
                      selected = rv.widgets$Step1_select2,
                      width = '150px')
        )
      
      
    })
    
    # Buttons must be explicitly enabled/disabled with a full code
    # Otherwise, they do not disable
    output$btn1_ui <- renderUI({
      if (rv$steps.enabled['Step1'])
        actionButton(ns('btn1'),
                     'btn1',
                     class = btn_success_color)
      else
        shinyjs::disabled(
          actionButton(ns('btn1'),
                       'btn1',
                       class = btn_success_color)
        )
    })
    
    # ------------------------ STEP 1 : UI ------------------------------------
    output$Step1 <- renderUI({
      name <- 'Step1'
      wellPanel(id = ns('toto'),
                uiOutput(ns('btn1_ui')),
                
                tagList(
                  div(id=ns('Step1a'),
                      div(style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                          uiOutput(ns('test1'))
                      ),
                      div(style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                          uiOutput(ns('test2'))
                      ),
                      div(style="display:inline-block; vertical-align: middle; padding-right: 40px;",
                          if (rv$steps.enabled['Step1'])
                            selectInput(ns('select3'), 'Select step 3',
                                        choices = 1:3,
                                        selected = rv.widgets$Step1_select3,
                                        width = '150px')
                          else
                            shinyjs::disabled(
                              selectInput(ns('select3'), 'Select step 3',
                                          choices = 1:5,
                                          selected = rv.widgets$Step1_select3,
                                          width = '150px')
                            )
                      ),
                      div(style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                          if (rv$steps.enabled['Step1'])
                            actionButton(ns(paste0('btn_validate_', name)),
                                         'Perform',
                                         class = btn_success_color)
                          else
                            shinyjs::disabled(
                              actionButton(ns(paste0('btn_validate_', name)),
                                           'Perform',
                                           class = btn_success_color)
                            )
                      )
                  )
                )
      )
      
      
      
    })
    
    
    observeEvent(input$btn_validate_Step1, ignoreInit = TRUE, {
      # Add your stuff code here
      # dataOut$trigger <- Send_Result_to_Caller(rv$dataIn)$trigger
      # dataOut$value <- Send_Result_to_Caller(rv$dataIn)$value
      dataOut$trigger <- Magellan::Timestamp()
      dataOut$value <- rv$dataIn
      rv$steps.status['Step1'] <- global$VALIDATED
    })
    
    #-------------------------- Code for step 2 ------------------------------
    
    
    
    output$select2_1_UI <-renderUI({
      rv$steps.enabled
      if (rv$steps.enabled['Step2'])
        selectInput(ns('select2_1'), 'Select 2_1 in renderUI',
                    choices = 1:3,
                    selected = rv.widgets$Step2_select1,
                    width = '150px')
      else
        shinyjs::disabled(
          selectInput(ns('select2_1'), 'Select 2_1 in renderUI',
                      choices = 1:3,
                      selected = rv.widgets$Step2_select1,
                      width = '150px')
        )
      
    })
    
    
    output$Step2_2_ui <- renderUI({
      if (rv$steps.enabled['Step2'])
        selectInput(ns('select2_2'), 'Select 2_2',
                    choices = 1:5,
                    selected = rv.widgets$Step2_select1,
                    width = '150px')
      else
        shinyjs::disabled(
          selectInput(ns('select2_2'),
                      'Select 2_2',
                      choices = 1:5,
                      selected = rv.widgets$Step2_select1,
                      width = '150px')
        )
    })
    
    
    output$Step2 <- renderUI({
      rv$steps.enabled
      name <- 'Step2'
      wellPanel(
        tagList(
          div(id=ns(name),
              div(style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                  uiOutput(ns('select2_1_UI'))
              ),
              div(style="display:inline-block; vertical-align: middle; padding-right: 40px;",
                  uiOutput(ns('Step2_2_ui'))
              ),
              div(style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                  if (rv$steps.enabled['Step2'])
                    actionButton(ns(paste0('btn_validate_', name)),
                                 'Perform',
                                 class = btn_success_color)
                  else
                    shinyjs::disabled(
                      actionButton(ns(paste0('btn_validate_', name)),
                                   'Perform',
                                   class = btn_success_color)
                    )
              )
          )
        )
      )
      
      
      
      
    })
    
    observeEvent(input$btn_validate_Step2, ignoreInit = TRUE, {
      # Add your stuff code here
      #dataOut$trigger <- Send_Result_to_Caller(rv$dataIn)$trigger
      #dataOut$value <- Send_Result_to_Caller(rv$dataIn)$value
      
      dataOut$trigger <- Magellan::Timestamp()
      dataOut$value <- rv$dataIn
      
      #rv$steps.status['Step2'] <- global$VALIDATED
    })
    
    
    #------------- Code for step 3 ---------------
    
    output$Step3 <- renderUI({
      rv$steps.enabled
      tagList(
        h3('Step 3'),
        if (rv$steps.enabled['Step3'])
          actionButton(ns('btn_validate_Step3'),
                       'Perform',
                       class = btn_success_color)
        else
          shinyjs::disabled(
            actionButton(ns('btn_validate_Step3'),
                         'Perform',
                         class = btn_success_color)
          )
      )
      
      
    })
    
    observeEvent(input$btn_validate_Step3, ignoreInit = TRUE, {
      # Add your stuff code here
      
      rv$dataIn <- Add_Datasets_to_Object(object = rv$dataIn, 
                                          dataset = rnorm(1:5),
                                          name = config$name)
      dataOut$trigger <- Magellan::Timestamp()
      dataOut$value <- rv$dataIn
      
      rv$steps.status['Step3'] <- global$VALIDATED
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
