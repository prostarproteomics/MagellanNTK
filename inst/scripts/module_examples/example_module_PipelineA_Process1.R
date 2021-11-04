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
    
    
    observe({
      # Check the syntax and functions in the module before running it
      
    })
    
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
    
    # ObserveEvent of the widgets
    observeEvent(input$select1, {rv.widgets$Step1_select1 <- input$select1})
    observeEvent(input$select2, {rv.widgets$Step1_select2 <- input$select2})
    observeEvent(input$select3, {rv.widgets$Step1_select3 <- input$select3})
    observeEvent(input$select2_1, {rv.widgets$Step2_select1 <- input$select2_1})
    observeEvent(input$select2_2, {rv.widgets$Step2_select2 <- input$select2_2})
    
    
    
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
    
    
    
    #-----------------------------------------------------

   # Observer for the validation buttons of all steps
    observeEvent(lapply(config$steps, function(x) input[[paste0('btn_validate_', x)]]),
                 ignoreInit = TRUE,
                 ignoreNULL = TRUE,
                 {
                   test <- lapply(config$steps, function(x) input[[paste0('btn_validate_', x)]])
                   if( sum(unlist(test)) != 1)
                     return()
                   
                   if (current.pos() == length(config$steps)){
                     rv$dataIn <- Add_Datasets_to_Object(object = rv$dataIn,
                                                         dataset = rnorm(1:5),
                                                         name = config$name)
                     }
                   
                   if (current.pos() == 1 ){
                     rv$dataIn <- dataIn()
                   }
                   
                   dataOut$trigger <- Magellan::Timestamp()
                   dataOut$value <- rv$dataIn
                   rv$steps.status[current.pos()] <- global$VALIDATED
                   })
    

## ----------------------------------
    # Buttons must be explicitly enabled/disabled with a full code
    # Otherwise, they do not disable
    
    toto <- "output$validationBtn_Description_ui <- renderUI({
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
    })"
    
    generateValidationBtnCode <- function(name){
      
      label <- 'Perform'
      if (name == 'Description')
        label <- 'Start'
      
      code <- "output$validationBtn_step.name_ui <- renderUI({
      if (isTRUE(rv$steps.enabled['step.name'])  )
        actionButton(ns('btn_validate_step.name'),
                     paste0('Start ', config$name),
                     class = btn_success_color)
      else
        shinyjs::disabled(
          actionButton(ns('btn_validate_step.name'),
                       paste0('Start ', config$name),
                       class = btn_success_color)
        )
    })"
      
      
      code <- gsub("step.name", name, code)
      code
    }
    
    
    lapply(config$steps, function(x) eval(parse(text = generateValidationBtnCode(x))))
    
    
    ##-------------------------------------------------------------------
    
    
    
    
    
    ###### ------------------- Code for Description (step 0) -------------------------    #####
    output$Description <- renderUI({
      tagList(
        includeMarkdown(paste0("md/", paste0(config$parent, '_', config$name, ".md"))),
        uiOutput(ns('datasetDescription')),
        
        # Insert validation button
        uiOutput(ns('validationBtn_Description_ui'))
      )
    })
    
    
    
    
    
    ###### ------------------- Code for step 1 -------------------------    #####
    
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
                      # Insert validation button
                      div(style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                          uiOutput(ns('validationBtn_Step1_ui'))
                      )
                  )
                ),
                plotOutput(ns('showPlot'))
      )
      
      
      
    })
    
    
    output$showPlot <- renderPlot({
      plot(as.matrix(dataIn()[[1]]))
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
              # Insert validation button
              div(style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                  uiOutput(ns('validationBtn_Step2_ui'))
              )
          )
        )
      )
      
      
      
      
    })
    
    
    
    
    #------------- Code for step 3 ---------------
    
    output$Step3 <- renderUI({
      rv$steps.enabled
      tagList(
        h3('Step 3'),
        # Insert validation button
        uiOutput(ns('validationBtn_Step3_ui'))
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