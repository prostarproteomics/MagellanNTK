#' @title Shiny example process module.
#'
#' @description
#' This module contains the configuration informations for the corresponding pipeline.
#' It is called by the nav_pipeline module of the package MagellanNTK
#' 
#' The name of the server and ui functions are formatted with keywords separated by '_', as follows:
#' * first string `mod`: indicates that it is a Shiny module
#' * `pipeline name` is the name of the pipeline to which the process belongs
#' * `process name` is the name of the process itself
#' 
#' This convention is important because MagellanNTK call the different
#' server and ui functions by building dynamically their name.
#' 
#' In this example, `PipelineDemo_Process2_ui()` and `PipelineDemo_Process2_server()` define
#' the code for the process `Process1` which is part of the pipeline called `PipelineDemo`.
#'
#' @name example_module_process1
#' 
#' @param id xxx
#' @param dataIn The dataset
#' @param steps.enabled A vector of boolean which has the same length of the steps
#' of the pipeline. This information is used to enable/disable the widgets. It is not
#' a communication variable between the caller and this module, thus there is no
#' corresponding output variable
#' @param remoteReset It is a remote command to reset the module. A boolean that
#' indicates is the pipeline has been reseted by a program of higher level
#' Basically, it is the program which has called this module
#' @param steps.status xxx
#' @param current.pos xxx
#' @param path xxx
#' 
#' 
#' 
#' 
#' 
#' @author Samuel Wieczorek
#' 
#' 
NULL

#' @rdname example_module_process1
#' @export
#' 
PipelineDemo_Process2_conf <- function(){
  Config(
    fullname = 'PipelineDemo_Process2',
    mode = 'process',
    steps = c('Step 1', 'Step 2', 'Step 3'),
    mandatory = c(FALSE, TRUE, FALSE)
  )
}


#' @rdname example_module_process1
#' 
#' @export
#'
PipelineDemo_Process2_ui <- function(id){
  ns <- NS(id)
}



#' @rdname example_module_process1
#' 
#' @importFrom stats setNames rnorm
#' 
#' @export
#' 
PipelineDemo_Process2_server <- function(
    id,
  dataIn = reactive({NULL}),
  steps.enabled = reactive({NULL}),
  remoteReset = reactive({FALSE}),
  steps.status = reactive({NULL}),
  current.pos = reactive({1})
){
  
  #source(paste0(path, '/foo.R'), local=TRUE)$value
  
  # Define default selected values for widgets
  # This is only for simple workflows
  widgets.default.values <- list(
    Step1_select1 = 1,
    Step1_select2 = NULL,
    Step1_select3 = 1,
    Step1_radio1 = NULL,
    Step1_btn1 = NULL,
    Step2_select1 = 1,
    Step2_select2 = 1,
    Step3_select1 = 1
  )
  
  
  rv.custom.default.values <- list(
    foo = NULL
  )
  
  ###-------------------------------------------------------------###
  ###                                                             ###
  ### ------------------- MODULE SERVER --------------------------###
  ###                                                             ###
  ###-------------------------------------------------------------###
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Insert necessary code which is hosted by MagellanNTK
    # DO NOT MODIFY THIS LINE
    core.code <- Get_Workflow_Core_Code(
      mode = 'process',
      name = id,
      w.names = names(widgets.default.values),
      rv.custom.names = names(rv.custom.default.values)
    )
    
    eval(str2expression(core.code))
    
    
    # >>>
    # >>> START ------------- Code for Description UI---------------
    # >>> 
    
    
    output$Description <- renderUI({
      md.file <- paste0(id, '.md')
      path <- system.file('workflow/PipelineDemo/md', package='MagellanNTK')
      file <- file.path(path, md.file)
      
      tagList(
        ### In this example, the md file is found in the extdata/module_examples 
        ### directory but with a real app, it should be provided by the package 
        ### which contains the UI for the different steps of the process module.
        ### system.file(xxx)
        
        if (file.exists(file))
          includeMarkdown(file)
        else
          p('No Description available'),
        
        
        # Used to show some information about the dataset which is loaded
        # This function must be provided by the package of the process module
        uiOutput(ns('datasetDescription_ui')),
        
        # Insert validation button
        uiOutput(ns('Description_btn_validate_ui'))
      )
    })
    
    output$datasetDescription_ui <- renderUI({
      # Insert your own code to visualize some information
      # about your dataset. It will appear once the 'Start' button
      # has been clicked
      
    })
    
    output$Description_btn_validate_ui <- renderUI({
      widget <- actionButton(ns("Description_btn_validate"),
                             "Start",
                             class = btn_success_color)
      toggleWidget(widget, rv$steps.enabled['Description'])
    })
    
    
    observeEvent(input$Description_btn_validate, {
      rv$dataIn <- dataIn()
      dataOut$trigger <- Timestamp()
      dataOut$value <- rv$dataIn
      rv$steps.status['Description'] <- stepStatus$VALIDATED
    })
    
    # observe({
    #   req(dataIn())
    #   rv$dataIn <- dataIn()
    #   dataOut$trigger <- Timestamp()
    #   dataOut$value <- rv$dataIn
    #   rv$steps.status['Description'] <- stepStatus$VALIDATED
    # })
    
    # >>>
    # >>> START ------------- Code for step 1 UI---------------
    # >>> 
    
    # >>>> -------------------- STEP 1 : Global UI ----------------------
    output$Step1 <- renderUI({
      wellPanel(
        # uiOutput for all widgets in this UI
        # This part is mandatory
        # The renderUI() function of each widget is managed by MagellanNTK
        # The dev only have to define a reactive() function for each
        # widget he want to insert
        # Be aware of the naming convention for ids in uiOutput()
        # For more details, please refer to the dev document.
        fluidRow(
          column(width = 3, uiOutput(ns('Step1_btn1_ui'))),
          column(width = 3, uiOutput(ns('Step1_radio1_ui')))
        ),
        
        fluidRow(
          column(width = 3, uiOutput(ns('Step1_select1_ui'))),
          column(width = 3, uiOutput(ns('Step1_select2_ui'))),
          column(width = 3, uiOutput(ns('Step1_select3_ui')))
        ),
        #foo_ui(ns('foo')),
        # Insert validation button
        uiOutput(ns('Step1_btn_validate_ui')),
        
        # Additional code
        plotOutput(ns('showPlot'))
      )
    })
    
    
    # >>> START: Definition of the widgets
    
    
    
    
    # rv.custom$foo <- foo_server('foo',
    #   obj = reactive({rv$dataIn}),
    #   reset = reactive({NULL}),
    #   is.enabled = reactive({rv$steps.enabled['Step1']})
    # )
    
    
    
    output$Step1_btn1_ui <- renderUI({
      widget <- actionButton(ns('Step1_btn1'), 'Button',
                             class = btn_success_color)
      toggleWidget(widget, rv$steps.enabled['Step1'] )
    })
    
    # This part must be customized by the developer of a new module
    output$Step1_select1_ui <- renderUI({
      widget <- selectInput(ns('Step1_select1'), 'Select',
                            choices = 1:4,
                            selected = rv.widgets$Step1_select1,
                            width = '150px')
      toggleWidget(widget, rv$steps.enabled['Step1'] )
    })
    
    
    output$Step1_select2_ui <- renderUI({
      widget <- selectInput(ns('Step1_select2'), 'Select',
                            choices = 1:4,
                            selected = rv.widgets$Step1_select2,
                            width = '150px')
      toggleWidget(widget, rv$steps.enabled['Step1'])
    })
    
    
    output$Step1_select3_ui <- renderUI({
      widget <- selectInput(ns('Step1_select3'), 'Select',
                            choices = 1:4,
                            selected = rv.widgets$Step1_select3,
                            width = '150px')
      toggleWidget(widget, rv$steps.enabled['Step1'])
    })
    
    output$Step1_radio1_ui <- renderUI({
      widget <- radioButtons(ns('Step1_radio1'), 'Choose',
                             c("choice 1" = "choice1", "choice 2" = "choice2"),
                             selected = rv.widgets$Step1_radio1
      )
      toggleWidget(widget, rv$steps.enabled['Step1'])
    })
    
    output$Step1_btn_validate_ui <- renderUI({
      widget <-  actionButton(ns("Step1_btn_validate"),
                              "Perform",
                              class = btn_success_color)
      toggleWidget(widget, rv$steps.enabled['Step1'] )
      
    })
    # >>> END: Definition of the widgets
    
    
    observeEvent(input$Step1_btn_validate, {
      # Do some stuff
      # new.dataset <- 10*rv$dataIn[[length(rv$dataIn)]]
      # rv$dataIn <- addDatasets(object = rv$dataIn,
      #                          dataset = new.dataset,
      #                          name = paste0('temp_',id))
      
      # DO NOT MODIFY THE THREE FOLLOWINF LINES
      dataOut$trigger <- Timestamp()
      dataOut$value <- NULL
      rv$steps.status['Step1'] <- stepStatus$VALIDATED
      
    })
    
    
    # output$showPlot <- renderPlot({
    #   req(rv$dataIn)
    #   plot(as.matrix(rv$dataIn[[1]][,1]))
    # })
    # <<< END ------------- Code for step 1 UI---------------
    
    
    # >>> START ------------- Code for step 2 UI---------------
    
    output$Step2 <- renderUI({
      wellPanel(
        # Two examples of widgets in a renderUI() function
        fluidRow(
          column(width = 3, uiOutput(ns('Step2_select1_ui'))),
          column(width = 3, uiOutput(ns('Step2_select2_ui')))
        ),
        # Insert validation button
        # This line is necessary. DO NOT MODIFY
        uiOutput(ns('Step2_btn_validate_ui'))
      )
    })
    
    
    output$Step2_select1_ui <- renderUI({
      widget <- selectInput(ns('Step2_select1'), 'Select',
                            choices = 1:4,
                            selected = rv.widgets$Step2_select1,
                            width = '150px')
      toggleWidget(widget, rv$steps.enabled['Step2'] )
    })
    
    output$Step2_select2_ui <- renderUI({
      widget <- selectInput(ns('Step2_select2'), 'Select',
                            choices = 1:4,
                            selected = rv.widgets$Step2_select2,
                            width = '150px')
      toggleWidget(widget, rv$steps.enabled['Step2'] )
    })
    
    output$Step2_btn_validate_ui <- renderUI({
      widget <- actionButton(ns("Step2_btn_validate"),
                             "Perform",
                             class = btn_success_color)
      toggleWidget(widget, rv$steps.enabled['Step2'] )
    })
    
    observeEvent(input$Step2_btn_validate, {
      # Do some stuff
      # new.dataset <- 10*rv$dataIn[[length(rv$dataIn)]]
      # rv$dataIn <- addDatasets(object = rv$dataIn,
      #                          dataset = new.dataset,
      #                          name = paste0('temp_',id))
      
      # DO NOT MODIFY THE THREE FOLLOWINF LINES
      dataOut$trigger <- Timestamp()
      dataOut$value <- NULL
      rv$steps.status['Step2'] <- stepStatus$VALIDATED
    })
    
    # <<< END ------------- Code for step 2 UI---------------
    
    
    
    # >>> START ------------- Code for step 3 UI---------------
    
    output$Step3 <- renderUI({
      wellPanel(
        # Two examples of widgets in a renderUI() function
        fluidRow(
          column(width=4, uiOutput(ns('Step3_select1_ui'))),
          column(width=8, plotOutput(ns('Step3_plot_ui')))
          ),
        # Insert validation button
        # This line is necessary. DO NOT MODIFY
        uiOutput(ns('Step3_btn_validate_ui'))
      )
    })
    
    
    output$Step3_select1_ui <- renderUI({
      widget <- selectInput(ns('Step3_select1'), 'Select number of points',
                            choices = 1:20,
                            selected = rv.widgets$Step2_select1,
                            width = '150px')
      toggleWidget(widget, rv$steps.enabled['Step3'] )
    })
    
    output$Step3_plot_ui <- renderPlot({
      req(input$Step3_select1)
      plot(runif(1:input$Step3_select1))
    })

    
    output$Step3_btn_validate_ui <- renderUI({
      widget <- actionButton(ns("Step3_btn_validate"),
                             "Perform",
                             class = btn_success_color)
      toggleWidget(widget, rv$steps.enabled['Step3'] )
    })
    
    observeEvent(input$Step3_btn_validate, {
      # Do some stuff
      # new.dataset <- 10*rv$dataIn[[length(rv$dataIn)]]
      # rv$dataIn <- addDatasets(object = rv$dataIn,
      #                          dataset = new.dataset,
      #                          name = paste0('temp_',id))
      
      # DO NOT MODIFY THE THREE FOLLOWINF LINES
      dataOut$trigger <- Timestamp()
      dataOut$value <- NULL
      rv$steps.status['Step3'] <- stepStatus$VALIDATED
    })
    
    # <<< END ------------- Code for step 2 UI---------------
    
    
    
    # >>> START ------------- Code for step 'Save' UI---------------
    output$Save <- renderUI({
      tagList(
        # Insert validation button
        # This line is necessary. DO NOT MODIFY
        uiOutput(ns('Save_btn_validate_ui')),
        uiOutput(ns('dl_ui'))
      )
    })
    
    output$dl_ui <- renderUI({
      req(config@mode == 'process')
      req(rv$steps.status['Save'] == stepStatus$VALIDATED)
      download_dataset_ui(ns('createQuickLink'))
    })
    
    output$Save_btn_validate_ui <- renderUI({
      toggleWidget(
        actionButton(ns("Save_btn_validate"), "Save",
                     class = btn_success_color),
        rv$steps.enabled['Save']
      )
    })
    observeEvent(input$Save_btn_validate, {
      # Do some stuff
      rv$dataIn <- MagellanNTK::addDatasets(object = rv$dataIn,
                               dataset = 10*rv$dataIn[[length(rv$dataIn)]],
                               name = 'Process2')
      
      # DO NOT MODIFY THE THREE FOLLOWINF LINES
      dataOut$trigger <- Timestamp()
      dataOut$value <- rv$dataIn
      rv$steps.status['Save'] <- stepStatus$VALIDATED
      download_dataset_server('createQuickLink', 
        dataIn = reactive({rv$dataIn}))
      
    })
    # <<< END ------------- Code for step 3 UI---------------
    
    
    
    # Insert necessary code which is hosted by MagellanNTK
    # DO NOT MODIFY THIS LINE
    eval(parse(text = Module_Return_Func()))
  }
  )
}
