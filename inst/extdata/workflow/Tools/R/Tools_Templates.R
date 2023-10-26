#' @title Shiny example process module.
#'
#' @description
#' This module contains the configuration information for the corresponding pipeline.
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
#' In this example, `PipelineA_ProcessA_ui()` and `PipelineA_ProcessA_server()` define
#' the code for the process `ProcessA` which is part of the pipeline called `PipelineA`.
#'
#' @name Tools_Templates_conf
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

#' @rdname Tools_Templates_conf
#' @export
#' 
Tools_Templates_conf <- function(){
  Config(
    fullname = 'Tools_Templates',
    mode = 'process',
    steps = c('Directory', 'Add steps'),
    mandatory = c(TRUE, TRUE)
  )
}


#' @rdname Tools_Templates_conf
#' 
#' @export
#'
Tools_Templates_ui <- function(id){
  ns <- NS(id)
}



#' @rdname Tools_Templates_conf
#' 
#' @importFrom stats setNames rnorm
#' 
#' @export
#' 
Tools_Templates_server <- function(id,
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
    Directory_mode = '',
    Directory_parent = NULL,
    Directory_name = NULL,
    Directory_mdEditor = '',
    Addsteps_selectStep = NULL
  )
  
  
  rv.custom.default.values <- list(
    path =  reactive({'~'}),
    files = NULL,
    md_raw = ''
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
      path <- system.file('extdata/workflow/Tools/md', package='MagellanNTK')
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
                             class = GlobalSettings$btn_success_color)
      toggleWidget(widget, rv$steps.enabled['Description'])
    })
    
    
    observeEvent(input$Description_btn_validate, {
      rv$dataIn <- dataIn()
      dataOut$trigger <- Timestamp()
      dataOut$value <- rv$dataIn
      rv$steps.status['Description'] <- global$VALIDATED
    })
    
    
    
    # >>>
    # >>> START ------------- Code for step 1 UI---------------
    # >>> 
    
    # >>>> -------------------- STEP 1 : Global UI ------------------------------------
    output$Directory <- renderUI({
      wellPanel(
        # uiOutput for all widgets in this UI
        # This part is mandatory
        # The renderUI() function of each widget is managed by MagellanNTK
        # The dev only have to define a reactive() function for each
        # widget he want to insert
        # Be aware of the naming convention for ids in uiOutput()
        # For more details, please refer to the dev document.
        uiOutput(ns('Directory_chooseDir_ui')),
        #uiOutput(ns('Directory_guess_ui')),
        uiOutput(ns('Directory_warnDir_ui')),
        
        
        fluidRow(
          column(width = 3, uiOutput(ns('Directory_mode_ui'))),
          column(width = 3, uiOutput(ns('Directory_parent_ui'))),
          column(width = 3, uiOutput(ns('Directory_name_ui')))
        ),
        
        uiOutput(ns('Directory_mdEditor_ui')),
        
        
        # Insert validation button
        uiOutput(ns('Directory_btn_validate_ui'))
      )
    })
    
    
    # >>> START: Definition of the widgets
    
    
    
    
    # rv.custom$foo <- foo_server('foo',
    #   obj = reactive({rv$dataIn}),
    #   reset = reactive({NULL}),
    #   is.enabled = reactive({rv$steps.enabled['Step1']})
    # )
    
  rv.custom$path <- chooseDir_server('Directory_chooseDir',
                                       path = reactive({'~'}),
                                       is.enabled = reactive({rv$steps.enabled['Directory']}))
    
    
    output$Directory_chooseDir_ui <- renderUI({
        widget <- div(id = ns('div_Directory_chooseDir'),
                    chooseDir_ui(ns('Directory_chooseDir')))
      toggleWidget(widget, rv$steps.enabled['Directory'] )
    })
    
    
    output$Directory_warnDir_ui <- renderUI({
      req(rv.custom$path())
      .path <- gsub('\\', '/', rv.custom$path(), fixed = TRUE)
      pattern <- c('R', 'md')
      dirs <- list.dirs(.path, recursive = FALSE, full.names=FALSE)
      if (length(dirs) == 0)
        msg <- pattern
      else
        msg <- pattern[-which(intersect(dirs, pattern) == pattern)]
      
      lapply(msg, function(i) 
        p(paste0("The '", i, "' directory does not exists. It will be created")))
    })  

    # This part must be customized by the developer of a new module
    output$Directory_guess_ui <- renderUI({
      req(rv.custom$path())
      lst.files <- list.files(file.path(rv.custom$path(), 'R'))
      lapply(lst.files, function(i)
        p(i))
    })
    
    
    
    output$Directory_mode_ui <- renderUI({
      widget <- selectInput(ns('Directory_mode'), 'Template to create', 
                            choices = c('pipeline', 'process', 'module'), 
                            selected = isolate(rv.widgets$Directory_mode),
                            width='100px')
      toggleWidget(widget, rv$steps.enabled['Directory'] )
    })
    
    output$Directory_parent_ui <- renderUI({
      widget <- textInput(ns('Directory_parent'), 'Parent pipeline', 
                          value = isolate(rv.widgets$Directory_parent),
                          width='100px')
      
      toggleWidget(widget, rv$steps.enabled['Directory'] 
                   && input$Step2_mode == 'process')
    })
    
    output$Directory_name_ui <- renderUI({
      widget <- textInput(ns('Directory_name'), 'Name', 
                          value = isolate(rv.widgets$Directory_name),
                          width='100px')
      toggleWidget(widget, rv$steps.enabled['Directory'] )
    })
    
    output$Directory_mdEditor_ui <- renderUI({
      
      shiny::div(
        #class = class,
        style = "margin-bottom: 15px;",
        # shiny::tags$label('label'),
        
        shiny::tabsetPanel(selected = 1,
                           type = "tabs",
                           
                           # text input:
                           shiny::tabPanel(
                             title = "Write Description md code",
                             value = 1,
                             shinyAce::aceEditor(ns("Directory_mdEditor"),
                                                 value = isolate(input$Directory_mdEditor),
                                                 mode = "markdown",
                                                 theme = 'github',
                                                 height = '300px',
                                                 readOnly = !rv$steps.enabled['Directory'])
                           ),
                           
                           
                           # MD preview:
                           shiny::tabPanel(
                             title = "Preview",
                             value = 2,
                             uiOutput(ns('Directory_preview_ui'))
                           )
        )
      )
    })
    
    
    output$Directory_preview_ui <- renderUI({
      req(input$Directory_mdEditor)
      shiny::div(class = "",
                 shiny::withMathJax(
                   shiny::HTML(
                     markdown::markdownToHTML(text = input$Directory_mdEditor,
                                              fragment.only = TRUE)
                   )
                 )
      )
    })
    
    
    
    output$Directory_btn_validate_ui <- renderUI({
      widget <-  actionButton(ns("Directory_btn_validate"), "Perform",
                              class = 'btn-info')
      toggleWidget(widget, rv$steps.enabled['Directory'] )
      
    })
    # >>> END: Definition of the widgets
    
    
    observeEvent(input$Directory_btn_validate, {
      # Do some stuff
      
      # !!! DO NOT MODIFY THE THREE FOLLOWINF LINES !!!
      dataOut$trigger <- Timestamp()
      dataOut$value <- rv$dataIn
      rv$steps.status['Directory'] <- global$VALIDATED
      
    })
    
    
    # <<< END ------------- Code for step 1 UI---------------
    
    
   
    
    
    # # >>> START ------------- Code for Write md UI---------------
    # 
    # output$Writemd <- renderUI({
    #   wellPanel(
    #     # Two examples of widgets in a renderUI() function
    #     uiOutput(ns('Writemd_editor_ui')),
    #     # Insert validation button
    #     # This line is necessary. DO NOT MODIFY
    #     uiOutput(ns('Writemd_btn_validate_ui'))
    #   )
    # })
    # 
    # 
    # output$Writemd_editor_ui <- renderUI({
    #   init <- 'test'
    #   widget <- shinyAce::aceEditor(ns("Writemd_editor"),
    #                                 value = input$Writemd_editor,
    #                                 mode = "markdown",
    #                                 theme = 'github',
    #                                 height = '100px',
    #                                 readOnly = !rv$steps.enabled['Writemd'])
    #   
    #   #toggleWidget(widget, rv$steps.enabled['Writemd'] )
    # })
    # 
    # 
    # 
    # output$Writemd_btn_validate_ui <- renderUI({
    #   widget <- actionButton(ns("Writemd_btn_validate"),
    #                          "Perform",
    #                          class = GlobalSettings$btn_success_color)
    #   toggleWidget(widget, rv$steps.enabled['Writemd'] )
    # })
    # 
    # observeEvent(input$Writemd_btn_validate, {
    #   # Do some stuff
    #   
    #   # DO NOT MODIFY THE THREE FOLLOWINF LINES
    #   dataOut$trigger <- Timestamp()
    #   dataOut$value <- rv$dataIn
    #   rv$steps.status['Writemd'] <- global$VALIDATED
    # })
    # 
    # # <<< END ------------- Code for Write md UI---------------
    # 
    
    
    
    
    # >>> START ------------- Code for step 3 UI---------------
    
    output$Addsteps <- renderUI({
      wellPanel(
        # Two examples of widgets in a renderUI() function
        uiOutput(ns('Addsteps_dyn_steps_ui')),
        uiOutput(ns('Addsteps_view_ui')),
        uiOutput(ns('Addsteps_selectStep_ui')),
        # Insert validation button
        # This line is necessary. DO NOT MODIFY
        uiOutput(ns('Addsteps_btn_validate_ui'))
      )
    })
    
    
    rv.custom$steps <- dyn_widgets_server('Addsteps_dyn_steps')
    
    output$Addsteps_dyn_steps_ui <- renderUI({
      widget <- div(id = 'div_Addsteps_dyn_steps',
                    dyn_widgets_ui(ns('Addsteps_dyn_steps'))
      )
      toggleWidget(widget, rv$steps.enabled['Addsteps'] )
    })
    
    # render the widget collection
    output$Addsteps_selectStep_ui <- renderUI({
      req(rv.custom$steps())
      
      widget <- selectInput(ns('Addsteps_selectStep'), 'Select step to configure',
                            choices = rv.custom$steps()$inputs,
                            width = '150px')
      toggleWidget(widget, rv$steps.enabled['Addsteps'] )
    })
    
    
    # render the widget collection
    output$Addsteps_view_ui <- renderUI({
      req(rv.custom$steps())

      # function to create widget
      create_widget = function(i){
        p(paste0(rv.custom$steps()$inputs[i], ' ', rv.custom$steps()$mandatory[i]))
      }
      
      lapply(1:length(rv.custom$steps()$inputs), create_widget)
    })
    
    
    
    
    output$Addsteps_btn_validate_ui <- renderUI({
      widget <- actionButton(ns("Addsteps_btn_validate"),
                             "Perform",
                             class = GlobalSettings$btn_success_color)
      toggleWidget(widget, rv$steps.enabled['Addsteps'] )
    })
    
    observeEvent(input$Addsteps_btn_validate, {
      # Do some stuff
      
      # !!! DO NOT MODIFY THE THREE FOLLOWINF LINES !!!
      dataOut$trigger <- Timestamp()
      dataOut$value <- rv$dataIn
      rv$steps.status['Addsteps'] <- global$VALIDATED
    })
    
    # <<< END ------------- Code for step 3 UI---------------
    
    # >>> START ------------- Code for step 'Save' UI---------------
    output$Save <- renderUI({
      tagList(
        # Insert validation button
        # This line is necessary. DO NOT MODIFY
        uiOutput(ns('Save_btn_validate_ui'))
        #uiOutput(ns('dl_ui'))
      )
    })
    
    # output$dl_ui <- renderUI({
    #   req(config@mode == 'process')
    #   req(rv$steps.status['Save'] == global$VALIDATED)
    #   dl_ui(ns('createQuickLink'))
    # })
    
    output$Save_files_ui <- renderUI({
      req(rv.custom$files)
      tagList(
        h5('Files created'),
        lapply(rv.custom$files, function(i)
          p(i))
      )
      
    })
    
    output$Save_btn_validate_ui <- renderUI({
      toggleWidget(
        actionButton(ns("Save_btn_validate"), "Create template",
                     class = GlobalSettings$btn_success_color),
        rv$steps.enabled['Save']
      )
    })
    observeEvent(input$Save_btn_validate, {
      # Do some stuff
      fullname <- paste0(input$Step2_parent, '_', input$Step2_name)
      
      miniConfig <- list(fullname = fullname,
                         mode = input$Step2_mode,
                         steps = rv.custom$steps()$inputs,
                         mandatory = rv.custom$steps()$mandatory
                         )
      
     
      rv.custom$files <- c(rv.custom$files, 
                    Create_md_file(mode = input$Step2_mode,
                                   fullname = fullname,
                                   path = rv.custom$path(),
                                   rawText = input$Step2_mdEditor)
                    )
      
      
      if (input$Step2_mode == 'module')
        rv.custom$files <- c(rv.custom$files, 
                      createExtraModule(name = input$Step2_name, path = rv.custom$path()))
      else {
        rv.custom$files <- c(rv.custom$files, 
                      createModuleTemplate(miniConfig, path = rv.custom$path()))
        rv.custom$files <- c(rv.custom$files, 
                      createExtraFunctions(rv.custom$path()))
      }
      
      # DO NOT MODIFY THE THREE FOLLOWINF LINES
      dataOut$trigger <- Timestamp()
      dataOut$value <- rv$dataIn
      rv$steps.status['Save'] <- global$VALIDATED
     # dl_server('createQuickLink', dataIn = reactive({rv$dataIn}))
      
    })
    # <<< END ------------- Code for step 3 UI---------------
    
    
    
    # Insert necessary code which is hosted by MagellanNTK
    # DO NOT MODIFY THIS LINE
    eval(parse(text = Module_Return_Func()))
  }
  )
}

