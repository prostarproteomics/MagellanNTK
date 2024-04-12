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
#' In this example, `PipelineDemo_Process1_ui()` and `PipelineDemo_Process1_server()` define
#' the code for the process `Process1` which is part of the pipeline called `PipelineDemo`.
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
    steps = c('Create template', 'Custom dataset functions', 'Configure steps'),
    mandatory = c(TRUE, FALSE, FALSE)
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
    Createtemplate_mode = '',
    Createtemplate_parent = NULL,
    Createtemplate_name = NULL,
    #Createtemplate_mdEditor = '',
    #Createtemplate_mdEditorLayout = 'tabs',
    Configuresteps_selectStep = NULL,
    Customdatasetfunctions_addDatasets = default_add_func(),
    Customdatasetfunctions_keepDatasets = default_keep_func()
  )
  
  
  rv.custom.default.values <- list(
    path =  reactive({'~'}),
    tempdir = reactive({NULL}),
    files = NULL,
    rawmd = '',
    miniconfig = NULL
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
      path <- system.file('workflow/Tools/md', package='MagellanNTK')
      file <- file.path(path, md.file)
      
      tagList(
        ### In this example, the md file is found in the extdata/module_examples 
        ### Createtemplate but with a real app, it should be provided by the package 
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
      rv$steps.status['Description'] <- global$VALIDATED
    })
    
    
    
    # >>>
    # >>> START ------------- Code for step 1 UI---------------
    # >>> 
    
    # >>>> -------------------- STEP 1 : Global UI ------------------------------------
    output$Createtemplate <- renderUI({
      wellPanel(
        # uiOutput for all widgets in this UI
        # This part is mandatory
        # The renderUI() function of each widget is managed by MagellanNTK
        # The dev only have to define a reactive() function for each
        # widget he want to insert
        # Be aware of the naming convention for ids in uiOutput()
        # For more details, please refer to the dev document.
        
        
        fluidRow(
          column(width = 3, uiOutput(ns('Createtemplate_mode_ui'))),
          column(width = 3, uiOutput(ns('Createtemplate_parent_ui'))),
          column(width = 3, uiOutput(ns('Createtemplate_name_ui')))
        ),
        
        uiOutput(ns('Createtemplate_dyn_steps_ui')),
        uiOutput(ns('Createtemplate_view_ui')),
        
        # uiOutput(ns('Createtemplate_mdEditorLayout_ui')),
        # #uiOutput(ns('Createtemplate_mdEditor_ui')),
        # uiOutput(ns('Createtemplate_mdEditorPanels_ui')),
        # uiOutput(ns('Createtemplate_mdEditorTabs_ui')),

        # Insert validation button
        uiOutput(ns('Createtemplate_btn_validate_ui'))
      )
    })
    
    
    # >>> START: Definition of the widgets
    
    
    
    
    # rv.custom$foo <- foo_server('foo',
    #   obj = reactive({rv$dataIn}),
    #   reset = reactive({NULL}),
    #   is.enabled = reactive({rv$steps.enabled['Step1']})
    # )
    
 
    
    
    output$Createtemplate_mode_ui <- renderUI({
      widget <- selectInput(ns('Createtemplate_mode'), 'Template to create', 
                            choices = c('pipeline', 'process', 'module'), 
                            selected = isolate(rv.widgets$Createtemplate_mode),
                            width='100px')
      toggleWidget(widget, rv$steps.enabled['Createtemplate'] )
    })
    
    output$Createtemplate_parent_ui <- renderUI({
      widget <- textInput(ns('Createtemplate_parent'), 'Parent pipeline', 
                          value = isolate(rv.widgets$Createtemplate_parent),
                          width='100px')
      
      toggleWidget(widget, rv$steps.enabled['Createtemplate'] 
                   && input$Createtemplate_mode == 'process')
    })
    
    output$Createtemplate_name_ui <- renderUI({
      widget <- textInput(ns('Createtemplate_name'), 'Name', 
                          value = isolate(rv.widgets$Createtemplate_name),
                          width='100px')
      toggleWidget(widget, rv$steps.enabled['Createtemplate'] )
    })
    
    
    
    
    
    rv.custom$steps <- dyn_widgets_server('Createtemplate_dyn_steps')
    
    output$Createtemplate_dyn_steps_ui <- renderUI({
      widget <- div(id = 'div_Createtemplate_dyn_steps',
                    dyn_widgets_ui(ns('Createtemplate_dyn_steps'))
      )
      toggleWidget(widget, rv$steps.enabled['Createtemplate'] )
    })
    
    output$Createtemplate_view_ui <- renderUI({
      req(rv.custom$steps())
      
      # function to create widget
      create_widget = function(i){
        p(paste0(rv.custom$steps()$steps[i], ' ', rv.custom$steps()$mandatory[i]))
      }
      
      lapply(1:length(rv.custom$steps()$steps), create_widget)
    })
    
    
    
    
    
    
    
    
    
    
    output$Createtemplate_btn_validate_ui <- renderUI({
      widget <- actionButton(ns("Createtemplate_btn_validate"), "Perform",
                              class = 'btn-info')
      toggleWidget(widget, rv$steps.enabled['Createtemplate'] )
      
    })
    # >>> END: Definition of the widgets
    
    
    observeEvent(input$Createtemplate_btn_validate, {
      # Do some stuff
      
      rv.custom$tempdir <- tempdir()
      R.path <- file.path(tempdir(), 'R')
      md.path <- file.path(tempdir(), 'md')
      
      if (!dir.exists(R.path))
        dir.create(R.path)
      
      if (!dir.exists(md.path))
      dir.create(md.path)
      
      if (input$Createtemplate_mode == 'pipeline')
        fullname <- input$Createtemplate_name
      else if (input$Createtemplate_mode == 'process')
        fullname <- paste0(input$Createtemplate_parent, '_', input$Createtemplate_name)
      
      rv.custom$miniConfig <- list(fullname = fullname,
                         mode = input$Createtemplate_mode,
                         steps = rv.custom$steps()$steps,
                         mandatory = rv.custom$steps()$mandatory
      )
      
      if (input$Createtemplate_mode == 'module')
        rv.custom$files <- c(rv.custom$files, 
                             file.path('R', createExtraModule(name = input$Createtemplate_name, 
                                               path = rv.custom$tempdir)))
      else {
        rv.custom$files <- c(rv.custom$files, 
                             file.path('R', createModuleTemplate(rv.custom$miniConfig, 
                                                  path = rv.custom$tempdir)))
   
      }
      
      
      # !!! DO NOT MODIFY THE THREE FOLLOWINF LINES !!!
      dataOut$trigger <- Timestamp()
      dataOut$value <- rv$dataIn
      rv$steps.status['Createtemplate'] <- global$VALIDATED
      
    })
    
    
    # <<< END ------------- Code for step 1 UI---------------
    
    
    
    
    # >>> START ------------- Code for Custom dataset functions  UI---------------
    
    output$Customdatasetfunctions <- renderUI({
      wellPanel(
        # Two examples of widgets in a renderUI() function
        h3('Custom addDataset() function'),
        uiOutput(ns('Customdatasetfunctions_addDatasets_ui')),
        h3('Custom keepDataset() function'),
        uiOutput(ns('Customdatasetfunctions_keepDatasets_ui')),
        # Insert validation button
        # This line is necessary. DO NOT MODIFY
        uiOutput(ns('Customdatasetfunctions_btn_validate_ui'))
      )
    })
    
    
       # render the widget collection
    
    output$Customdatasetfunctions_addDatasets_ui <- renderUI({
      
      widget <- shinyAce::aceEditor(ns("Customdatasetfunctions_addDatasets"),
                          value = isolate(rv.widgets$Customdatasetfunctions_addDatasets),
                          mode = "r",
                          theme = 'github',
                          height = '300px')
      
      toggleWidget(widget, condition = rv$steps.enabled['Customdatasetfunctions'])
      
    })
    
    output$Customdatasetfunctions_keepDatasets_ui <- renderUI({
       
      widget <- shinyAce::aceEditor(ns("Customdatasetfunctions_keepDatasets"),
                          value = isolate(rv.widgets$Customdatasetfunctions_keepDatasets),
                          mode = "r",
                          theme = 'github',
                          height = '300px')
      
      toggleWidget(widget, condition = rv$steps.enabled['Customdatasetfunctions'])
      
    })
    
    output$Customdatasetfunctions_btn_validate_ui <- renderUI({
      widget <- actionButton(ns("Customdatasetfunctions_btn_validate"),
                             "Perform",
                             class = btn_success_color)
      toggleWidget(widget, rv$steps.enabled['Customdatasetfunctions'] )
    })
    
    observeEvent(input$Customdatasetfunctions_btn_validate, {
      # Do some stuff
      keepDat <- input$Customdatasetfunctions_keepDatasets
      addDat <- input$Customdatasetfunctions_addDatasets
      
      rv.custom$files <- c(rv.custom$files, 
                           file.path('R', createExtraFunctions(path.dir = rv.custom$tempdir,
                                                               add_func = addDat,
                                                               keep_func = keepDat)))
      
      # !!! DO NOT MODIFY THE THREE FOLLOWINF LINES !!!
      dataOut$trigger <- Timestamp()
      dataOut$value <- rv$dataIn
      rv$steps.status['Customdatasetfunctions'] <- global$VALIDATED
    })
    
    # <<< END ------------- Code for Step: Custom dataset functions UI---------------
    
    
    
   
    
    # >>> START ------------- Code for ConfigureSteps UI---------------
    
    output$Configuresteps <- renderUI({
      wellPanel(
        # Two examples of widgets in a renderUI() function
        uiOutput(ns('Configuresteps_preview_ui')),
        uiOutput(ns('Configuresteps_selectStep_ui')),
        shinyjs::hidden(uiOutput(ns('Configuresteps_editMd_ui'))),
        # Insert validation button
        # This line is necessary. DO NOT MODIFY
        uiOutput(ns('Configuresteps_btn_validate_ui'))
      )
    })
    
    
    observeEvent(input$Configuresteps_selectStep, {
      shinyjs::toggle('Configuresteps_editMd_ui', 
                      condition = input$Configuresteps_selectStep =='Description')
    })
    
    # render the widget collection
    output$Configuresteps_selectStep_ui <- renderUI({
      req(rv.custom$steps())

      widget <- selectInput(ns('Configuresteps_selectStep'), 'Select step to configure',
                            choices = c('Description', 
                                        rv.custom$steps()$steps,
                                        'Save'),
                            width = '150px')
      toggleWidget(widget, rv$steps.enabled['Configuresteps'] )
    })
    
    
    
    rv.custom$rawmd <- bsmodal_server(id = "Configuresteps_editMd",
                   label = "Edit md",
                   title = "test",
                   shiny.module = list(id = 'toto',
                                       ui.func = mdEditor_ui,
                                       ui.params = list(),
                                       server.func = mdEditor_server,
                                       server.params = list())
    )
    
    output$Configuresteps_editMd_ui <- renderUI({
      req(input$Configuresteps_selectStep == 'Description')
      widget <- div(id = ns('Configuresteps_editMd_div'),
                    bsmodal_ui(ns("Configuresteps_editMd"))
      )
      toggleWidget(widget, rv$steps.enabled['Configuresteps'] )
    })
    
    # render the widget collection
    
    
    
    
    
    output$Configuresteps_btn_validate_ui <- renderUI({
      widget <- actionButton(ns("Configuresteps_btn_validate"),
                             "Perform",
                             class = btn_success_color)
      toggleWidget(widget, rv$steps.enabled['Configuresteps'] )
    })

    observeEvent(input$Configuresteps_btn_validate, {
      # Do some stuff
      md_file <- Create_md_file(mode = input$Createtemplate_mode,
                                fullname = rv.custom$miniConfig$fullname,
                                path = rv.custom$tempdir,
                                rawText = rv.custom$rawmd())
      
      rv.custom$files <- c(rv.custom$files, file.path('md', md_file))
      
      # !!! DO NOT MODIFY THE THREE FOLLOWINF LINES !!!
      dataOut$trigger <- Timestamp()
      dataOut$value <- rv$dataIn
      rv$steps.status['Configuresteps'] <- global$VALIDATED
    })
    
    # <<< END ------------- Code for step 3 UI---------------
    
    # >>> START ------------- Code for step 'Save' UI---------------
    output$Save <- renderUI({
      tagList(
        # Insert validation button
        # This line is necessary. DO NOT MODIFY
        
        uiOutput(ns('Save_chooseDir_ui')),
        #uiOutput(ns('Createtemplate_guess_ui')),
        uiOutput(ns('Save_warnDir_ui')),
        uiOutput(ns('Save_files_ui')),
        uiOutput(ns('Save_btn_validate_ui'))
        #uiOutput(ns('dl_ui'))
      )
    })
    
    # output$dl_ui <- renderUI({
    #   req(config@mode == 'process')
    #   req(rv$steps.status['Save'] == global$VALIDATED)
    #   dl_ui(ns('createQuickLink'))
    # })
    
    
    rv.custom$path <- chooseDir_server('Save_chooseDir',
                                       path = reactive({'~'}),
                                       is.enabled = reactive({rv$steps.enabled['Save']}))
    
    
    output$Save_chooseDir_ui <- renderUI({
      widget <- div(id = ns('div_Save_chooseDir'),
                    chooseDir_ui(ns('Save_chooseDir')))
      toggleWidget(widget, rv$steps.enabled['Save'] )
    })
    
    
    output$Save_warnDir_ui <- renderUI({
      req(rv.custom$path())
      .path <- gsub('\\', '/', rv.custom$path(), fixed = TRUE)
      pattern <- c('R', 'md')
      dirs <- list.dirs(.path, recursive = FALSE, full.names=FALSE)
      if (length(dirs) == 0)
        msg <- pattern
      else
        msg <- pattern[-which(intersect(dirs, pattern) == pattern)]
      
      lapply(msg, function(i) 
        p(paste0("The '", i, "' Createtemplate does not exists. It will be created")))
    })  
    
    # # This part must be customized by the developer of a new module
    # output$Createtemplate_guess_ui <- renderUI({
    #   req(rv.custom$path())
    #   lst.files <- list.files(file.path(rv.custom$path(), 'R'))
    #   lapply(lst.files, function(i)
    #     p(i))
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
                     class = btn_success_color),
        rv$steps.enabled['Save']
      )
    })
    observeEvent(input$Save_btn_validate, {
      # Do some stuff
      
      R.path <- file.path(rv.custom$path(), 'R')
      md.path <- file.path(rv.custom$path(), 'md')
      
      if (!dir.exists(R.path))
        dir.create(R.path)
      
      if (!dir.exists(md.path))
        dir.create(md.path)
      
      for (f in rv.custom$files)
        file.copy(file.path(tempdir(), f), file.path(rv.custom$path(), f))
      
      print('Done')
                          
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

