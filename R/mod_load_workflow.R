#' @title Load dataset shiny module
#'
#' @description  A shiny Module to load a dataset.
#' @name Load_Dataset
#' 
#' @example examples/test_load_dataset.R
NULL

#' @param id xxx
#' @rdname Load_Dataset
#'
#' @export
#'
mod_load_workflow_ui <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyjs(),
    uiOutput(ns('chooseLoadUI')),
    hidden(
      div(id = ns('directory'),
        fluidRow(
      column(width=4, 
             uiOutput(ns('folder_ui'))),
      column(width=4, 
             shinyFiles::shinyDirButton(id = ns('sheets_dir'), 
                               label = "Folder select", 
                               title = "Sheets Folder Selector")
             ),
    #verbatimTextOutput(ns("sheets_dir"))),
    column(width=4, 
           disabled(actionButton(ns('load'), 'Load directory', class = 'btn-primary')))
    )
      )
    ),
    hidden(
      div(id = ns('plugin'), 
          fluidRow(
            column(width=8, 
                   uiOutput(ns('pluginsUI'))),
            column(width=4, 
                   disabled(actionButton(ns('loadPlugin'), 'Load plugin', class = 'btn-primary'))
            )
          )
          
          )

      ),
    uiOutput(ns('wf_summary'))
  )
}


#' @param id xxx
#' @return xxxxx
#'
#' @rdname Load_Dataset
#'
#' @export
#'
mod_load_workflow_server <- function(id, 
                                     path = reactive({NULL}),
                                     mode = reactive({'user'})) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    dataOut <- reactiveValues(
      folder = NULL,
      workflow = NULL,
      package = NULL
      )
    
    
    rv <- reactiveValues(
      folder = NULL,
      workflow = NULL,
      package = NULL,
      is.valid = FALSE
      )
    
    
    
    output$chooseLoadUI <- renderUI({
      mode()
      .choices <- switch(mode(),
                         dev = c('directory', 'plugin'),
                         user = c('plugin')
                         )
      
      radioButtons(ns('chooseLoad'), 'Open a workflow', 
                   choices = .choices, 
                   selected = character(0))
      
    })
    
    
    observeEvent(input$chooseLoad, {
      toggle('directory', condition = input$chooseLoad == 'directory')
      toggle('plugin', condition = input$chooseLoad == 'plugin')
    })
    
    
    ###
    ###
    ### Load from a directory
    ###
    ###
    Theroots <- reactive({
      root <- input$root
      #req(root, dir.exists(root))
      
      if(length(root) == 0 || root == ""){
        volumes <- getVolumes()()
        c(volumes)
      } else{
        c(project_root = root)
      }
    })
    
    
    output$folder_ui <- renderUI({
      textInput(ns("root"), 
                "Please enter your workflow folder:",
                value = path()
      )
    })
    
   
    
    
    observe({
      shinyDirChoose(input, 'sheets_dir', roots = Theroots(), session = session)
      rv$folder <- parseDirPath(roots = Theroots(), input$sheets_dir)
      #browser()
      if (length(rv$folder) > 0){
          rv$workflow <- unlist(input$sheets_dir$path)[length(unlist(input$sheets_dir$path))]
          toggleState('load', condition = TRUE)
          
      }
        #browser()
      #rv$is.valid <- CheckWorkflowDir(rv$folder)
    })
    
    
    # Source files once the Load button has been clicked
    observeEvent(input$load, {
      
      # Loading source files
      lst.code <- list.files(file.path(rv$folder, 'R'))
      lapply(lst.code, 
             function(x)
               source(file.path(rv$folder, 'R', x), local=FALSE)
      )
      
      dataOut$folder <- rv$folder
      dataOut$workflow <- rv$workflow
    })
    
    # output$sheets_dir <- renderPrint({
    #   req(rv$folder)
    #   rv$folder
    #   rv$workflow
    # })
    
    
    # GetWorkflowNames <- reactive({
    #   req(rv$folder)
    #   lst <- list.files(rv$folder)
    #   #path.md <- file.path(rv$folder, 'md')
    #   #lst <- lst[-which(grepl( 'Description', lst))]
    #   #lst <- gsub('.md', '', lst)
    #   lst
    # })

    # output$chooseWorkflowUI <- renderUI({
    #   req(rv$folder)
    #   selectInput(ns('select_wf'),
    #               'Choose workflow',
    #               choices = GetWorkflowNames(),
    #               width = '150px')
    # })
    # 
    
    
    
    ###
    ###
    ### Load a plugin already installed
    ###
    ###
    
    
    
    GetAvailablePlugins <- reactive({
      plugins <- pkgs <- NULL
      for (pkg in installed.packages()[,'Package']){
        path <- system.file('workflows', package=pkg)
        tmp <- list.files(path, recursive=FALSE)
        if (length(tmp) > 0){
          pkgs <- c(pkgs, rep(pkg, length(tmp)))
          plugins <- c(plugins, tmp)
        }
      }
      
      list(
        pkgs = pkgs,
        plugins = plugins
      )
    })
    
    
    
    output$pluginsUI <- renderUI({
      
      #.choices <- setNames(GetAvailablePlugins()$plugins,
      #                     nm = paste0(GetAvailablePlugins()$plugins, ' (', GetAvailablePlugins()$pkgs, ')'))
      .choices <- paste0(GetAvailablePlugins()$plugins, ' (', GetAvailablePlugins()$pkgs, ')')
      selectInput(ns('plugins'), 'Choose a workflow', 
                  choices = .choices)
      
    })
    
    observeEvent(req(input$plugins),{
      
      tmp <- strsplit(input$plugins, split=' (', fixed=TRUE)
      rv$workflow <- unlist(tmp)[1]
      rv$package <- gsub(')', '', unlist(tmp)[2])
      
      toggleState('loadPlugin', condition = TRUE)

    })
    
    observeEvent(input$loadPlugin, {
     
      # Loading source files
      
      library(rv$package, character.only = TRUE)
      
      dataOut$package <- rv$package
      dataOut$workflow <- rv$workflow
    })
    
    
    ###
    ###
    ### Communs functions
    ###
    ###
    
    
    output$wf_summary <- renderUI({
      req(rv$workflow)
      

      if (!is.null(rv$folder) && length(rv$folder) == 1){
       file <- file.path(rv$folder, 'md', 'summary.md')
      } else if (!is.null(rv$package) && length(rv$package) == 1){
         file <- file.path(system.file('workflows', package = rv$package), 
                           rv$workflow, 'md', 'summary.md')
      }
       
       mod_insert_md_server('summary', file)
       
       box(
         title = "Inputs",
         status = "warning",
         solidHeader = TRUE,
         mod_insert_md_ui(ns('summary'))
      )

      
    })
    

    
    output$checkDirs <- renderUI({
      req(rv$folder)
      
      txt <- paste0('<img src=\"',
        system.file("app/www/images/Problem.png", package="MagellanNTK"), 
        '\", height="24"></img>')
      
      
      tagList(
        h3('md directory '), 
        HTML(txt),
    
    #dir.exists(file.path(rv$folder, 'md')))),
        h3(paste0('R directory ', dir.exists(file.path(rv$folder, 'R')))),
        h3(paste0('data directory ', dir.exists(file.path(rv$folder, 'data'))))
      )

      
    })
    
    
    
    
     return(
       list(folder = reactive({dataOut$folder}),
            workflow = reactive({dataOut$workflow}),
            package = reactive({dataOut$package})
            )
     )    
  })
}
