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
Load_Workflow_ui <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyjs(),
    h3('Load workflow'),
    textInput(ns("root"), 
              "Please enter your workflow folder:"),
    shinyFiles::shinyDirButton(id = ns('sheets_dir'), 
                               label = "Folder select", 
                               title = "Sheets Folder Selector"),
    verbatimTextOutput(ns("sheets_dir")),
    uiOutput(ns('chooseWorkflowUI')),
    #mod_shinyTree_ui(ns("tree")),
    uiOutput(ns('checkDirs')),
    disabled(actionButton(ns('start'), 'Start'))
  )
}


#' @param id xxx
#' @return xxxxx
#'
#' @rdname Load_Dataset
#'
#' @export
#'
Load_Workflow_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    dataOut <- reactiveValues(
      folder = NULL,
      workflow = NULL
      )
    
    
    rv <- reactiveValues(
      folder = NULL,
      workflow = NULL,
      is.valid = FALSE
      )
    
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
    
    observe({
      shinyDirChoose(input, 'sheets_dir', roots = Theroots(), session = session)
      rv$folder <- parseDirPath(roots = Theroots(), input$sheets_dir)
      #rv$is.valid <- CheckWorkflowDir(rv$folder)
    })
    
    # output$sheets_dir <- renderPrint({
    #   req(rv$folder)
    #   rv$folder
    #   rv$workflow
    # })
    
    
    GetWorkflowNames <- reactive({
      req(rv$folder)
      lst <- list.files(rv$folder)
      #path.md <- file.path(rv$folder, 'md')
      #lst <- lst[-which(grepl( 'Description', lst))]
      #lst <- gsub('.md', '', lst)
      lst
    })

    output$chooseWorkflowUI <- renderUI({
      req(rv$folder)
      selectInput(ns('select_wf'),
                  'Choose workflow',
                  choices = GetWorkflowNames()
                  )
    })
    
   observe({rv$workflow <- input$select_wf})
    
    observe({
      req(rv$workflow)
      # The functions of the module server (and ui) are supposed to 
      # be already loaded. Check if it is the case. If not, show a 
      # message and abort
      # LoadCode(name = rv$workflow,
      #          path = file.path(rv$folder, rv$workflow, 'R'),
      #          recursive = TRUE
      #          )
      
      lst.code <- list.files(file.path(rv$folder, rv$workflow, 'R'))
      lapply(lst.code, 
             function(x)
               source(file.path(rv$folder, rv$workflow, 'R', x), local=FALSE)
        )
     toggleState('start', condition = TRUE)
    })
    
    
    
    
    output$checkDirs <- renderUI({
      req(rv$folder)
      
      txt <- paste0('<img src=\"',
        system.file("app/www/images/Problem.png", package="MagellanNTK"), 
        '\", height="24"></img>')
      
      print(txt)
      tagList(
        h3('md directory '), 
        HTML(txt),
    
    #dir.exists(file.path(rv$folder, 'md')))),
        h3(paste0('R directory ', dir.exists(file.path(rv$folder, 'R')))),
        h3(paste0('data directory ', dir.exists(file.path(rv$folder, 'data'))))
      )

      
    })
    
    observeEvent(input$start, {
      dataOut$folder <- rv$folder
      dataOut$workflow <- rv$workflow
    })
    
    
     reactive({dataOut})
    
  })
}
