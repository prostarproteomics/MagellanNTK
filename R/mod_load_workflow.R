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
    h3('Load workflow'),
    uiOutput(ns('folder_ui')),
    shinyFiles::shinyDirButton(id = ns('sheets_dir'), 
                               label = "Folder select", 
                               title = "Sheets Folder Selector"),
    verbatimTextOutput(ns("sheets_dir")),
    #uiOutput(ns('checkDirs')),
    uiOutput(ns('wf_summary')),
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
mod_load_workflow_server <- function(id, path=reactive({NULL})) {
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
          toggleState('start', condition = TRUE)
          
      }
        #browser()
      #rv$is.valid <- CheckWorkflowDir(rv$folder)
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
    
    output$wf_summary <- renderUI({
      req(rv$folder)
       file <- file.path(rv$folder, 'md', 'summary.md')
       box(
         title = "Inputs", 
         status = "warning", 
         solidHeader = TRUE,
         mod_insert_md_ui(ns('summary'))
      )
      
      mod_insert_md_server('summary', file)

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
    
    observeEvent(input$start, {
      
      # Loading source files
      lst.code <- list.files(file.path(rv$folder, 'R'))
      lapply(lst.code, 
             function(x)
               source(file.path(rv$folder, 'R', x), local=FALSE)
      )
      
      dataOut$folder <- rv$folder
      dataOut$workflow <- rv$workflow
    })
    
    
     return(
       list(folder = reactive({dataOut$folder}),
            workflow = reactive({dataOut$workflow})
            )
     )    
  })
}
