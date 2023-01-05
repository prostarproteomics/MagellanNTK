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
    textInput(ns("root"), "Please enter your project folder root:", value=''),
    shinyFiles::shinyDirButton(id = ns('sheets_dir'), label = "Folder select", title = "Sheets Folder Selector"),
    verbatimTextOutput(ns("sheets_dir")),
    uiOutput(ns('checkDirs'))
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
    
    rv <- reactiveValues(folder = NULL)
    
    Theroots <- reactive({
      print('reactive...')
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
    })
    
    output$sheets_dir <- renderPrint({
      req(rv$folder)
      rv$folder
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
    
    
    
    reactive({rv$folder})
  })
}
