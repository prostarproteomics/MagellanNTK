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
Load_Dataset_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("file_ui"))
}


#' @param id xxx
#' @return xxxxx
#'
#' @rdname Load_Dataset
#'
#' @export
#'
Load_Dataset_server <- function(id, path = reactive({NULL})) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    rv <- reactiveValues(dataOut = NULL)
    
    
    output$file_ui <- renderUI({
      req(path())
      print('toto')
      print(path())
      fileInput(ns("file"), "Open file", multiple = FALSE)
    })
    
    observe({
      req(input$file)
      
      ext <- unlist(strsplit(input$file$name, '.', fixed=TRUE))[2]
      rv$dataOut <- readRDS(input$file$datapath)
    })
    
    reactive({rv$dataOut})
  })
}
