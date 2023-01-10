#' @title Read a file
#' 
#' @description This function is the implementation of the empty readFiles
#' function (used in MagellantNTK)
#' 
#' 
#' @param name xxx
#' @param path xxx
#' 
#' @export
#' 
readFile <- function(name, path){
  
  ext <- unlist(strsplit(name, '.', fixed=TRUE))[2]
  object <- readRDS(path)
  
  return(object)
  
}





#' @export
custom_openfile_ui <- function(id){
  ns <- NS(id)
  tagList(
    fileInput(ns('filename'), "Select file")
  )
}

#' @export
custom_openfile_server <- function(id, path = reactive({NULL})){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  
  rv <- reactiveValues(dataOut = NULL)
  
  
  output$file_ui <- renderUI({
    #req(path())
    fileInput(ns("file"), "Open file", multiple = FALSE)
  })
  
  observe({
    req(input$file)
    
    #ext <- unlist(strsplit(input$file$name, '.', fixed=TRUE))[2]
    rv$dataOut <- readFile(input$file$name, input$file$datapath)
  })
  
  reactive({rv$dataOut})
})
}
