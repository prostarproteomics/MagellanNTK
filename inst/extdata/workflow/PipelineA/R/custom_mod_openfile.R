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
    box(
      title = "Open file", status = "primary", solidHeader = TRUE,
      collapsible = TRUE,
      fileInput(ns('file'), "Select file", multiple = FALSE)
    ),
    
    box(
      title = "Convert dataset", status = "primary", solidHeader = TRUE,
      'Work in progress...'
    )
  )
}

#' @export
custom_openfile_server <- function(id, path = reactive({NULL})){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  
  dataOut <- reactiveValues(
    dataset = NULL,
    name = NULL
  )
  
  observeEvent(req(input$file),{
     dataOut$dataset <- readFile(input$file$name, input$file$datapath)
     dataOut$name <- input$file$name
  })
  
  return(
    list(
      data = reactive({dataOut$dataset}),
      name = reactive({dataOut$name})
      )
  )
})
}
