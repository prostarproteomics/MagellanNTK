#' @title mod_Load_Dataset
#' 
#' @description  A shiny Module.
#' @param id xxx
#'
#' @rdname mod_Load_Dataset
#' 
#' @export
#' 
mod_Load_Dataset_ui <- function(id){

}


#' @return xxx 
#' 
#' @rdname mod_Load_Dataset
#' 
#' @export
#' 
mod_Load_Dataset_server <- function(id){
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    rv <- reactiveValues(data = NULL)

    modal <- function(){
      modalDialog(
        fileInput(ns("file"), "Open file", 
                  multiple = FALSE,
                  accept = ".rds"),
        footer = tagList(
          modalButton('Cancel'),
          actionButton(ns('ok'), 'OK')
        )
      )
    }

    observe({showModal(modal())})
    
    observeEvent(input$Cancel, {removeModal()})
    
    observeEvent(input$ok, { 
      req(input$file)
      rv$data <- readRDS(input$file$datapath)
      removeModal()
    })
  
    reactive({rv$data})
  })
  
}

