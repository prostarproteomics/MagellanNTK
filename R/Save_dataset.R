#' @title Load dataset shiny module
#'
#' @description  A shiny Module to load a dataset.
#' @name Save_Dataset
#' 
NULL

#' @param id xxx
#' @rdname Save_Dataset
#'
#' @export
#'
Save_Dataset_ui <- function(id) {
  ns <- NS(id)
  downloadLink(ns('downloadData'), 'Download')
}


#' @param id xxx
#' @param data xxx
#' 
#' @return xxxxx
#'
#' @rdname Save_Dataset
#'
#' @export
#'
Save_Dataset_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # rv <- reactiveValues(
    #   dataIn = NULL,
    #   dataOut = NULL)
    # 
    output$downloadData <- downloadHandler(
      filename = function() {
        #paste('data-', input$files, "-", Sys.Date(), '.pdf', sep='')
        'temp.RData'
      },
      content = function(file) {
        #file.copy(paste0(input$files, ".pdf"), file)
        saveRDS(data(), file = 'temp.RData')
      }
    )
  })
}
