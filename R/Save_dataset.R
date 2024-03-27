#' @title Load dataset shiny module
#'
#' @description  A shiny Module to load a dataset.
#' @name Save_Dataset
#' 
#' @param id xxx
#' @param data xxx
#' 
#' @examplesIf interactive()
#' data(sub_R25)
#' shiny::runApp(Save_Dataset(sub_R25))
#' 
#' 
NULL


#' @rdname Save_Dataset
#'
#' @export
#'
Save_Dataset_ui <- function(id) {
  ns <- NS(id)
  downloadLink(ns('downloadData'), 'Download')
}



#' @importFrom shiny downloadHandler moduleServer
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




#' @importFrom shiny fluidPage shinyApp
#' 
#' @return xxxxx
#'
#' @rdname Save_Dataset
#'
#' @export
#'
Save_Dataset <- function(data){
  ui <- Save_Dataset_ui(id = 'saveDataset')
  
  server <- function(input, output, session) {
    Save_Dataset_server(id = 'saveDataset', reactive({data}))
  }
  
  app <- shiny::shinyApp(ui, server)
}