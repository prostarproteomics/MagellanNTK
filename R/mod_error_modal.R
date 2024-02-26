#' @title Error modal shiny module.
#' 
#' @description A shiny module that shows messages in modal.
#' 
#' @param id internal
#' @param text xxxx
#'
#' @name errorModal
#' @example examples/test_errorModal.R
#'
NULL


#' @rdname errorModal
#'
#' @export
#'
errorModal_ui <- function(id) {}

#' @rdname errorModal
#' @return NA
#'
#' @export
#'
errorModal_server <- function(id, text){
  
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Please install shiny: BiocManager::install('shiny')")
  }
  
  shiny::moduleServer(id, function(input, output, session) {
      
      observeEvent(TRUE, ignoreInit = FALSE, {
        # shiny::showModal(
        #     shiny::modalDialog('test')
        # )
        
        shiny::showModal(
          div(
            id = 'errModal',
            tags$style("#errModal .modal-dialog{width: 600px;}"),
            shiny::modalDialog(
              h3("Error log"),
              tags$style("#tPanel {overflow-y:scroll; color: red;}"),
              shiny::wellPanel(
                id = "tPanel",
                HTML(paste('> ', text, collapse = "<br/>"))
              )
              ,easyClose = TRUE)
          ))
      })
    }
  )
}





###################################################################
#                             Example                             #
###################################################################

library(shiny)

ui <- fluidPage(
  errorModal_ui(id = 'ex')
)

server <- function(input, output) {
  
  
  errorModal_server(id = "ex", text = 'test')

}

shinyApp(ui = ui, server = server)

