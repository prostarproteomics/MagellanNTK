#' @title Error modal shiny module.
#' 
#' @description A shiny module that shows messages in modal.
#' 
#' @param id internal
#' @param msg xxxx
#'
#' @name errorModal
#' 
#' @examples
#' \dontrun{
#' shiny::runApp(errorModal("my error text"))
#' }
#'
NULL


#' @rdname errorModal
#'
#' @export
#'
errorModal_ui <- function(id) {}

#' @rdname errorModal
#' @return NA
#' @import shiny
#' @export
#'
errorModal_server <- function(id, msg){
  
  ishiny::moduleServer(id, function(input, output, session) {
      
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
                HTML(paste('> ', msg, collapse = "<br/>"))
              )
              ,easyClose = TRUE)
          ))
      })
    }
  )
}





#' @export
#' @rdname errorModal
#' 
errorModal <- function(msg){
ui <- fluidPage(
  errorModal_ui(id = 'ex')
)

server <- function(input, output) {
  errorModal_server(id = "ex", msg = msg)
}

app <- shiny::shinyApp(ui, server)
}

