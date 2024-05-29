#' @title xxx
#' @name mod_err_modal
#' 
#' @examplesIf interactive()
#' shiny::runApp(mod_errorModal('myTitle', 'myContent'))
#' 
NULL


#' @rdname mod_err_modal
#' @export
#' 
mod_errorModal_ui <- function(id) {}

#' @rdname mod_err_modal
#' @export
#' 
mod_errorModal_server <- function(id, 
                                  title = NULL,
                                  text = NULL, 
                                  footer = modalButton("Close")){
    

    shiny::moduleServer(id,
        function(input, output, session) {
            observeEvent(TRUE, ignoreInit = FALSE, {
                shiny::showModal(
                    div(
                        id = 'errModal',
                        tags$style("#errModal .modal-dialog{width: 600px;}"),
                        shiny::modalDialog(
                            h3(title, style='color: red;'),
                            tags$style("#tPanel {overflow-y:scroll; color: black; background: white;}"),
                            shiny::wellPanel(id = "tPanel",
                                HTML(paste(text, collapse = "<br/>")),
                                width = '250px'
                            ),
                            footer = footer,
                            easyClose = TRUE)
                    ))
            })
            }
        )
}
    



#' @rdname mod_err_modal
#' @export
#' 
mod_errorModal <- function(title = NULL, text = NULL){
  
  ui <- fluidPage()
  
server <- function(input, output) {
    mod_errorModal_server('test',
                          title = title,
                          text = text)
}
app <- shiny::shinyApp(ui, server)

}