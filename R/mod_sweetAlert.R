#' @title xxx
#' @description xxxxx
#' 
#' 
#' @examplesIf interactive()
#' shiny::runApp(mod_SweetAlert('my title', 'my message'))
#' 
#' @name mod_sweetAlert
#' 
NULL




#' @export
#' @rdname mod_sweetAlert
mod_SweetAlert_ui <- function(id) {}

#' @importFrom shinyWidgets sendSweetAlert
#' @importFrom rclipboard rclipButton
#' @importFrom shiny moduleServer p 
#' 
#' @export
#' @rdname mod_sweetAlert
#' 
mod_SweetAlert_server <- function(id, 
  title = NULL,
  text = NULL, 
  showClipBtn = TRUE,
  type = 'error'){
  
  
  shiny::moduleServer(id,
    function(input, output, session) {
      
      shinyWidgets::sendSweetAlert(
        session = session,
        title = NULL,
        text = tags$div(style = "display:inline-block; vertical-align: top;",
          p(text),
          if (showClipBtn)
            rclipboard::rclipButton(inputId = "clipbtn",
              label = "",
              clipText = text,
              icon = icon("copy"),
              class = actionBtnClass)
        ),
        type = type,  #success, info, question, error,
        danger_mode = FALSE,
        closeOnClickOutside = TRUE,
        showCloseButton = FALSE
      )
      
    }
  )
}





mod_SweetAlert <- function(title, text, type = "warning"){
ui <- fluidPage(
  #actionButton('test', 'Test')
)

server <- function(input, output) {
  
  
 # observeEvent(input$test, {
    mod_SweetAlert_server('test',
      title = title,
      text = text,
      showClipBtn = FALSE,
      type = type)
 # })
}

app <- shinyApp(ui, server)
}




