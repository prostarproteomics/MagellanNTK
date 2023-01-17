# ouvre une fenetre, parametre renseigne quoi afficher dans la fenetre


#' @title Predefined modal
#'
#' @description Displays of formatted modal-dialog with 'Cancel' and 
#' 'Ok' buttons.
#'
#' @rdname bsmodal
#'
#' @export
#'
apModal_ui <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyjs(),
    actionButton(ns("openModalBtn"), "",
                 icon("chart-bar", lib = "font-awesome"),
                 class = "btn-success"
    ),
    shinyjs::hidden(
      uiOutput(ns("apModalUI")))

  )
}


#' @param id A `character(1)` which is the id of the instance of the module
#' @param title A `character(1)`
#' @param width A `character(1)` indicating the size of the modal window. Can 
#' be "s" for small (the default), "m" for medium, or "l" for large.
#' @param uiContent The content of the modal dialog.
#'
#' @importFrom shinyjqui jqui_draggable
#'
#' @export
#'
#' @return A Shiny modal-dialog
#'
#' @examples
#' if (interactive()) {
#'     library(shiny)
#'     library(shinyBS)
#'
#'     ui <- fluidPage(
#'         bsmodal_ui("tbl")
#'     )
#'     server <- function(input, output) {
#'         bsmodal_server(
#'             id = "tbl",
#'             title = "test",
#'             uiContent = p("test")
#'         )}
#'     shinyApp(ui, server)
#' }
#'
#' @rdname bsmodal
#'
apModal_server <- function(id,
                           title = NULL,
                           width = NULL,
                           uiContent = NULL) { # height auto
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
    
    observeEvent(input$openModalBtn, { shinyjs::toggle('apModalUI')})
    
    output$apModalUI <- renderUI({
      tagList(
        absolutePanel(style='box-shadow: 10px 5px 5px black; 
                      z-index: 1000; 
                      background-color: lightgrey;
                      background-color: white;
  	                  opacity: 0.85;
  	                  padding: 20px 20px 20px 20px;
  	                  margin: auto;
  	                  border-radius: 5pt;
  	                  box-shadow: 0pt 0pt 6pt 0px rgba(61,59,61,0.48);
                      padding-bottom: 2mm;
                      padding-top: 1mm;',
            tagList(
              h3('EDA'),
              uiContent
              ),
            top = NULL, left = NULL, right = '50%', bottom = '50%',
            width = '200px', height = '400px',
            draggable = TRUE, fixed = FALSE,
            cursor = c("auto", "move", "default", "inherit"))
      )
    })
  })
}
