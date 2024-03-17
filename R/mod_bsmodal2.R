#' # ouvre une fenetre, parametre renseigne quoi afficher dans la fenetre
#' 
#' #' @title Predefined modal
#' #'
#' #' @description Displays of formatted modal-dialog with 'Cancel' and 
#' #' 'Ok' buttons.
#' #' 
#' #' @param id A `character(1)` which is the id of the instance of the module
#' #' @param title A `character(1)`
#' #' @param width A `character(1)` indicating the size of the modal window. Can 
#' #' be "s" for small (the default), "m" for medium, or "l" for large.
#' #' @param uiContent The content of the modal dialog.
#' #'
#' #' @importFrom shinyjqui jqui_draggable
#' #' 
#' #' @name bsmodal
#' #' 
#' #' @return A Shiny modal-dialog
#' #'
#' NULL
#' 
#' #'
#' #' @rdname bsmodal
#' #'
#' #' @export
#' #'
#' bsmodal_ui <- function(id){
#'   ns <- NS(id)
#'   tagList(
#'     uiOutput(ns("bsmodal_ui"))
#'   )
#' }
#' 
#' 
#' #' @export
#' #'
#' #' @rdname bsmodal
#' #' 
#' #' @import shiny
#' #' @import shinyBS
#' #' @import shinyjqui
#' #'
#' bsmodal_server <- function(id,
#'                            title = NULL,
#'                            width = NULL,
#'                            uiContent = NULL){ #height auto
#'   
#'   moduleServer(id, function(input, output, session){
#'     ns <- session$ns
#'     
#'     if (!requireNamespace("shiny", quietly = TRUE)) {
#'       stop("Please install shiny: BiocManager::install('shiny')")
#'     }
#'     if (!requireNamespace("shinyBS", quietly = TRUE)) {
#'       stop("Please install shinyBS: BiocManager::install('shinyBS')")
#'     }
#'     if (!requireNamespace("shinyjqui", quietly = TRUE)) {
#'       stop("Please install shinyjqui: BiocManager::install('shinyjqui')")
#'     }
#'     
#'     
#'     # jqui_resizable(paste0("#",ns("window")," .modal-content")
#'     #                ,options = list(minHeight = 500, minWidth=500  ))
#'     
#'     # jqui_draggable(paste0("#",ns("window")," .modal-content")
#'     #                , options = list(revert=FALSE))
#'     
#'     
#'     output$bsmodal_ui <- renderUI({
#'       
#'       if (is.null(width)) {
#'         width <- "small"
#'       }
#'       
#'       tagList(
#'         tags$head(tags$style(paste0(".modal-dialog { width:", width, " }"))),
#'         tags$head(tags$style(".modal-dialog {z-index: 1000;}")),
#'         tags$head(
#'           tags$style("#test .modal-dialog {width: fit-content !important;}")),
#'         actionButton(ns("openModalBtn"), "",
#'                      icon("chart-bar", lib = "font-awesome"),
#'                      class = "btn-success"
#'         ),
#'         shinyBS::bsModal(ns("window"),
#'                          
#'                          title = title,
#'                          trigger = ns("openModalBtn"),
#'                          footer = NULL,
#'                          uiContent
#'         )
#'         #  )
#'       )
#'     })
#'   })
#' }
#' 
#' 
#' 
#' # Example
#' #
#' library(shiny)
#' library(shinyBS)
#' 
#' ui <- fluidPage(
#'   bsmodal_ui(id = "tbl")
#' )
#' 
#' server <- function(input, output) {
#'   
#'   bsmodal_server(id = "tbl",
#'                  title = "test",
#'                  uiContent = p("test")
#'   )
#' }
#'   
#' shinyApp(ui = ui, server = server)
#' 
#' 
#' 
#' # Example in a module
#' #
#' library(shiny)
#' library(shinyBS)
#' 
#' 
#' loadapp_ui <- function(id){
#'   ns <- NS(id)
#'   uiOutput(ns('tutu'))
#' }
#' 
#' loadapp_server <- function(id){
#'   moduleServer(id, function(input, output, session){
#'     ns <- session$ns
#'     
#'     
#'     output$tutu <- renderUI({
#'       bsmodal_ui(id = ns("tbl"))
#'     })
#'     
#'     bsmodal_server(id = "tbl",
#'                    title = "test2",
#'                    uiContent = p("test2")
#'     )
#'     })
#' }
#' 
#' 
#' 
#' ui <- fluidPage(
#'   loadapp_ui(id = "tbl1")
#' )
#' 
#' server <- function(input, output) {
#'   
#'   loadapp_server(id = "tbl1")
#' }
#' 
#' shinyApp(ui = ui, server = server)
#' 
