#' @title Load dataset shiny module
#'
#' @description  A shiny Module to load a dataset.
#' @name mod_Load_Dataset
#' 
#' @examples
#' if (interactive()) {
#'     data(ft_na)
#'     ui <- mod_Load_Dataset_ui("load")
#'
#'     server <- function(input, output, session) {
#'         mod_Load_Dataset_server(id = "load")}
#'     shinyApp(ui = ui, server = server)
#' }
NULL

#' @param id xxx
#' @rdname mod_Load_Dataset
#'
#' @export
#'
mod_Load_Dataset_ui <- function(id) {

}


#' @param id xxx
#' @return NA
#'
#' @rdname mod_Load_Dataset
#'
#' @export
#'
mod_Load_Dataset_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        rv <- reactiveValues(data = NULL)

        modal <- function() {
            modalDialog(
                fileInput(ns("file"), "Open file",
                    multiple = FALSE,
                    accept = ".rds"
                ),
                footer = tagList(
                    modalButton("Cancel"),
                    actionButton(ns("ok"), "OK")
                )
            )
        }

        observe({showModal(modal())})

        observeEvent(input$Cancel, {removeModal()})

        observeEvent(input$ok, {
            req(input$file)
            # browser()
            rv$data <- readRDS(input$file$datapath)
            # rv$data <- load(input$file$datapath)
            removeModal()
        })

        reactive({
            rv$data
        })
    })
}
