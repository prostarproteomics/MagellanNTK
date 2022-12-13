#' @title Load dataset shiny module
#'
#' @description  A shiny Module to load a dataset.
#' @name mod_Load_Dataset
#' 
#' @example examples/test_mod_load_dataset.R
NULL

#' @param id xxx
#' @rdname mod_Load_Dataset
#'
#' @export
#'
mod_Load_Dataset_ui <- function(id) {
}


#' @param id xxx
#' @return xxxxx
#'
#' @rdname mod_Load_Dataset
#'
#' @export
#'
mod_Load_Dataset_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        rv <- reactiveValues(dataOut = NULL)

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
            rv$dataOut <- readRDS(input$file$datapath)
             removeModal()
        })

        reactive({rv$dataOut})
    })
}
