#' @title Load dataset shiny module
#'
#' @description  A shiny Module to load a dataset.
#' @name Load_Dataset
#' 
#' @example examples/test_load_dataset.R
NULL

#' @param id xxx
#' @rdname Load_Dataset
#'
#' @export
#'
Load_Dataset_ui <- function(id) {
}


#' @param id xxx
#' @return xxxxx
#'
#' @rdname Load_Dataset
#'
#' @export
#'
Load_Dataset_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        rv <- reactiveValues(dataOut = NULL)

        modal <- function() {
            modalDialog(
                fileInput(ns("file"), "Open file", multiple = FALSE),
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

            ext <- unlist(strsplit(input$file$name, '.', fixed=TRUE))[2]
            rv$dataOut <- readRDS(input$file$datapath)
            removeModal()
        })

        reactive({rv$dataOut})
    })
}
