#' @title mod_dl
#'
#' @description  A shiny Module.
#'
#' @name mod-dl
#' @examples
#' if (interactive()) {
#'     data(ft_na)
#'     ui <- mod_dl_ui("dl")
#'
#'     server <- function(input, output, session) {
#'         mod_dl_server(
#'             id = "dl",
#'             dataIn = reactive({ft_na})
#'         )}
#'     shinyApp(ui = ui, server = server)
#' }
#'
NULL


#' @param id xxx
#'
#' @rdname mod-dl
#'
#' @export
#'
mod_dl_ui <- function(id) {
    ns <- NS(id)
    downloadLink(ns("download"), "Download dataset")
}

#' @param id xxx
#' @param dataIn xxx
#'
#' @return NA
#'
#' @rdname mod-dl
#'
#' @export
#'
mod_dl_server <- function(id,
    dataIn = reactive({NULL})) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        output$download <- downloadHandler(
            filename = function() {
                # paste0(input$nameExport, '.rds')
                paste0("foo.rds")
            },
            content = function(file) {
                fname <- tempfile()
                saveRDS(dataIn(), file = fname)
                file.copy(fname, file)
                file.remove(fname)
                removeModal()
            }
        )
    })
}
