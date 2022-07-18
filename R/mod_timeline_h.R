#' timeline UI Function
#'
#' @description A shiny Module.
#' 
#' #'
#' @examples
#' if (interactive()) {
#'     ui <- fluidPage(
#'         actionButton("changePos", "Change position"),
#'         mod_timeline_h_ui("TLh")
#'     )
#'
#'     server <- function(input, output) {
#'         rv <- reactiveValues(
#'             status = c(0, 1, 0, 0),
#'             current.pos = 1,
#'             tl.tags.enabled = c(1, 1, 1, 1),
#'             position = NULL
#'         )
#'         config <- list(
#'             name = "Protein_Filtering",
#'             steps = c("Description", "Step1", "Step2", "Step3"),
#'             mandatory = c(TRUE, FALSE, TRUE, TRUE)
#'         )
#'         mod_timeline_h_server(
#'             id = "TLh",
#'             config = config,
#'             status = reactive({
#'                 rv$status
#'             }),
#'             position = reactive({
#'                 rv$current.pos
#'             }),
#'             enabled = reactive({
#'                 rv$tl.tags.enabled
#'             })
#'         )
#'     }
#'     shinyApp(ui, server)
#' }
#' 
#' @name mod_timeline_h
NULL


#'
#' @rdname mod_timeline_h
#'
#' @importFrom shiny NS tagList
#' @importFrom sass sass sass_file
#'
#' @export
#'
#' @return NA
#'
mod_timeline_h_ui <- function(id) {
    ns <- NS(id)
    fpath <- system.file("app/www/sass",
        "h_timeline.sass",
        package = "Magellan"
    )
    tagList(
        shinyjs::useShinyjs(),
        tags$div(
            shinyjs::inlineCSS(sass::sass(sass::sass_file(fpath))),
            uiOutput(ns("show_h_TL"))
        )
    )
}


#' @param id The `id` of the module,
#' @param config A static `list` (not `reactiveList`) containing the same elements
#' as the process module.
#' @param status A `reactive vector` which contain the status (validated,
#' skipped or undone) of each step of the process module. Its length is equal
#' to the number of steps.
#' @param position A `reactive integer` that reflects the position of the
#' current (active) step.
#' @param enabled A `reactive vector` of length the number of steps and which
#' indicate whether the step is enabled or disabled.
#'
#' @return NA
#'
#' @rdname mod_timeline_h
#'
#' @export
#'
mod_timeline_h_server <- function(id,
    config,
    status,
    position,
    enabled) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        UpdateTags <- reactive({
            tl_status <- rep("undone", length(config@steps))
            tl_status[which(config@mandatory)] <- "mandatory"
            tl_status[which(unlist(status()) == global$VALIDATED)] <- "completed"
            tl_status[which(unlist(status()) == global$SKIPPED)] <- "skipped"

            for (i in seq_len(length(enabled()))) {
                if (!enabled()[i]) {
                    tl_status[i] <- paste0(tl_status[i], "Disabled")
                }
            }

            tl_status[position()] <- paste0(tl_status[position()], " active")
            tl_status
        })

        output$show_h_TL <- renderUI({
            tags$div(
                class = "timeline",
                id = "timeline",
                lapply(
                    seq_len(length(config@steps)),
                    function(x) {
                        tags$li(
                            class = paste0("li ", UpdateTags()[x]),
                            tags$div(
                                class = "timestamp status",
                                tags$h4(config@steps[x])
                            )
                        )
                    }
                )
            )
        })
    })
}


## To be copied in the UI
# mod_timeline_ui("timeline_ui_1")

## To be copied in the server
# callModule(mod_timeline_server, "timeline_ui_1")
