
#'
#' @rdname timelines
#'
#' @importFrom shiny NS tagList
#' @importFrom sass sass sass_file
#' @return NA
#' @export
#'
timeline_h_ui <- function(id) {
    ns <- NS(id)
    fpath <- system.file("app/www/sass",
        "h_timeline.sass",
        package = "MagellanNTK"
    )
    tagList(
        shinyjs::useShinyjs(),
        tags$div(
            shinyjs::inlineCSS(sass::sass(sass::sass_file(fpath))),
            uiOutput(ns("show_h_TL"))
        )
    )
}


#' @rdname timelines
#' 
#' @export
#'
timeline_h_server <- function(id,
                              config,
                              status,
                              position,
                              enabled) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        
        UpdateTags <- reactive({
          req(config@steps != '')

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
          req(config@steps != '')
          
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
