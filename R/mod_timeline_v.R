#' @title Module timeline functions
#'
#' @description A shiny Module.
#' 
#' @name mod_timeline_v
#' 
#' @example examples/test_timeline_v.R
#' 
NULL

#'
#' @param id The `id` of the server
#'
#' @importFrom shiny NS tagList
#'
#' @rdname mod_timeline_v
#'
#' @author Samuel Wieczorek
#'
#' @export
#'
mod_timeline_v_ui <- function(id) {
    ns <- NS(id)

    # fpath <- system.file("app/www/sass",
    #                     "v_timeline.sass",
    #                     package="Magellan")
    div(
        shinyjs::useShinyjs(),
        # shinyjs::inlineCSS(sass::sass(sass::sass_file(fpath))),
        uiOutput(ns("show_v_TL"))
    )
}


#' @param id The `id` of the server
#' @param config A static `list` (not `reactiveList`) containing the same 
#' elements as the process module.
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
#'
#' @rdname mod_timeline_v
#'
#' @author Samuel Wieczorek
#'
#' @export
#'
mod_timeline_v_server <- function(id,
    config,
    status,
    position,
    enabled) {

    # Define colors for the different status of steps
    colCompletedDisabled <- "#ABDBAC"
    colCompleted <- "#07910A"
    colSkippedDisabled <- "#B3AFAB80"
    colSkipped <- "#B3AFAB"
    colMandatory <- "#D30505"
    colMandatoryDisabled <- "#D3050580"
    colUndone <- "#B3AFAB"
    colUndoneDisabled <- "#B3AFAB80"
    colLine <- "#C6C8C6"

    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        UpdateTags <- reactive({
            tl_status <- rep("undone", length(config@steps))
            
            .ind1 <- which(unlist(status()) == global$VALIDATED)
            tl_status[which(config@mandatory)] <- "mandatory"
            tl_status[.ind1] <- "completed"
            tl_status[which(unlist(status()) == global$SKIPPED)] <- "skipped"

            for (i in seq_len(length(enabled()))) {
                if (!enabled()[i]) {
                    tl_status[i] <- paste0(tl_status[i], "Disabled")
                }
            }

            tl_status[position()] <- paste0(tl_status[position()], " active")
            tl_status
        })


        # Builds css-styles tags for the different possible status
        active <- reactive({
            "box-shadow: 0 0 0 3px black;"
        })

        undone <- reactive({
            paste0(
                "background-color: white;color: black;border: 3px solid ",
                colUndone,
                ";"
            )
        })

        undoneDisabled <- reactive({
            paste0(
                "background-color: white;color: black;border: 3px solid ",
                colUndoneDisabled,
                ";"
            )
        })

        mandatory <- reactive({
            paste0(
                "background-color: white; color: black;border: 3px solid ",
                colMandatory,
                ";"
            )
        })

        mandatoryDisabled <- reactive({
            paste0(
                "background-color: white;color: black;border: 3px solid ",
                colMandatoryDisabled,
                ";"
            )
        })

        completed <- reactive({
            paste0(
                "background-color: ",
                colCompleted,
                ";border: 3px solid ",
                colCompleted,
                ";"
            )
        })

        completedDisabled <- reactive({
            paste0(
                "background-color: ",
                colCompletedDisabled,
                ";border: 3px solid ",
                colCompletedDisabled,
                ";"
            )
        })

        skipped <- reactive({
            paste0(
                "background-color: white; border: 3px dashed ",
                colSkipped,
                ";"
            )
        })

        skippedDisabled <- reactive({
            paste0(
                "background-color: white; border: 3px dashed ",
                colSkippedDisabled,
                ";"
            )
        })



        GetStyle <- reactive({
            tl_status <- rep(undone(), length(config@steps))
            tl_status[which(enabled() == 1)] <- undoneDisabled()
            
            .ens_A <- intersect(which(config@mandatory), which(enabled() == 1))
            tl_status[.ens_A] <- mandatory()
            
            .ensB <- intersect(which(config@mandatory), which(enabled() == 0))
            tl_status[.ensB] <- mandatoryDisabled()

            .ensC <- intersect(which(unlist(status()) == global$VALIDATED), 
                which(enabled() == 1))
            tl_status[.ensC] <- completed()
            
            .ensD <- intersect(which(unlist(status()) == global$VALIDATED), 
                which(enabled() == 0))
            tl_status[.ensD] <- completedDisabled()

            .ensE <- intersect(which(unlist(status()) == global$SKIPPED), 
                which(enabled() == 1))
            tl_status[.ensE] <- skipped()
            
            .ensF <- intersect(which(unlist(status()) == global$SKIPPED), 
                which(enabled() == 0))
            tl_status[.ensF] <- skippedDisabled()

            tl_status[position()] <- paste0(tl_status[position()], active())
            tl_status
        })

        output$show_v_TL <- renderUI({
            tags$div(
                style = "width: 150px;",
                lapply(seq_len(length(config@steps)), function(x) {
                    # tags$li(tags$p( class=UpdateTags()[x], config@steps[x]))
                    tags$p(
                        style = paste0(
                            "font-weight: 100;
                            border: 3px solid lightgrey;
                            border-radius: 10px;
                            display: block;color: #000;
                            padding: 8px 10px;margin: 10px;
                            text-align: center;",
                            GetStyle()[x]
                        ),
                        config@steps[x]
                    )
                })
            )
        })
    })
}

