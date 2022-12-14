#' @title Shiny example module `Process`
#'
#' @description
#' This module contains the configuration information for the corresponding 
#' pipeline. It is called by the nav_pipeline module of the package MagellanNTK
#' This documentation is for developers who want to create their own 
#' pipelines nor processes to be managed with `MagellanNTK`.
#'
#' @param id xxx
#' @param verbose A `boolean` that indicates whether to show some infos in the 
#' console and add the shiny module for debugging
#' @param tl.layout Additional parameters for mod_nav
#'
#' @rdname example_mod_workflow
#'
#' @author Samuel Wieczorek
#'
#' @importFrom utils data
#' @import shiny
#'
#' @export
#'
#' @return NA
#'
#' @example examples/example_run_workflow.R
#'
run_workflow <- function(id,
    verbose = FALSE,
    tl.layout = NULL) {
    if (missing(id)) {
        warning("'id' is required.")
        return(NULL)
    }
    if (!Found_Mod_Funcs(id)) {
        return(NULL)
    }

    options(shiny.fullstacktrace = verbose)


    ui <- fluidPage(
        tagList(
            mod_nav_ui(id),
            uiOutput("debugInfos_ui")
        )
    )


    #----------------------------------------------------------------------
    server <- function(input, output) {
        dataOut <- reactiveVal()

        dataIn <- mod_Load_Dataset_server("exemple")

        output$debugInfos_ui <- renderUI({
            req(verbose)
            mod_Debug_Infos_ui("debug_infos")
        })

        output$save_dataset_ui <- renderUI({
            req(dataOut())
            req(dataOut()$dataOut()$value)
            mod_dl_ui("saveDataset")

            mod_dl_server(
                id = "saveDataset",
                dataIn = reactive({dataOut()$dataOut()$value})
            )
        })

        observeEvent(req(dataIn()), {
            dataOut(mod_nav_server(
                id = id,
                verbose = verbose,
                dataIn = reactive({dataIn()}),
                tl.layout = tl.layout
                ))

            mod_Debug_Infos_server(
                id = "debug_infos",
                title = "Infos from shiny app",
                rv.dataIn = reactive({dataIn()}),
                dataOut = reactive({dataOut()$dataOut()$value})
            )
        })
    }

    shinyApp(ui = ui, server = server)
}
