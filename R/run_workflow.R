#' @title Shiny example module `Process`
#'
#' @description
#' This module contains the configuration information for the corresponding 
#' pipeline. It is called by the nav_pipeline module of the package MagellanNTK
#' This documentation is for developers who want to create their own 
#' pipelines nor processes to be managed with `MagellanNTK`.
#'
#' @param id xxx
#' @param tl.layout Additional parameters for nav
#' @param path The path to the directory where are the source code files 
#' for processes and pipelines
#'
#' @rdname example_workflow
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
  dataIn = NULL,
  tl.layout = NULL,
  path = NULL) {
  
    if (missing(id))
        stop("'id' is required.")
    
  ui <- fluidPage(
        tagList(
            nav_ui(id),
            uiOutput("debugInfos_ui")
        )
    )


    #----------------------------------------------------------------------
    server <- function(input, output) {
        dataOut <- reactiveVal()

        
        output$debugInfos_ui <- renderUI({
            req(rv.core$mode == 'dev')
            Debug_Infos_ui("debug_infos")
        })

        output$save_dataset_ui <- renderUI({
            req(dataOut())
            req(dataOut()$dataOut()$value)
            dl_ui("saveDataset")

            dl_server(
                id = "saveDataset",
                dataIn = reactive({dataOut()$dataOut()$value})
            )
 
        })

        observeEvent(dataIn, {
            dataOut(nav_server(
                id = id,
                dataIn = reactive({dataIn}),
                tl.layout = tl.layout,
                path = path
                ))

            Debug_Infos_server(
                id = "debug_infos",
                title = "Infos from shiny app",
                rv.dataIn = reactive({dataIn}),
                dataOut = reactive({dataOut()$dataOut()$value})
            )
        })
    }

    shinyApp(ui, server)
}
