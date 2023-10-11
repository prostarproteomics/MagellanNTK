#' @title Shiny example module `Process`
#'
#' @description
#' This module contains the configuration information for the corresponding 
#' pipeline. It is called by the nav_pipeline module of the package MagellanNTK
#' This documentation is for developers who want to create their own 
#' pipelines nor processes to be managed with `MagellanNTK`.
#'
#' @param id xxx
#' @param dataIn xxx
#' @param tl.layout Additional parameters for nav
#' @param mode xxx
#'
#' @rdname example_workflow
#' 
#' @example inst/extdata/funcs_examples/example_run_workflow.R
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
#'
run_workflow <- function(id,
                         dataIn = NULL,
                         tl.layout = NULL,
                         mode = 'user') {
  
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
            req(mode == 'dev')
          Debug_Infos_server(id = 'debug_infos',
                             title = 'Infos from shiny app',
                             rv.dataIn = reactive({rv$dataIn}),
                             dataOut = reactive({rv$dataOut$dataOut()})
          )
            Debug_Infos_ui("debug_infos")
        })

        output$save_dataset_ui <- renderUI({
            req(c(dataOut(), dataOut()$dataOut()$value))

            dl_ui("saveDataset")
            dl_server(
                id = "saveDataset",
                dataIn = reactive({dataOut()$dataOut()$value})
            )
        })

        observeEvent(dataIn, {
            dataOut(
              nav_server(id = id,
                         dataIn = reactive({dataIn}),
                         tl.layout = tl.layout
                         )
              )
          })
    }

    shinyApp(ui, server)
}
