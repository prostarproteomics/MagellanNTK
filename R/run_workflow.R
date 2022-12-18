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
    verbose = FALSE,
    tl.layout = NULL,
    path = NULL) {
  
    if (missing(id)) {
        warning("'id' is required.")
        return(NULL)
    }
    

    # if (is.null(path)){
    #     warning('xxxx')
    #     return (NULL)
    # } else {
    #     # Load source code files for processes
    #     for (l in list.files(path = path, pattern = ".R", recursive = TRUE))
    #         source(file.path(path, l), local=FALSE)$value
    #     if (!Found_Mod_Funcs(id)) {
    #         return(NULL)
    #     }
    # }
    # 
    # 
    options(shiny.fullstacktrace = verbose)


    ui <- fluidPage(
        tagList(
            nav_ui(id),
            uiOutput("debugInfos_ui")
        )
    )


    #----------------------------------------------------------------------
    server <- function(input, output) {
        dataOut <- reactiveVal()

        dataIn <- Load_Dataset_server("exemple")

        output$debugInfos_ui <- renderUI({
            req(verbose)
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

        observeEvent(req(dataIn()), {
            dataOut(nav_server(
                id = id,
                verbose = verbose,
                dataIn = reactive({dataIn()}),
                tl.layout = tl.layout
                ))

            Debug_Infos_server(
                id = "debug_infos",
                title = "Infos from shiny app",
                rv.dataIn = reactive({dataIn()}),
                dataOut = reactive({dataOut()$dataOut()$value})
            )
        })
    }

    shinyApp(ui, server)
}
