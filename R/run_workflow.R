#' @title Shiny example module `Process`
#'
#' @description
#' This module contains the configuration information for the corresponding 
#' pipeline. It is called by the nav_pipeline module of the package MagellanNTK
#' This documentation is for developers who want to create their own 
#' pipelines nor processes to be managed with `MagellanNTK`.
#'
#' @param id xxx
#' @param path xxx
#' @param dataIn xxx
#' @param tl.layout Additional parameters for nav
#' @param usermod Available values are 'superdev', 'dev', 'superuser', 'user'
#' @param ... xxx
#'
#' @name workflow
#' 
#' @example inst/extdata/funcs_examples/example_run_workflow.R
#'
#' @author Samuel Wieczorek
#'
#' @importFrom utils data
#' @import shiny
#'
#' @return NA
#' 
#' @examplesIf interactive()
#' data(lldata)
#' path <- system.file('workflow/PipelineDemo', package = 'MagellanNTK')
#' 
#' # Nothing happens when dataIn is NULL
#' shiny::runApp(workflowApp("PipelineDemo_Process1", path, dataIn = NULL))
#' 
#' shiny::runApp(workflowApp("PipelineDemo_Process1", path, dataIn = lldata))
#' 
#' shiny::runApp(workflowApp("PipelineDemo_Process1", dataIn = data.frame())
#' 
#' shiny::runApp(workflowApp("PipelineDemo", path, dataIn = lldata))
#' 
#' shiny::runApp(workflowApp("PipelineB", path, tl.layout = c("v", "h")))
#' 
#' shiny::runApp(workflowApp("PipelineDemo", path, dataIn = sub_R25, 
#' tl.layout = c("v", "h")))
#'
#'
NULL


#' @export
#' @rdname workflow
#' 
workflow_ui <- function(id){
  ns <- NS(id)
  tagList(
    nav_ui(ns(id)),
    uiOutput(ns("debugInfos_ui"))
  )
  
}



#' @export
#' @rdname workflow
#' 
workflow_server <- function(id,
  path = NULL,
  dataIn = reactive({NULL}),
  tl.layout = NULL,
  usermod = "dev",
  verbose = FALSE){
  
  
  source_shinyApp_files()
  
 if(is.null(path)){
   message("'path' is not correctly configured. Abort...")
   return(NULL)
 } 
  
  
  source_wf_files(path)
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    dataOut <- reactiveVal()
    
    output$debugInfos_ui <- renderUI({
      req(usermod == 'dev')
      Debug_Infos_server(id = 'debug_infos',
        title = 'Infos from shiny app',
        rv.dataIn = reactive({dataIn}),
        dataOut = reactive({rv$dataOut$dataOut()})
      )
      Debug_Infos_ui("debug_infos")
    })
    
    output$save_dataset_ui <- renderUI({
      req(c(dataOut(), dataOut()$dataOut()$value))
      
      dl_ui(ns("saveDataset"))
      dl_server(
        id = "saveDataset",
        dataIn = reactive({dataOut()$dataOut()$value})
      )
    })
    
    
    observeEvent(path, {
      session$userData$workflow.path <- path
      
      session$userData$funcs <- readConfigFile(path)$funcs
      
    })
    
    
    observeEvent(dataIn, {
      
      dataOut(
        nav_server(id = id,
          dataIn = reactive({dataIn}),
          tl.layout = tl.layout, 
          verbose = verbose,
          usermod = usermod
        )
      )
    })
    
    return(reactive({dataOut()}))
    
    })
}
  
  
  

#' @rdname workflow
#' @export
workflowApp <- function(id,
  path = NULL,
  dataIn = NULL,
  tl.layout = NULL,
  usermod = 'dev',
  verbose = FALSE) {

  ui <- workflow_ui(id)
  server <- function(input, output, session) {
    
    
      res <- workflow_server(id, 
        path = path,
        dataIn = dataIn)

      observeEvent(req(res()$dataOut()$trigger), {
        print(res()$dataOut()$value)
      })
    }

  app <-shinyApp(ui, server)
}

