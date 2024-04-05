# Module UI

#' @title   mod_open_demo_dataset_ui and mod_open_demo_dataset_server
#' 
#' @description  A shiny Module.
#'
#' @param id shiny id

#' @param pipeline.def xxx
#' 
#' @return An object of class [`xxxx`]
#' 
#' @rdname mod_open_demo_dataset
#'
#' @keywords internal
#' 
#' @export 
#' 
#' @importFrom shiny NS tagList 
#' @import shinyjs
#' 
dataManager_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    selectInput(ns("dmUI"), "Choose one:",
                 choices = c('convert' = 'convert',
                             'open' = 'open',
                             'demo' = 'demo'),
                width = '150px'),
    
    uiOutput(ns('open_demo_dataset_UI')),
    uiOutput(ns('open_convert_dataset_UI')),
    uiOutput(ns('open_dataset_UI')),
    hr(),
    uiOutput(ns("infos_dataset_UI"))
    )
}

# Module Server

#' @rdname mod_open_demo_dataset
#' 
#' @export
#' 
#' @keywords internal
#' 
#' @import DaparToolshedData
#' @importFrom BiocGenerics get
#' @importFrom utils data
#' @importFrom BiocManager install
#' @importFrom shinyjs info
#' @import QFeatures
#' @import omXplore
#' 
dataManager_server <- function(id, funcs){
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    
    rv.dm <- reactiveValues(
      dataOut = NULL
    )

  
   call.func(
     fname = paste0(funcs$infos_dataset, '_server'),
     args = list(id = 'infos',
                 obj = reactive({rv.dm$dataOut}))
   )
   
   output$infos_dataset_UI <- renderUI({
     req(funcs)
     req(rv.dm$dataOut)
     call.func(
       fname = paste0(funcs$infos_dataset, '_ui'),
       args = list(id = ns('infos')))
   })
   
   
    #
    # Code for convert tool
    #
    rv.dm$result_convert_dataset <- call.func(
      fname = paste0(funcs$convert, '_server'),
      args = list(id = 'Convert'))
    
    output$open_convert_dataset_UI <- renderUI({
      req(funcs)
      req(input$dmUI == 'convert')
      call.func(
        fname = paste0(funcs$convert, '_ui'),
        args = list(id = ns('Convert')))
    })
    
    observeEvent(req(rv.dm$result_convert_dataset()),{
      rv.dm$dataOut <- rv.dm$result_convert_dataset()
       })
    
    
    #
    # Code for open demo dataset
    #
    rv.dm$result_demo_dataset <- call.func(
      fname = paste0(funcs$open_demoDataset, '_server'),
      args = list(id = 'open_demo_dataset'))
    
    output$open_demo_dataset_UI <- renderUI({
      req(funcs)
      req(input$dmUI == 'demo')
      call.func(
        fname = paste0(funcs$open_demoDataset, '_ui'),
        args = list(id = ns('open_demo_dataset')))
    })
    
    observeEvent(req(rv.dm$result_demo_dataset()),{
      rv.dm$dataOut <- rv.dm$result_demo_dataset()
      })
    
    #
    # Code for open dataset
    #
    rv.dm$result_open_dataset <- call.func(
      fname = paste0(funcs$open_dataset, '_server'),
      args = list(id = 'open_dataset'))
    
    output$open_dataset_UI <- renderUI({
      req(funcs)
      req(input$dmUI == 'open')
      call.func(fname = paste0(funcs$open_dataset, '_ui'),
                args = list(id = ns('open_dataset')))
    })
    
    observeEvent(req(rv.dm$result_open_dataset()),{
      rv.dm$dataOut <- rv.dm$result_open_dataset()
      })
    
    reactive({rv.dm$dataOut })
  })
  
}



library(shiny)

ui <- fluidPage(dataManager_ui("demo"))


server <- function(input, output, session) {
  
  funcs <- list(convert_dataset = "DaparToolshed::convert",
                open_dataset = "DaparToolshed::open_dataset",
                open_demoDataset = "DaparToolshed::open_demoDataset",
                view_dataset = "omXplore::view_dataset",
                infos_dataset = "DaparToolshed::infos_dataset"
  )
  
  
  rv <- reactiveValues(
    obj = NULL
  )
  
  rv$obj <- dataManager_server("demo", funcs)
  
  observeEvent(rv$obj(), {
    print(rv$obj())
  })
  
}

shinyApp(ui = ui, server = server)
