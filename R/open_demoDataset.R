#' @title   mod_open_demo_dataset_ui and mod_open_demo_dataset_server
#' 
#' @description  A shiny Module.
#' 
#' @param id xxx
#' 
#' @name generic_mod_open_demo_dataset
#'
#' @keywords internal
#' 
NULL




#' @export 
#' @rdname generic_mod_open_demo_dataset
#' @importFrom shiny NS tagList 
#' @import shinyjs
#' 
open_demoDataset_ui <- function(id){
  ns <- NS(id)
  tagList(
    h3(style="color: blue;", '-- Default demo dataset module --'),
      shinyjs::useShinyjs(),
      tagList(
        uiOutput(ns("choosePkg")),
        uiOutput(ns("chooseDemoDataset")),
        uiOutput(ns("linktoDemoPdf")),
        shinyjs::disabled(
          actionButton(ns('load_dataset_btn'), 'Load dataset', 
            class= GlobalSettings$actionBtnClass))
      )
    )
}


#' @rdname generic_mod_open_demo_dataset
#' 
#' @export
#' @importFrom BiocGenerics get
#' @importFrom utils data
#' @importFrom shinyjs info
#' 
open_demoDataset_server <- function(id){
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    rv.openDemo <- reactiveValues(
      dataRead = NULL,
      dataOut = NULL
    )
    
    
    
    
    output$choosePkg <- renderUI({
      
      selectInput(ns("pkg"),
        "Choose package",
        choices = rownames(installed.packages()),
        selected = character(0),
        width='200px')
    })
    
    ## function for demo mode
    output$chooseDemoDataset <- renderUI({
      req(input$pkg)
      pkgs.require(input$pkg)
      
      selectInput(ns("demoDataset"),
        "Demo dataset",
        choices = c('None', utils::data(package=input$pkg)$results[,"Item"]),
        selected = character(0),
        width='200px')
    })
    
    
    
    observeEvent(req(input$demoDataset != 'None'), {
      nSteps <- 1
      withProgress(message = '',detail = '', value = 0, {
        incProgress(1/nSteps, detail = 'Loading dataset')
        utils::data(list = input$demoDataset, package = input$pkg)
        rv.openDemo$dataRead <- BiocGenerics::get(input$demoDataset)
        # if (!inherits(rv.openDemo$dataRead, "QFeatures")) {
        #   shinyjs::info("Warning : this file is not a QFeatures file ! 
        #               Please choose another one.")
        #   return(NULL)
        # }
        shinyjs::toggleState('load_dataset_btn', condition = !is.null(rv.openDemo$dataRead))
      }) # End withProgress
    }) # End observeEvent
    
    
    observeEvent(input$load_dataset_btn, {
      rv.openDemo$dataOut <- rv.openDemo$dataRead
    })
    
    output$linktoDemoPdf <- renderUI({
      req(input$demoDataset)
      
    })
    
    reactive({rv.openDemo$dataOut })
  })
  
  
}




###################################################################
##                                                               ##
##                                                               ##
###################################################################

library(shiny)

ui <- open_demoDataset_ui("demo")


server <- function(input, output, session) {
  rv <- reactiveValues(
    obj = NULL
  )
  
  rv$obj <- open_demoDataset_server("demo")

}

shinyApp(ui = ui, server = server)
