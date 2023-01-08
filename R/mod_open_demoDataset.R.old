# Module UI

#' @title   mod_open_demo_dataset_ui and mod_open_demo_dataset_server
#' 
#' @description  A shiny Module.
#' 
#' @name mod_open_demo_dataset
#'
#' @keywords internal
#' 
#' @examples xxxx
#' 
NULL



#' @param id xxx
#' @export 
#' @rdname mod_open_demo_dataset
#' @importFrom shiny NS tagList 
#' @import shinyjs
#' 
mod_open_demoDataset_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    div(
      div(
        style="display:inline-block; vertical-align: middle; padding-right: 20px;",
        div(id='div_demoDataset',
            tagList(
              uiOutput(ns("chooseDemoDataset")),
              uiOutput(ns("linktoDemoPdf"))
            )
        )
      ),
      div(
        style="display:inline-block; vertical-align: middle; padding-right: 20px;",
        shinyjs::disabled(
          actionButton(
            ns('load_dataset_btn'), 
            'Load dataset', 
            class=actionBtnClass)
        )
      )
    )
  )
}

# Module Server

#' @rdname mod_open_demo_dataset
#' 
#' @export
#' 
#' @param id xxx
#' 
#' @keywords internal
#' 
#' @import DAPARdata2
#' @importFrom BiocGenerics get
#' @importFrom utils data
#' @importFrom BiocManager install
#' @importFrom shinyjs info
#' @import QFeatures
#' 
mod_open_demoDataset_server <- function(id, path){
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    
    rv.openDemo <- reactiveValues(
      datasets = NULL,
      dataRead = NULL,
      dataOut = NULL
    )
    
    
    
    GetDatasets <- reactive({
      req(path())
      
      list.files(file.path(path(), 'data'))
      
    })
    
    ### function for demo mode
    output$chooseDemoDataset <- renderUI({
      
      selectInput(ns("demoDataset"),
                  "Demo dataset",
                  choices = c('None', GetDatasets()),
                  selected = character(0),
                  width='200px')
    })
    
    observeEvent(req(input$demoDataset != 'None'), {
      
      
      rv.openDemo$dataRead <- load_dataset(file.path(path(), 'data', input$demoDataset))
      shinyjs::toggleState('load_dataset_btn', condition = !is.null(rv.openDemo$dataRead))
      })

    
    observeEvent(input$load_dataset_btn, {
      rv.openDemo$dataOut <- rv.openDemo$dataRead
    })
    
    
    reactive({rv.openDemo$dataOut })
  })
  
}
