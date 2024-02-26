# Module UI
  
#' @title   choose_pipeline_ui and mod_choose_pipeline_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param dataType xxx
#' @param package xxx
#' 
#' @name mod_choose_pipeline
#'
NULL


#' @export 
#' @importFrom shiny NS tagList
#' @rdname mod_choose_pipeline 
#' @import sos
#' 
choose_pipeline_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    h3('mod_choose_pipeline'),
    # div(
    #   style="display:inline-block; vertical-align: middle; padding-right: 20px;",
    #   selectInput(ns("dataType"), 'Data type', 
    #               choices = c('None'='None', 
    #                           'protein'='protein', 
    #                           'peptide'='peptide'), 
    #               width='150px')
    # ),
    div(
      style="display:inline-block; vertical-align: middle; padding-right: 20px;",
      uiOutput(ns("selectWidgetPipeline"))
    ),
    div(
      style="display:inline-block; vertical-align: middle; padding-right: 20px;",
      actionButton(ns('selectPipeline_btn'), 'load pipeline')
    ),
      uiOutput(ns('describePipeline'))
  )
}
    
   
#' @rdname mod_choose_pipeline
#' @export
#' @keywords internal
#' 
#' 
    
choose_pipeline_server <- function(id, dataType = NULL, package = NULL){
  
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    rv.pipe <- reactiveValues(
      pipeline = NULL
    )
    
    output$selectWidgetPipeline <- renderUI({

      req(!is.null(dataType), dataType != 'None')
      #if (dataType() == 'None') return(NULL)
      #print('inside selectWidgetPipeline')
      #print(paste0('dataType received = ', dataType()))
      library(package, character.only = TRUE)
      selectInput(ns("pipelineChoice"),
                     "Choose the pipeline",
                     choices = c('None',names(Pipelines()[grep(dataType, Pipelines())])), 
                     width='150px'
      )
    })
    
    observeEvent(input$selectPipeline_btn, {
      rv.pipe$pipeline <- input$pipelineChoice
    #  shinyjs::toggleState('loadPipeline', condition = rv.pipe$pipeline != 'None')
    })
    
    output$describePipeline <- renderUI({
      req(input$pipelineChoice)
      includeMarkdown(system.file('md', paste0(input$pipelineChoice, '.md'), package=package))
    })
    
    reactive({rv.pipe$pipeline})
  })
  
}
    
# Example
#
library(shiny)


ui <- fluidPage(
  choose_pipeline_ui(id = "tbl")
)

server <- function(input, output) {
  
  choose_pipeline_server(id = "tbl")

}

shinyApp(ui = ui, server = server)

