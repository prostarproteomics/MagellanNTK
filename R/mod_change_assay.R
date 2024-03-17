
#' @title xxx
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#' 
#' @examples
#' if(interactive()){
#' mod_change_assay(ll.se, indice)
#' }
#' 
#'
NULL



#' @importFrom shiny NS tagList absolutePanel uiOutput
mod_change_assay_ui <- function(id){
  ns <- NS(id)

    absolutePanel(
     # id  = "#AbsolutePanel",
      #class = "panel panel-default",
      style= "text-align: center; z-index: 10000;",
      top = 0, 
      right = 50, 
      width = "250px",
      height = "50px",
      draggable = FALSE,
      fixed = FALSE,
      cursor = "default",
      uiOutput(ns("datasetAbsPanel"))
  )
}
    
#' change_assay Server Function
#' @importFrom shiny moduleServer reactiveValues observe req renderUI
#' tagList req div selectInput observeEvent
#'
#' @noRd 
mod_change_assay_server <- function(id, 
  ll.se, 
  indice){
  
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    rv.change <- reactiveValues(
      out = NULL
    )
    
    
    observe({
      req(indice())
      rv.change$out <- indice()
    })
    
    
    output$datasetAbsPanel <- renderUI({
      req(ll.se())
      req(indice())
      
      tagList(
        div(
          div(
            style="display:inline-block; vertical-align: middle; margin:0px",
            p('Current assay', style='color: white')
          ),
          div(
            style="display:inline-block; vertical-align: middle; margin:0px",
            selectInput(ns('currentDataset'), '',
                        choices = ll.se(),
                        selected = ll.se()[indice()],
                        width='150px')
          )
        )
      )
      
    })
    
    
    
    ## manual change of current dataset
    observeEvent(input$currentDataset,{
      print('!!!!! Manual change of current dataset')
      
      n <- which(ll.se() == input$currentDataset)
      if (length(n)==0){
        rv.change$out <- 1
      } else {
        rv.change$out <- n
      }
      
    })
    
    
    return(reactive({rv.change$out}))
    
  })
  
  
}
    
#' @rdname mod_open_dataset
#' 
#' @export
#' @importFrom shiny fluidPage tagList textOutput reactiveValues observeEvent
#' shinyApp
#' 
mod_change_assay <- function(ll.se, indice){
  ui <- fluidPage(
    mod_change_assay_ui("chassay")
  )
  
  server <- function(input, output, session) {
    rv$result <- mod_change_assay_server("chassay",
      ll.se = ll.se,
      indice = indice)
  }
  
  app <- shiny::shinyApp(ui, server)
}