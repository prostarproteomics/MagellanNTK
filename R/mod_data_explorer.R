#' data_explorer UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_data_explorer_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    
    mod_bsmodal_ui('exemple')
  )
}
    
#' data_explorer Server Function
#'
#' @noRd 
mod_data_explorer_server <- function(input, output, session, dataIn){
  ns <- session$ns
  
  
  output$chooseDataset_UI <- renderUI({
    if (length(names(dataIn())) == 0){
      choices <- list(' '=character(0))
    } else {
      choices <- names(dataIn())
    }
    selectInput(ns('chooseDataset'), 'Dataset',
                choices = choices,
                selected = names(dataIn())[length(dataIn())],
                width=150)
  })
  
  
  mod_all_plots_server('plots',
                       dataIn = reactive({dataIn()}),
                       indice = reactive({which(names(dataIn()) == input$chooseDataset)})
  )
 
}
    
## To be copied in the UI
# mod_data_explorer_ui("data_explorer_ui_1")
    
## To be copied in the server
# callModule(mod_data_explorer_server, "data_explorer_ui_1")
 
