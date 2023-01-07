#' @title   format_DT_ui and format_DT_server
#' 
#' @description  A shiny Module.
#' 
#' @name format_DT
#'
#' @keywords internal
#' @example examples/test_format_DT.R
#' 
NULL



#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname format_DT
#' @export
#'  
#' @importFrom shiny NS tagList 
#' @importFrom DT dataTableOutput
#' 
mod_shinyTree_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyTree(ns("tree123"))
  )
}

# Module Server

#' @rdname format_DT
#' 
#' @param input internal
#' @param output internal
#' @param session internal
#' @param withBtns xxx
#' @param showRownames xxxx
#' @param dom xxx
#' @param style A list of four items:
#' * cols: a vector of colnames of columns to show,
#' * vals: a vector of colnames of columns that contain values,
#' * unique: unique(conds),
#' * pal: RColorBrewer::brewer.pal(3, "Dark2")[seq_len(2)]
#' @param filename xxx
#' 
#' @export
#' 
#' @keywords internal
#' 
#' @import DT
#' @importFrom htmlwidgets JS    
#' 
mod_shinyTree_server <- function(id, path){
  
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    rv <- reactiveValues(
      selected = NULL
    )
    
    
    BuildTree <- reactive({
      req(path())
      
      lst.wf <- list.files(path())
      
      tree <- list()
      for (i in lst.wf){
        tree[[i]] <- list()
        files <- list.files(file.path(path(), i, 'R'), pattern = i)
        files <- gsub('.R', '', files)
        files <- files[-which(files==i)]
        for (j in files){
          tree[[i]][[j]]=''
        }
      }
      
      tree
      
      
      
      
    })
    
    
    output$tree123 <- renderTree({
      BuildTree()
    })
    
    
    observe({
      input$tree123
      rv$selected <- get_selected(input$tree123, format = "classid")
    })
    
    return(reactive({rv$selected}))
    
  })
  
}
