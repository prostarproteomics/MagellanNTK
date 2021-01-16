utils::data(Exp1_R25_prot, package='DAPARdata2')
#' @title 
#' Pipeline launch class
#' 
#' @description
#' This class is a workaround to make work shinyjs::disable/enable functions
#' within renderUI:
#' https://github.com/daattali/shinyjs/issues/74
#' https://github.com/daattali/shinyjs/issues/25
#' Its goal is to activate automatically the pipeline when a 
#' dataset is loaded
#' 
#' @export
#' 
Magellan <- R6::R6Class(
  public = list(
    id = NULL,
    ns = NULL,
    Pipeline = NULL,
    rv = "<reactiveValues>",
    
    initialize = function(id, name){
      self$id <- id
      self$ns <- NS(id)
      self$rv <- reactiveValues(
        dataIn = NULL
      )
      
       obj <- base::get(name)
      # self$Pipeline <- do.call(obj$new, list(self$ns('App')))
      self$Pipeline <- Example_ProcessA$new(self$ns('App'))
    },
    
    ui = function(){
      req(self$Pipeline)
      self$Pipeline$ui()
    },
    
    server = function(dataIn = reactive({NULL})) {
      if(is.null(self$Pipeline)){
        stop('Empty Pipeline. Exiting...')
      }
      self$rv$dataOut <- self$Pipeline$server(dataIn = reactive({dataIn()}))
      
      moduleServer(self$id, function(input, output, session) {
        reactive({self$rv$dataOut})
      })
    }
  )
)


