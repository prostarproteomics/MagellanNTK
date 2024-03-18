#' @title   mod_open_dataset_ui and mod_open_dataset_server
#' 
#' @description  A shiny Module.
#' 
#' @param id xxx
#' 
#' @name mod_open_dataset
#'
#' @keywords internal
#' 
#' @examples 
#' if (interactive()){
#' shiny::runApp(open_dataset())
#' }
#' 
NULL




#' @export 
#' @rdname mod_open_dataset
#' @import shiny
#' 
open_dataset_ui <- function(id){
  ns <- NS(id)
  tagList(
    h3(style="color: blue;", '-- Default open dataset module --'),
    fileInput(ns("file"), "Open file", multiple = FALSE),
    actionButton(ns('load_btn'), 'Load file')
  )
}


#' @rdname mod_open_dataset
#' 
#' @export
#' @importFrom shinyjs info 
#' @importFrom shiny moduleServer reactiveValues observeEvent
#' 
open_dataset_server <- function(id){
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    rv.open <- reactiveValues(
      dataRead = NULL,
      dataOut = NULL
    )
    
    ## -- Open a MSnset File --------------------------------------------
    observeEvent(input$load_btn, ignoreInit = TRUE, {
      input$file
      
      rv.open$dataRead <- tryCatch({
          readRDS(input$file$datapath)
        },
        warning = function(w) {
          shinyjs::info(conditionMessage(w))
          return(NULL)
        },
        error = function(e) {
          shinyjs::info(conditionMessage(e))
          return(NULL)
        }
      )
      
      if (is.null(rv.open$dataRead)){
        rv.open$dataRead <- tryCatch({
          load(file = input$file$datapath)
          name <- unlist(strsplit(input$file$name, split='.', fixed = TRUE))[1]
          rv.open$dataRead <- get(name)
        },
          warning = function(w) {
            shinyjs::info(conditionMessage(w))
            return(NULL)
          },
          error = function(e) {
            shinyjs::info(conditionMessage(e))
            return(NULL)
          }
        )
      }
      
      
      rv.open$dataOut <- rv.open$dataRead
      
    })
    
    reactive({rv.open$dataOut})
  })
  
}






#' @rdname mod_open_dataset
#' 
#' @export
#' @importFrom shiny fluidPage tagList textOutput reactiveValues observeEvent
#' shinyApp
#' 
open_dataset <- function(){
ui <- fluidPage(
  tagList(
    open_dataset_ui("qf_file"),
    textOutput('res')
  )
)

server <- function(input, output, session) {
  rv <- reactiveValues(
    obj = NULL,
    result = NULL
  )
  
  
  rv$result <- open_dataset_server("qf_file")
  
  observeEvent(req(rv$result()), {
    rv$obj <- rv$result()
    print(rv$obj)
  })
  
}

app <- shiny::shinyApp(ui, server)
}


