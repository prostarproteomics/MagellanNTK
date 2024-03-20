#' @title   mod_open_dataset_ui and mod_open_dataset_server
#' 
#' @description  A shiny Module.
#' 
#' @param id xxx
#' 
#' @name mod_open_workflow
#'
#' @keywords internal
#' 
#' @examples 
#' if (interactive()){
#' shiny::runApp(open_workflow())
#' }
#' 
#' @return A list
#' 
NULL




#' @export 
#' @rdname mod_open_workflow
#' @import shiny
#' 
open_workflow_ui <- function(id){
  ns <- NS(id)
  tagList(
    h3(style="color: blue;", '-- Default open dataset module --'),
    directoryInput(ns('directory'), label = 'selected directory', value = '~'),
    actionButton(ns('load_btn'), 'Load'),
    infos_dataset_ui(ns("infos"))
  )
}


#' @rdname mod_open_workflow
#' 
#' @export
#' @importFrom shinyjs info 
#' @importFrom shiny moduleServer reactiveValues observeEvent
#' 
open_workflow_server <- function(id){
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    rv.open <- reactiveValues(
      dataRead = NULL,
      dataOut = NULL
    )
    
    
    
    session$onSessionEnded(function(){
      stopApp()
    })
    
    observeEvent(
      ignoreNULL = TRUE,
      eventExpr = {
        input$directory
      },
      handlerExpr = {
        if (input$directory > 0) {
          # condition prevents handler execution on initial app launch
          path = choose.dir(default = readDirectoryInput(session, 'directory'),
            caption="Choose a directory...")
          updateDirectoryInput(session, 'directory', value = path)
        }
      }
    )
    
    output$directory = renderText({
      readDirectoryInput(session, 'directory')
    })
    
    
    
    ## -- Open a MSnset File --------------------------------------------
    observeEvent(input$load_btn, ignoreInit = TRUE, {
      input$file
      rv.open$dataRead <- NULL
     
        #  rv.open$dataOut <- list(original = rv.open$dataRead)
    })
    
    infos_dataset_server("infos", 
      obj = reactive({rv.open$dataOut}))
    
    reactive({rv.open$dataOut})
  })
  
}






#' @rdname mod_open_workflow
#' 
#' @export
#' @importFrom shiny fluidPage tagList textOutput reactiveValues observeEvent
#' shinyApp
#' 
open_workflow <- function(){
  ui <- fluidPage(
    tagList(
      open_workflow_ui("wf"),
      textOutput('res')
    )
  )
  
  server <- function(input, output, session) {
    rv <- reactiveValues(
      obj = NULL,
      result = NULL
    )
    
    
    rv$result <- open_workflow_server("wf")
    
    observeEvent(req(rv$result()), {
      rv$obj <- rv$result()
      print(rv$obj)
    })
    
  }
  
  app <- shiny::shinyApp(ui, server)
}


