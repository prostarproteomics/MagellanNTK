#' @title mod_open_workflow_ui and mod_open_workflow_server
#' 
#' @description  A shiny Module.
#' 
#' @param id xxx
#' 
#' @name mod_open_workflow
#' 
#' @examplesIf interactive()
#' shiny::runApp(open_workflow())
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
    h3(style="color: blue;", '-- Default open workflow module --'),
    uiOutput(ns('dirInput_UI')),
    uiOutput(ns('chooseWF_UI')),
    actionButton(ns('load_btn'), 'Load'),
    #infos_workflow_ui(ns("infos")),
    #tags$h3('Files'),
    #dataTableOutput(ns('files'))
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
    
    rv.wf <- reactiveValues(
      path = path.expand('~'),
      dataOut = NULL
    )

    session$onSessionEnded(function(){
      stopApp()
    })
    
    
    output$dirInput_UI <- renderUI({
      directoryInput(ns('directory'), 
        label = 'selected directory', 
        value = rv.wf$path)
      
    })
    
    observeEvent(input$directory, ignoreNULL = TRUE,
      handlerExpr = {
        if (input$directory > 0) {
          # condition prevents handler execution on initial app launch
          rv.wf$path = choose.dir(default = readDirectoryInput(session, 'directory'),
            caption="Choose a directory...")
          updateDirectoryInput(session, 'directory', value = rv.wf$path)
        }
      }
    )
    
    output$directory = renderText({
      readDirectoryInput(session, 'directory')
    })
    
    
    
    output$chooseWF_UI <- renderUI({
      req(rv.wf$path)
      ll.files <- list.files(file.path(rv.wf$path, 'R'), full.names = FALSE)
      
      ll <- unlist(lapply(ll.files, function(x)
        if (is.substr(basename(rv.wf$path), x))
          x
        ))
      

      if (length(ll) > 0){
        radioButtons(ns('chooseWF'), 'Choose workflow',
        choices = gsub('.R', '', ll)
        )
      } else {
        
      }
      
    })
    
    ## -- Open a MSnset File --------------------------------------------
    observeEvent(input$load_btn, ignoreInit = TRUE, {
      rv.wf$path

      rv.wf$dataOut$path <- rv.wf$path
      rv.wf$dataOut$wf_name <- input$chooseWF
      
      # Load customizable functions if config.txt file exists
      rv.wf$dataOut$funcs  <- readCustomizableFuncs(rv.wf$path)

    })
    
    # output$files = renderDataTable({
    #   files = list.files(rv.wf$path, full.names = TRUE)
    #   data.frame(name = basename(files), file.info(files))
    # })
    
    reactive({rv.wf$dataOut})
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
      open_workflow_ui("wf")
    )
  )
  
  server <- function(input, output, session) {
    rv <- reactiveValues(
      obj = NULL,
      result = NULL
    )
    
    
    rv$result <- open_workflow_server("wf")
    
    observeEvent(req(rv$result()), {
      print(paste('path = ', rv$result()$path))
      print(paste('wf_name = ', rv$result()$wf_name))
      print(paste('funcs = ', rv$result()$funcs))
    })
    
  }
  
  app <- shiny::shinyApp(ui, server)
}


