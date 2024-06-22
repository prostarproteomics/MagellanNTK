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
    h3(style="color: blue;", 'Open workflow (default)'),
    # div(
    #   width = '600px',
    #   uiOutput(ns('dirInput_UI'))
    #   ),
    fluidRow(
      column(width = 3, uiOutput(ns('choosePackage_UI'))),
      column(width = 3, uiOutput(ns('chooseWF1_UI'))),
      column(width = 3, uiOutput(ns('chooseProcess_UI')))
        ),
    uiOutput(ns('wf_preview_ui')),
    actionButton(ns('load_btn'), 'Load'),
    uiOutput(ns('infos_wf_UI'))
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
    
    
    output$wf_preview_ui <- renderUI({
      p('Preview')
    })
    
    FindPkg2MagellanNTK <- reactive({
      x <- data(package = .packages(all.available = TRUE))$results
      rnames <- rownames(installed.packages())
      ll <- lapply(rnames, function(x){
        dir.exists(system.file('workflow', package = as.character(x)))
      })
      
      rnames[which(ll == TRUE)]
      
    })
    
    
    output$choosePackage_UI <- renderUI({
      selectInput(ns('choosePkg'), 'Package',
        choices = FindPkg2MagellanNTK())
    })


    
    Find_WF <- reactive({
      req(input$choosePkg)
    
    path <- system.file('workflow', package = as.character(input$choosePkg))
    ll.workflows <- list.dirs(path, full.names = FALSE, recursive = FALSE)
    
    ll.workflows
    })
    
    
    output$chooseWF1_UI <- renderUI({
      req(Find_WF())
        selectInput(ns('chooseWF1'), 'Choose workflow',
        choices = Find_WF()
        )
    })
    
    
    
    output$chooseProcess_UI <- renderUI({
      req(input$chooseWF1)

      rv.wf$path <- system.file(file.path('workflow', input$chooseWF1), 
        package = as.character(input$choosePkg))
      
      tmp <- normalizePath(file.path(rv.wf$path, 'R', fsep = file.sep()))
      ll.files <- list.files(tmp, full.names = FALSE)
  
      ll <- unlist(lapply(ll.files, function(x)
        if (is.substr(basename(input$chooseWF1), x))
          x
      ))
      
      
      # Remove Description and Save process
      ll <- ll[-c(grep('_Description', ll), grep('_Save', ll))]
      
      
        selectInput(ns('chooseProcess'), 'Choose process',
          choices = gsub('.R', '', ll)
        )
    })
    
    
    
    ## -- Open a MSnset File --------------------------------------------
    observeEvent(input$load_btn, ignoreInit = TRUE, {
      rv.wf$path

      rv.wf$dataOut$path <- rv.wf$path
      rv.wf$dataOut$wf_name <- input$chooseProcess
      # Load customizable functions if config.txt file exists
      rv.wf$dataOut$funcs  <- readConfigFile(rv.wf$path)$funcs

    })
    
    # output$files = renderDataTable({
    #   files = list.files(rv.wf$path, full.names = TRUE)
    #   data.frame(name = basename(files), file.info(files))
    # })
    
    
    output$infos_wf_UI <- renderUI({
      req(rv.wf$dataOut$wf_name)
      print(paste0('Worflow ', rv.wf$dataOut$wf_name, ' loaded'))
    })
    
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


