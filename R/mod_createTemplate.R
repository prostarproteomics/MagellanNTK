#' @title Create process template code
#'
#' @description  This function creates the source code of an empty module 
#' (i.e. a module without any widgets). This module contains the minimal
#' skeleton to work. The developer can then insert its own code for widgets
#' and data processing functions.
#' 
#' The 'Description' step is generic and creates a *.md file to be filled by
#' th developer.
#' 
#' @name create_template
#' 
#' @param id xxx
#' @param config xxx
#' @param path xxx
#'
#' @examples
#' NULL
#' 
#' @import shinyFiles
#' 
#' @author Samuel Wieczorek
NULL


#' @export
#' @import shinyFiles
#' @rdname create_template
createTemplate_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    h3(strong('Create templates'), align ='center'),
    uiOutput(ns('path_ui')),
    #uiOutput(ns('guess_ui')),
    selectInput(ns('mode'), 'Template to create', 
                choices = c('pipeline', 'process', 'module'), 
                width='100px'),
    
    
    wellPanel(
      fluidRow(
      column(width = 4, 
             shinyjs::disabled(
               textInput(ns('parent'), 'Parent pipeline', width='100px')
             )),
      column(width = 4, textInput(ns('name'), 'Name', width='100px'))
    )
    ),
    shinyjs::hidden(
      wellPanel(id = ns('wp_dyn_steps'),
                dyn_widgets_ui(ns('dyn_steps')))
      ),
    actionButton(ns('createTemplate'), 'Create', class = 'btn-primary')
    #uiOutput(ns('filesCreated'))
  )
}


#' @export
#' @rdname create_template
createTemplate_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    rv <- reactiveValues(
      steps = NULL,
      files = NULL
    )
    path <- reactiveVal(NULL)
     
    guessWorkflow <- reactive({
      req(path())
      lst.files <- list.files(file.path(path()(), 'R'))
    })
    #-------------------------------------------------------
    output$path_ui <- renderUI({
      path(chooseDir_server('chooseDir'))
      chooseDir_ui(ns('chooseDir'))
    })
    
    # List files in the R directory. It is supposed to contain R files
    # for the code of processes, pieplines, etc...
    output$guess_ui <- renderUI({
     req(path()())
    guessWorkflow()
      lapply(guessWorkflow(), function(i)
        p(i))
    })
    
    observe({
      input$mode
      shinyjs::toggleState('parent', condition = input$mode=='process')
      shinyjs::toggle('wp_dyn_steps', condition = input$mode!='module')
    })
    
    res <- reactiveValues(dataOut = list())
    res$dataOut <- dyn_widgets_server('dyn_steps')
    
  
    ################################################################### 
    
    build_fullname <- reactive({
      paste0(input$parent, '_', input$name)
    })
    
    observeEvent(input$createTemplate, {
      miniConfig <- list(fullname = paste0(input$parent, '_', input$name),
                         mode = input$mode,
                         steps = res$dataOut()$inputs,
                         mandatory = res$dataOut()$mandatory
                         )
      
      if (input$mode == 'module')
        createExtraModule(name = input$name, path = path()())
      else {
        createModuleTemplate(miniConfig, path = path()())
        createExtraFunctions(path()())
      }
    })
    
    # output$filesCreated <- renderUI({
    #   req(rv$files)
    #   tagList(
    #     p('Template files created;'),
    #     lapply(rv$files, function(i) p(i))
    #   )
    # })
})
}


#' @export
#' @rdname create_template
createTemplate <- function(){
  ui <- createTemplate_ui('test')
  
  server <- function(input, output, session) 
    createTemplate_server('test')
  
  shinyApp(ui, server)
  
}
