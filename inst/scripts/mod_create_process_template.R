
library(shiny)
library(shinyFiles)

create_process_template_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3('Create process template'),
    uiOutput(ns('path_ui')),
    uiOutput(ns('mode_ui')),
    uiOutput(ns('parent_ui')),
    uiOutput(ns('name_ui')),
    uiOutput(ns('steps_ui')),
    actionButton(ns('createTemplate'), 'Create template'),
    uiOutput(ns('filesCreated'))
  )
}


create_process_template_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    rv <- reactiveValues(
      steps = NULL
    )
    path <- reactiveVal(NULL)
    files <- reactiveVal(NULL)
    
    #-------------------------------------------------------
    output$path_ui <- renderUI({
      path(chooseDir_server('chooseDir'))
      chooseDir_ui(ns('chooseDir'))
      
    })
    
    
    output$mode_ui <- renderUI({
      selectInput(ns('mode'), 'Mode', choices = c('process', 'pipeline'), width='100px')
    })
    
    output$parent_ui <- renderUI({
      textInput(ns('parent'), 'Parent', width='100px')
    })
    
    output$name_ui <- renderUI({
      textInput(ns('name'), 'name', width='100px')
    })
    
    output$steps_ui <- renderUI({
      dyn_widgets_ui(ns('dyn_steps'))
    })
    
    res <- reactiveValues(dataOut = list())
    res$dataOut <- dyn_widgets_server('dyn_steps')
    
  
    ################################################################### 
    
    build_fullname <- reactive({
      paste0(input$parent, '_', input$name)
    })
    
    observeEvent(input$createTemplate, {
      browser()
      miniConfig <- list(fullname = paste0(input$parent, '_', input$name),
                         mode = input$mode,
                         steps = res$dataOut()$inputs,
                         mandatory = res$dataOut()$mandatory
                         )
      
      files(createModuleTemplate(miniConfig, path = path()()))
    })
    
    output$filesCreated <- renderUI({
      req(files())
      lapply(files(), function(i) p(i))
    })
})
}



create_process_template <- function(){
  ui <- create_process_template_ui('test')
  
  server <- function(input, output, session) 
    create_process_template_server('test')
  
  shinyApp(ui, server)
  
}

create_process_template()

