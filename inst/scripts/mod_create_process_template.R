
library(shiny)
library(shinyFiles)

create_process_template_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    h3('Create process template'),
    uiOutput(ns('path_ui')),
    fluidRow(
      column(width = 4, selectInput(ns('mode'), 'Mode', choices = c('pipeline', 'process'), width='100px')),
      column(width = 4, 
             shinyjs::disabled(
               textInput(ns('parent'), 'Parent', width='100px')
             )),
      column(width = 4, textInput(ns('name'), 'Name', width='100px'))
    ),
    dyn_widgets_ui(ns('dyn_steps')),
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
    
    observe({
      input$mode
      shinyjs::toggle('parent', condition = input$mode=='process')
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
      
      files(createModuleTemplate(miniConfig, path = path()()))
    })
    
    output$filesCreated <- renderUI({
      req(files())
      tagList(
        p('Template files created;'),
        lapply(files(), function(i) p(i))
      )
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

