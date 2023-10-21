
library(shiny)

create_process_template_ui <- function(id) {
  ns <- NS(id)
  tagList(
    p('test'),
    uiOutput(ns('mode_ui')),
    uiOutput(ns('parent_ui')),
    uiOutput(ns('name_ui')),
    actionButton(ns('addStep'), 'Add step'),
    uiOutput(ns('steps_ui')),
    actionButton(ns('createTemplate'), 'Create template')
  )
}


create_process_template_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    rv <- reactiveValues(
      steps = NULL
    )
    
    output$mode_ui <- renderUI({
      selectInput(ns('mode'), 'Mode', choices = c('process', 'pipeline'), width='100px')
    })
    
    output$parent_ui <- renderUI({
      textInput(ns('parent'), 'Parent', width='100px')
    })
    
    output$name_ui <- renderUI({
      textInput(ns('name'), 'name', width='100px')
    })
    
    observeEvent(input$addStep, {
      name <- paste0('step_', length(rv$steps))
      rv$steps[[name]] <- textInput(ns(name), name, 
                                             value = rv$steps[[name]],
                                             width='100px')
      # ,
      #                              selectInput(ns(paste0('mandatory_', name)), 
      #                                          paste0('mandatory_', name),
      #                                          choices=c(TRUE, FALSE), 
      #                                          width='70px')
                                   # )

    })
    
    output$steps_ui <- renderUI({
      rv$steps
    })
  
    ################################################################### 
    
    build_fullname <- reactive({
      paste0(input$parent, '_', input$name)
    })
    
    observeEvent(input$createTemplate, {
      fullname <- paste0(input$parent, '_', input$name)
      browser()
      steps <- names(rv$steps)
      mandatory <- unlist(rv$steps)
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

