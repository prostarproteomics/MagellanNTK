library(highcharter)
library(shinyjs)
library(tibble)
library(crayon)


options(shiny.fullstacktrace = TRUE)

setwd('~/GitHub/MagellanNTK/inst/scripts')

source(file.path('./module_examples', "example_module_PipelineDemo_Process1.R"), 
       local=TRUE)$value


dirpath <- '../../R'
for (l in list.files(path = dirpath, pattern = ".R", recursive = TRUE))
  source(file.path(dirpath, l), local=TRUE)$value


mod_test_navigation_process_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width=2, actionButton(ns('simReset'), 
                                   'Remote reset', 
                                   style='background-color: white;')),
      column(width=2, actionButton(ns('simEnabled'), 
                                   'Remote enable/disable', 
                                   style='background-color: white;')),
      column(width=2, actionButton(ns('simSkipped'), 
                                   'Remote is.skipped', 
                                   style='background-color: white;'))
    ),
    hr(),
    uiOutput(ns('UI')),
    wellPanel(title = 'foo',
              tagList(
                uiOutput(ns('show_Debug_Infos'))
              )
    )
  )
}


mod_test_navigation_process_server <- function(id){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    data(data1, package='MagellanNTK')
    rv <- reactiveValues(
      dataIn = data1,
      dataOut = NULL
    )
    
    observe({
      
      rv$dataOut <- mod_navigation_server(id = 'PipelineDemo_Process1',
        nav.mode = 'process',
        dataIn = reactive({rv$dataIn}),
        remoteReset = reactive({input$simReset}),
        is.skipped = reactive({input$simSkipped%%2 != 0}),
        is.enabled = reactive({input$simEnabled%%2 == 0})
      )
      
      
      observeEvent(rv$dataOut$dataOut()$trigger, {
        print(names(rv$dataOut$dataOut()$value))
      })
    }, priority=1000)
    
    
    output$UI <- renderUI({
      mod_navigation_ui(ns('PipelineDemo_Process1'))
    })
    
    
    
    #--------------------------------------------------------------------
    
    output$show_Debug_Infos <- renderUI({
      fluidRow(
        column(width=2,
               tags$b(h4(style = 'color: blue;', "Data In")),
               uiOutput(ns('show_rv_dataIn'))),
        column(width=2,
               tags$b(h4(style = 'color: blue;', "Data Out")),
               uiOutput(ns('show_rv_dataOut')))
      )
    })
    
    ###########---------------------------#################
    output$show_rv_dataIn <- renderUI({
      lapply(names(rv$dataIn), function(x){tags$p(x)})
    })
    
    output$show_rv_dataOut <- renderUI({
      lapply(names(rv$dataOut$dataOut()$value), function(x){tags$p(x)})
    })
    
  })
}


#----------------------------------------------------------------------
ui <- fluidPage(
  mod_test_navigation_process_ui('test_mod_process')
)


#----------------------------------------------------------------------
server <- function(input, output){
  mod_test_navigation_process_server('test_mod_process')
}
shinyApp(ui, server)

