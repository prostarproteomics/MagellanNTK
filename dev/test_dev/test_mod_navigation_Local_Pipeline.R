library(shiny)
library(shinyjs)
library(crayon)
library(Magellan)


options(shiny.fullstacktrace = TRUE)

setwd('~/GitHub/Magellan/dev/test_dev')

dirpath <- '../../R'
for (l in list.files(path = dirpath, pattern = ".R", recursive = TRUE))
  source(file.path(dirpath, l), local=TRUE)$value

dirpath <- 'example_modules'
for (l in list.files(path = dirpath, pattern = ".R", recursive = TRUE))
  source(file.path(dirpath, l), local=TRUE)$value

#--------------------------------------------


mod_test_navigation_pipeline_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns('UI')),
    wellPanel(title = 'foo',
              tagList(
                h3('Caller function'),
                uiOutput(ns('show_Debug_Infos'))
              )
    )
  )
}


mod_test_navigation_pipeline_server <- function(id){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    rv <- reactiveValues(
      dataIn = feat1,
      dataOut = NULL
    )
    
    observe({
      rv$dataOut <- mod_navigation_server(id = 'PipelineA',
                                          nav.mode = 'pipeline',
                                          dataIn = reactive({rv$dataIn}),
                                          is.enabled = reactive({TRUE}),
                                          remoteReset = reactive({FALSE})
      )
      output$UI <- renderUI({
        mod_navigation_ui(ns('PipelineA'))
      })
    }, priority=1000)
    
    
    
    #--------------------------------------------------------------------
    
    output$show_Debug_Infos <- renderUI({
      fluidRow(
        column(width=2,
               tags$b(h4(style = 'color: blue;', "Data In")),
               uiOutput(ns('show_rv_dataIn'))
        ),
        column(width=2,
               tags$b(h4(style = 'color: blue;', "Data Out")),
               uiOutput(ns('show_rv_dataOut'))
        )
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
  mod_test_navigation_pipeline_ui('test_pipeline')
)

#----------------------------------------------------------------------
server <- function(input, output){
  mod_test_navigation_pipeline_server('test_pipeline')
}


shinyApp(ui, server)

