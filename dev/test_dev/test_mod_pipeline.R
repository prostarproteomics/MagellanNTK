
library(shiny)
library(shinyjs)
library(QFeatures)
library(crayon)
library(DaparToolshed)


options(shiny.fullstacktrace = TRUE)
setwd('~/GitHub/Magellan/dev/test_dev')

dirpath <- '../../R'
for (l in list.files(path = dirpath, pattern = ".R"))
  source(file.path(dirpath, l), local=TRUE)$value


verbose <- FALSE


mod_test_pipeline_ui <- function(id){
  ns <- NS(id)
  tagList(
    selectInput('choosePipeline', 'Choose pipeline',
                choices = setNames(nm=c('', 'Protein')),
                width = '200'),
    # selectInput(ns('choosePipeline'), 'Choose pipeline',
    #             choices = setNames(nm=c('', names(DaparToolshed::Pipelines()))),
    #             width = '200'),
    uiOutput('UI'),
    wellPanel(title = 'foo',
              tagList(
                h3('Valler'),
                uiOutput('show_Debug_Infos')
              )
    )
  )
}


mod_test_pipeline_server <- function(id){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
   
  rv <- reactiveValues(
    dataIn = QFeatures::feat1,
    dataOut = NULL
  )
  
  observe({
    
    req(input$choosePipeline != '')
    basename <- paste0('mod_', input$choosePipeline)
    #source(file.path('.', paste0(basename,'.R')), local=FALSE)$value
    
    rv$dataOut <- do.call(paste0(basename, '_server'),
                          list(id = input$choosePipeline,
                               dataIn = reactive({rv$dataIn}),
                               tag.enabled = reactive({TRUE})
                          )
    )
    
    
    #basename <- paste0('mod_', input$choosePipeline)
    #source(file.path('.', paste0(basename,'.R')), local=FALSE)$value
    # Get the return value of the pipeline server
    # rv$dataOut <- mod_nav_pipeline_server(id = input$choosePipeline,
    #                                       dataIn = reactive({rv$dataIn}),
    #                                       is.enabled = reactive({TRUE}),
    #                                       remoteReset = reactive({FALSE})
    # )
    
    # output$UI <- renderUI({
    #   req(input$choosePipeline != '')
    #   mod_nav_pipeline_ui(ns(input$choosePipeline))
    # })
    
    output$UI <- renderUI({
      req(input$choosePipeline != '')
      do.call(paste0('mod_', input$choosePipeline, '_ui'),
              list(id = input$choosePipeline))
    })
    
  }, priority=1000)
  
  
  
  
  #--------------------------------------------
  #--------------------------------------------------------------------
  
  output$show_Debug_Infos <- renderUI({
    fluidRow(
      column(width=2,
             tags$b(h4(style = 'color: blue;', "Data In")),
             uiOutput('show_rv_dataIn')),
      column(width=2,
             tags$b(h4(style = 'color: blue;', "Data Out")),
             uiOutput('show_rv_dataOut'))
    )
  })
  
  ###########---------------------------#################
  output$show_rv_dataIn <- renderUI({
    req(rv$dataIn)
    tagList(
      lapply(names(rv$dataIn), function(x){tags$p(x)})
    )
  })
  
  output$show_rv_dataOut <- renderUI({
    req(rv$dataOut)
    tagList(
      lapply(names(rv$dataOut()$value), function(x){tags$p(x)})
    )
  })
  
})
}


#----------------------------------------------------------------------
ui <- fluidPage(
  mod_test_pipeline_ui('test_pipeline')
)

#----------------------------------------------------------------------
server <- function(input, output){
  mod_test_pipeline_server('test_pipeline')
}


shinyApp(ui, server)

