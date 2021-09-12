library(shiny)
library(shinyjs)
library(QFeatures)
library(crayon)


options(shiny.fullstacktrace = TRUE)
setwd('~/GitHub/Magellan/dev/test_dev')

dirpath <- '../../R'
for (l in list.files(path = dirpath, pattern = ".R"))
  source(file.path(dirpath, l), local=TRUE)$value
#--------------------------------------------

source(file.path('example_modules', 'mod_PipelineA_Description.R'), local=TRUE)$value
source(file.path('example_modules', 'mod_PipelineA_ProcessA.R'), local=TRUE)$value


mod_test_pipeline_ui <- function(id){
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


mod_test_pipeline_server <- function(id){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    utils::data(Exp1_R25_prot, package='DAPARdata2')
    
    obj <- NULL
    obj <- Exp1_R25_prot
    
    rv <- reactiveValues(
      dataIn = Exp1_R25_prot,
      dataOut = NULL
    )
    
    observe({
      source(file.path('example_modules', 'mod_PipelineA.R'), local=TRUE)$value
      
      
      rv$dataOut <- mod_nav_pipeline_server(id = 'PipelineA',
                                            dataIn = reactive({rv$dataIn}),
                                            is.enabled = reactive({TRUE}),
                                            remoteReset = reactive({FALSE})
                                            )
    }, priority=1000)
    
    
    output$UI <- renderUI({
      mod_nav_pipeline_ui(ns('PipelineA'))
    })
    

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

