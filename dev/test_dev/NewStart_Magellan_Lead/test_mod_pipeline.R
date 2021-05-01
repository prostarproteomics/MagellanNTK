library(shiny)
library(shinyjs)
library(QFeatures)
library(crayon)
library(MSPipelines)


options(shiny.fullstacktrace = T)
source(file.path('.', 'mod_nav_pipeline.R'), local=FALSE)$value

verbose <- F
redBtnClass <- "btn-danger"
PrevNextBtnClass <- "btn-info"
btn_success_color <- "btn-success"
optionsBtnClass <- "info"
btn_style <- "display:inline-block; vertical-align: middle; padding: 7px"


mod_test_pipeline_ui <- function(id){
  ns <- NS(id)
  tagList(
    selectInput(ns('choosePipeline'), 'Choose pipeline',
                choices = setNames(nm=c('', names(MSPipelines::Pipelines()))),
                width = '200'),
    uiOutput(ns('UI')),
    wellPanel(title = 'foo',
              tagList(
                h3('Valler'),
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
      
      req(input$choosePipeline != '')
      #basename <- paste0('mod_', input$choosePipeline)
      #source(file.path('.', paste0(basename,'.R')), local=FALSE)$value
      # Get the return value of the pipeline server
      rv$dataOut <- mod_nav_pipeline_server(id = input$choosePipeline,
                                            dataIn = reactive({rv$dataIn}),
                                            is.enabled = reactive({TRUE}),
                                            remoteReset = reactive({FALSE})
                                            )
      #browser()
      
      output$UI <- renderUI({
        req(input$choosePipeline != '')
        mod_nav_pipeline_ui(ns(input$choosePipeline))
      })
      
    }, priority=1000)
    
    
    
    
    #--------------------------------------------
    #--------------------------------------------
    
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
      req(rv$dataIn)
      tagList(
        lapply(names(rv$dataIn), function(x){tags$p(x)})
      )
    })
    
    output$show_rv_dataOut <- renderUI({
      req(rv$dataOut)
      tagList(
        lapply(names(rv$dataOut$dataOut()$value), function(x){tags$p(x)})
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
