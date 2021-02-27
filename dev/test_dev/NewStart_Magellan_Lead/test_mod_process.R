library(shiny)
library(shinyjs)
library(QFeatures)

options(shiny.fullstacktrace = T)
source(file.path('.', 'mod_nav_process.R'), local=FALSE)$value

verbose <- F
redBtnClass <- "btn-danger"
PrevNextBtnClass <- "btn-info"
btn_success_color <- "btn-success"
optionsBtnClass <- "info"
btn_style <- "display:inline-block; vertical-align: middle; padding: 7px"


AddItemToDataset <- function(dataset, name){
  addAssay(dataset, 
           dataset[[length(dataset)]], 
           name=name)
}


mod_test_process_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width=3,
             selectInput(ns('choosePipeline'), 'Choose pipeline',
                         choices = setNames(nm=c('', 'Protein')),
                         width = '200')
      ),
      column(width=5,
             selectInput(ns('chooseProcess'), 'Choose process', 
                         choices = setNames(nm=c('', 'Normalization', 'Description')),
                         width = '200')
      )
    ),
    uiOutput(ns('UI')),
    wellPanel(title = 'foo',
              tagList(
                h3('Valeur'),
                uiOutput(ns('show_Debug_Infos'))
              )
    )
  )
}


mod_test_process_server <- function(id){
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
      req(input$choosePipeline != '' && input$chooseProcess != '')
      basename <- paste0(input$choosePipeline, '_', input$chooseProcess)
      source(file.path('.', paste0('mod_', basename,'.R')), local=FALSE)$value
      
      rv$dataOut <- mod_nav_process_server(id = basename,
                                           dataIn = reactive({rv$dataIn}),
                                           is.enabled = reactive({TRUE}),
                                           reset = reactive({FALSE}),
                                           status = reactive({NULL})
                                           )
      observeEvent(rv$dataOut$dataOut()$trigger, {
        print('totototo')
        print(names(rv$dataOut$dataOut()$value))
       #browser()
      })
      
    }, priority=1000)
    
    
    
    
    
    output$UI <- renderUI({
      req(input$choosePipeline != '' && input$chooseProcess != '')
      mod_nav_process_ui(ns(paste0(input$choosePipeline, '_', input$chooseProcess)))
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
  mod_test_process_ui('test_mod_process')
)


#----------------------------------------------------------------------
server <- function(input, output){
  mod_test_process_server('test_mod_process')
}


shinyApp(ui, server)
