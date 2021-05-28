library(shiny)
library(shinyjs)
library(QFeatures)
library(DAPAR2)
library(Magellan)

options(shiny.fullstacktrace = TRUE)


verbose <- FALSE

AddItemToDataset <- function(dataset, name){
  addAssay(dataset, 
           dataset[[length(dataset)]], 
           name=name)
}


ui <- fluidPage(
  tagList(
    fluidRow(
      column(width=3,
             selectInput('choosePipeline', 'Choose pipeline',
                         choices = setNames(nm=c('', 'Protein')),
                         width = '200')
             ),
      column(width=5,
             selectInput('chooseProcess', 'Choose process', 
                         choices = setNames(nm=c('', 'Normalization', 'Description')),
                         width = '200')
             )
      ),
    uiOutput('UI'),
    wellPanel(title = 'foo',
              tagList(
                h3('Valler'),
                uiOutput('show_Debug_Infos')
              )
    )
  )
)


server <- function(input, output){
  utils::data(Exp1_R25_prot, package='DAPARdata2')
  
  obj <- NULL
  obj <- Exp1_R25_prot
  
  rv <- reactiveValues(
    dataIn = Exp1_R25_prot,
    dataOut = NULL
  )
  
  
  
  observe({
    req(input$choosePipeline != '' && input$chooseProcess != '')
    basename <- paste0('mod_', input$choosePipeline, '_', input$chooseProcess)
    #source(file.path('.', paste0(basename,'.R')), local=FALSE)$value
    
    rv$dataOut <- do.call(paste0(basename, '_server'),
                      list('process',
                           dataIn = reactive({obj}),
                           tag.enabled = reactive({TRUE})
                           )
                      )

  }, priority=1000)
  
  
  output$UI <- renderUI({
    req(input$choosePipeline != '' && input$chooseProcess != '')
    do.call(paste0('mod_', input$choosePipeline, '_', input$chooseProcess, '_ui'),
            list('process'))
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
 
}


shinyApp(ui, server)
