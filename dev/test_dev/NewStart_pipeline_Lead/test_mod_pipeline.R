library(shiny)
library(shinyjs)
library(QFeatures)

options(shiny.fullstacktrace = T)
source(file.path('.', 'mod_nav_pipeline.R'), local=FALSE)$value
source(file.path('.', 'mod_Protein.R'), local=FALSE)$value

verbose <- F
redBtnClass <- "btn-danger"
PrevNextBtnClass <- "btn-info"
btn_success_color <- "btn-success"
optionsBtnClass <- "info"
btn_style <- "display:inline-block; vertical-align: middle; padding: 7px"


#----------------------------------------------------------------------
ui <- fluidPage(
  tagList(
    selectInput('choosePipeline', 'Choose pipeline',
                choices = setNames(nm=c('', 'Protein')),
                width = '200'),
    uiOutput('UI'),
    wellPanel(title = 'foo',
              tagList(
                h3('Valler'),
                uiOutput('show_Debug_Infos')
              )
    )
  )

)

#----------------------------------------------------------------------
server <- function(input, output){
  utils::data(Exp1_R25_prot, package='DAPARdata2')
  
  obj <- NULL
  obj <- Exp1_R25_prot
  
  rv <- reactiveValues(
    dataIn = Exp1_R25_prot,
    dataOut = NULL
  )
  
  observe({
    
    req(input$choosePipeline != '')
    basename <- paste0('mod_', input$choosePipeline)
    source(file.path('.', paste0(basename,'.R')), local=FALSE)$value
   # browser()
    rv$dataOut <- do.call(paste0(basename, '_server'),
                          list(id = input$choosePipeline,
                               dataIn = reactive({rv$dataIn}),
                               is.enabled = reactive({TRUE})
                          )
    )
    
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
  
}


shinyApp(ui, server)
