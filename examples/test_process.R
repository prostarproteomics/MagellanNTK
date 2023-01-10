#if(interactive()){
  
  test_mod_process <- function(){
  
  library(shiny)
  library(shinyjs)
  library(MagellanNTK)


options(shiny.fullstacktrace = TRUE)


#' @importFrom stats setNames
#' 
mod_test_process_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width=2,
             selectInput(ns('choosePipeline'), 'Choose pipeline',
                         choices = setNames(nm=c('', 'PipelineA')),
                         width = '200')
             ),
      column(width=2,
             selectInput(ns('chooseProcess'), 'Choose process', 
                         choices = setNames(nm=c('', 'Description', 'Process1')),
                         width = '200')
             ),
      column(width=2, actionButton(ns('simReset'), 'Remote reset')),
      column(width=2, actionButton(ns('simEnabled'), 'Remote enable/disable')),
      column(width=2, actionButton(ns('simSkipped'), 'Remote is.skipped'))
      
      ),
    uiOutput(ns('UI')),
    wellPanel(title = 'foo',
              tagList(
                h3('Valler'),
                uiOutput(ns('show_Debug_Infos'))
              )
    )
  )
}


mod_test_process_server <- function(id){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    rv <- reactiveValues(
      dataIn = data1,
      remoteReset = FALSE,
      remoteSkipped = FALSE,
      remoteEnabled = TRUE
      )
  
  observeEvent(input$simReset, {rv$remoteReset <- input$simReset})
  observeEvent(input$simEnabled, {rv$remoteEnabled <- input$simEnabled%%2 != 0})
  observeEvent(input$simSkipped, {rv$remoteSkipped <- input$simSkipped%%2 != 0})
  
  
  observe({
    req(input$choosePipeline != '' && input$chooseProcess != '')
    basename <- paste0(input$choosePipeline, '_', input$chooseProcess)

    rv$dataOut <- mod_nav_process_server(id = basename,
      dataIn = reactive({rv$dataIn}),
      is.enabled = reactive({rv$remoteEnabled}),
      remoteReset = reactive({rv$remoteReset}),
      is.skipped = reactive({rv$remoteSkipped})
      )
    
  }, priority=1000)
  
  
  output$UI <- renderUI({
    req(input$choosePipeline != '' && input$chooseProcess != '')
    basename <- paste0(input$choosePipeline, '_', input$chooseProcess)
    # do.call(paste0(basename, '_ui'),
    #          list('process'))
     mod_nav_process_ui(ns(basename))
     
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
      lapply(names(rv$dataOut$value), function(x){tags$p(x)})
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
  }
  
#}