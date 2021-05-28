library(shiny)
library(R6)
library(tibble)
library(MSPipelines)
library(Magellan)
options(shiny.fullstacktrace = TRUE)

#------------------------ Class TimelineDraw --------------------------------------
source(file.path('../../R', 'class_TimelineDraw.R'), local=TRUE)$value
source(file.path('../../R', 'global.R'), local=TRUE)$value
source(file.path('../../R', 'class_ScreenManager.R'), local=TRUE)$value
source(file.path('../../R', 'class_Process.R'), local=TRUE)$value
source(file.path('.', 'Example_ProcessA.R'), local=TRUE)$value

utils::data(Exp1_R25_prot, package='DAPARdata2')


ui = fluidPage(
  tagList(
    shinyjs::useShinyjs(),
    #pipe$ui()
    actionButton('send', 'send'),
    uiOutput('show_pipe')
  )
)


server = function(input, output){
  # Get a QFeatures dataset for example
  rv <- reactiveValues(
    dataIn = NULL,
    res = NULL,
    pipe = NULL
  )
  rv$pipe <- Example_ProcessA$new('App')
  
  observe({
    rv$res <- rv$pipe$server(dataIn = reactive({rv$dataIn}))
  })
  

  observeEvent(input$send, {
    rv$dataIn <-Exp1_R25_prot 
  })
  
  output$show_pipe <- renderUI({
    req(rv$pipe)
    rv$pipe$ui()
  })
}
shiny::shinyApp(ui, server)