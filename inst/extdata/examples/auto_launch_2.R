library(shiny)
library(R6)
library(tibble)
library(MSPipelines)
library(Magellan)
options(shiny.fullstacktrace = T)

#------------------------ Class TimelineDraw --------------------------------------
source(file.path('../../../R', 'class_TimelineDraw.R'), local=TRUE)$value
source(file.path('../../../R', 'global.R'), local=TRUE)$value
source(file.path('../../../R', 'class_ScreenManager.R'), local=TRUE)$value
source(file.path('../../../R', 'class_Process.R'), local=TRUE)$value
source(file.path('.', 'Example_ProcessA.R'), local=TRUE)$value

utils::data(Exp1_R25_prot, package='DAPARdata2')

pipe <- Example_ProcessA$new('App')

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
    res = NULL
  )
  
  rv$res <- pipe$server(dataIn = reactive({rv$dataIn}))
  
  # observeEvent(req(rv$res()), {
  #   print(rv$res()$trigger)
  # })
  
  observeEvent(input$send, {
    rv$dataIn <-Exp1_R25_prot 
  })
  
  output$show_pipe <- renderUI({
    req(pipe)
    pipe$ui()
  })
}
shiny::shinyApp(ui, server)