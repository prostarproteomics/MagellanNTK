library(shiny)
library(R6)
library(tibble)
library(MSPipelines)
library(Magellan)
options(shiny.fullstacktrace = T)

#------------------------ Class TimelineDraw --------------------------------------
 source(file.path('../../R', 'class_TimelineDraw.R'), local=TRUE)$value
 source(file.path('../../R', 'global.R'), local=TRUE)$value
 source(file.path('../../R', 'class_ScreenManager.R'), local=TRUE)$value
 source(file.path('../../R', 'class_Process.R'), local=TRUE)$value
source(file.path('.', 'Example_ProcessA.R'), local=TRUE)$value
source(file.path('../../R', 'class_Launch_Magellan.R'), local=TRUE)$value

utils::data(Exp1_R25_prot, package='DAPARdata2')

pipe <- Magellan$new('App', name = "Example_ProcessA")

ui = fluidPage(
  tagList(
    pipe$ui()
  )
)


server = function(input, output){
  # Get a QFeatures dataset for example
  rv <- reactiveValues(
    dataIn = NULL,
    res = NULL
  )
  #pipe$Launch_Pipeline('Example_ProcessA')
  rv$res <- pipe$server(dataIn = reactive({rv$dataIn}))
  rv$dataIn <- Exp1_R25_prot
  # observeEvent(req(rv$res()), {
  #   print(rv$res()$trigger)
  # })
}
shiny::shinyApp(ui, server)