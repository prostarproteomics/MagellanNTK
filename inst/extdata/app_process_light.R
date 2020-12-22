library(shiny)
library(R6)
library(highcharter)
library(DAPAR2)


source(file.path('../../R', 'class_TimelineDraw.R'), local=TRUE)$value
source(file.path('../../R', 'class_global.R'), local=TRUE)$value
source(file.path('../../R', 'class_ScreenManager.R'), local=TRUE)$value
source(file.path('../../R', 'class_Process.R'), local=TRUE)$value


source(file.path('.', 'class_ProcessA.R'), local=TRUE)$value


actionBtnClass <- "btn-primary"

proc <- ProcessA$new('App', verbose = TRUE)

ui = fluidPage(
  tagList(
    proc$ui()
  )
  )

server = function(input, output){
  utils::data(Exp1_R25_prot, package='DAPARdata2')
  
  rv <- reactiveValues(
    dataIn = Exp1_R25_prot,
    dataOut = NULL
  )
  
  rv$dataOut <- proc$server(dataIn = reactive({rv$dataIn}))
  }

shiny::shinyApp(ui, server)