library(shiny)
library(R6)
library(highcharter)



source(file.path('../../../R', 'class_TimelineDraw.R'), local=TRUE)$value
source(file.path('../../../R', 'class_global.R'), local=TRUE)$value
source(file.path('../../../R', 'class_ScreenManager.R'), local=TRUE)$value
source(file.path('../../../R', 'class_Process.R'), local=TRUE)$value


source(file.path('.', 'class_ProcessA.R'), local=TRUE)$value


actionBtnClass <- "btn-primary"

proc <- ProcessA$new('processA', verbose = TRUE)

ui = fluidPage(
  tagList(
    proc$ui()
  )
  )

server = function(input, output){
  # Get a QFeatures dataset for example
  basename(f <- msdata::quant(pattern = "cptac", full.names = TRUE))
  i <- grep("Intensity\\.", names(read.delim(f)))
  cptac <- QFeatures::readQFeatures(f, ecol = i, sep = "\t", name = "peptides", fnames = "Sequence")
  
  rv <- reactiveValues(
    dataIn = cptac,
    dataOut = NULL
  )
  
  rv$dataOut <- proc$server(dataIn = reactive({rv$dataIn}))
  }

shiny::shinyApp(ui, server)