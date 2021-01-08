library(shiny)
library(R6)
library(tibble)
library(QFeatures)

verbose <- T

options(shiny.fullstacktrace = T)
options(shiny.reactlog=TRUE) 

#------------------------ Class TimelineDraw -----------------------------------
source(file.path('../../../R', 'class_TimelineDraw.R'), local=TRUE)$value
source(file.path('../../../R', 'global.R'), local=TRUE)$value
source(file.path('../../../R', 'class_ScreenManager.R'), local=TRUE)$value
source(file.path('../../../R', 'class_Process.R'), local=TRUE)$value
source(file.path('../../../R', 'class_Pipeline.R'), local=TRUE)$value

source(file.path('.', 'Example_Description.R'), local=TRUE)$value
source(file.path('.', 'Example_ProcessA.R'), local=TRUE)$value
source(file.path('.', 'Example_ProcessB.R'), local=TRUE)$value
source(file.path('.', 'Example_ProcessC.R'), local=TRUE)$value

source(file.path('.', 'Example.R'), local=TRUE)$value


## Main app
pipeline <- Example$new('App')

ui = fluidPage(
  tagList(
  actionButton('send', 'Send dataset'),
  actionButton('showPlotModal', 'Show plots'),
  mod_bsmodal_ui('plotModal'),
  pipeline$ui()
  )
)

server = function(input, output, session){
  
  # Get a QFeatures dataset for example
  basename(f <- msdata::quant(pattern = "cptac", full.names = TRUE))
  i <- grep("Intensity\\.", names(read.delim(f)))
  cptac <- QFeatures::readQFeatures(f, ecol = i, sep = "\t", name = "peptides", fnames = "Sequence")
  
  rv <- reactiveValues(
    res = NULL
  )
  rv$res <- pipeline$server(dataIn = reactive({rv$dataIn}))
  
  observeEvent(rv$res()$trigger, {
    print(rv$res()$trigger)
  })
  
  observeEvent(input$send,{
    if (input$send%%2 != 0)
      rv$dataIn <- cptac
    else
      rv$dataIn <- NULL
  })
  
  observeEvent(input$showPlotModal, {
    
  })
}

shiny::shinyApp(ui, server)