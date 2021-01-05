library(shiny)
library(R6)
library(tibble)

verbose <- T

options(shiny.fullstacktrace = T)
options(shiny.reactlog=TRUE) 

#------------------------ Class TimelineDraw -----------------------------------
source(file.path('../../../R', 'class_TimelineDraw.R'), local=TRUE)$value

source(file.path('../../../R', 'class_global.R'), local=TRUE)$value

#----------------------- Class ScreenManager ----------------------------------
source(file.path('../../../R', 'class_ScreenManager.R'), local=TRUE)$value
source(file.path('../../../R', 'class_Process.R'), local=TRUE)$value
source(file.path('.', 'class_ProcessDescription.R'), local=TRUE)$value
source(file.path('.', 'class_ProcessA.R'), local=TRUE)$value
source(file.path('.', 'class_ProcessB.R'), local=TRUE)$value
source(file.path('.', 'class_ProcessC.R'), local=TRUE)$value

source(file.path('../../../R', 'class_Pipeline.R'), local=TRUE)$value
source(file.path('.', 'class_PipelineSimple.R'), local=TRUE)$value


AddItemToDataset <- function(dataset, name){
  addAssay(dataset, 
           dataset[[length(dataset)]], 
           name=name)
}

## Main app
pipeline <- PipelineSimple$new('App')

ui = fluidPage(
  tagList(
  actionButton('send', 'Send dataset'),
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
}

shiny::shinyApp(ui, server)