library(shiny)
library(shinyWidgets)
library(shinyjs)
library(R6)
library(tibble)

options(shiny.fullstacktrace = TRUE)

#------------------------ Class TimelineDraw --------------------------------------
source(file.path('../../R', 'class_TimelineDraw.R'), local=TRUE)$value
source(file.path('../../R', 'global.R'), local=TRUE)$value


config <- list(
  name = "test_TL_verticale",
  steps = c('step 1', 'step 2', 'step 3', 'step 4'),
  mandatory = c(step1=TRUE, step2=TRUE, step3=FALSE, step4=TRUE)
)

tl.h <- TimelineDraw$new('TL',
                       mandatory = config$mandatory,
                       orientation = 'h')

tl.v <- TimelineDraw$new('TL',
                       mandatory = config$mandatory,
                       orientation = 'v')

ui <- fluidPage(
  actionButton('pos', 'Change pos'),
  fluidRow(
    column(width=2, tl.v$ui()),
    column(width=10, tl.h$ui())
  )
)


server <- function(input, output){
  
  rv <- reactiveValues(
    status = c(0, 0, 0, 0),
    current.pos = 1,
    tl.tags.enabled = c(1, 1, 1, 1)
  )
  

  observeEvent(input$pos, {
    rv$current.pos <- input$pos %% length(rv$status) +1
  })
  

  tl.h$server(
    status = reactive({rv$status}),
    position = reactive({rv$current.pos}),
    enabled = reactive({rv$tl.tags.enabled})
  )
  
  tl.v$server(
    status = reactive({rv$status}),
    position = reactive({rv$current.pos}),
    enabled = reactive({rv$tl.tags.enabled})
  )
}


shinyApp(ui, server)
