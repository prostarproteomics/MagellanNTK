library(highcharter)
library(shinyjs)
library(tibble)


options(shiny.fullstacktrace = TRUE)

setwd('~/GitHub/Magellan/inst/scripts')

source(file.path('./module_examples', "example_module_PipelineA_Description.R"), 
       local=TRUE)$value

dirpath <- '../../R'
for (l in list.files(path = dirpath, pattern = ".R", recursive = TRUE))
  source(file.path(dirpath, l), local=TRUE)$value


ui <- fluidPage(
  tagList(
    uiOutput('UI'),
    wellPanel(
      uiOutput('show_Debug_Infos')
    )
  )
)


server <- function(input, output){
  rv <- reactiveValues(
    dataIn = feat1,
    dataOut = NULL
  )
  
  observe({
    rv$dataOut <- mod_navigation_server(id = 'PipelineA_Description',
                                        nav.mode = 'process',
                                        dataIn = reactive({rv$dataIn})
                                        )

  }, priority=1000)
  
  
  output$UI <- renderUI({
    mod_navigation_ui('PipelineA_Description')
  })
  
  #--------------------------------------------------------------------
  # The following functions are only for example purpose
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
  
  output$show_rv_dataIn <- renderUI({
    lapply(names(rv$dataIn), function(x){tags$p(x)})
  })
  
  output$show_rv_dataOut <- renderUI({
    lapply(names(rv$dataOut$dataOut()$value), function(x){tags$p(x)})
  })
}


shinyApp(ui, server)
