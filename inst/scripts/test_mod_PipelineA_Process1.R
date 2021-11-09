library(highcharter)
library(shinyjs)
library(tibble)
library(crayon)


options(shiny.fullstacktrace = TRUE)

setwd('~/GitHub/Magellan/inst/scripts')

source(file.path('./module_examples', "mod_PipelineA_Process1.R"), 
       local=TRUE)$value


dirpath <- '../../R'
for (l in list.files(path = dirpath, pattern = ".R", recursive = TRUE))
  source(file.path(dirpath, l), local=TRUE)$value


#----------------------------------------------------------------------
ui <- fluidPage(
  tagList(
    uiOutput('UI'),
    
    # Just for example purpose
    wellPanel(
      uiOutput('show_Debug_Infos')
    )
  )
)


#----------------------------------------------------------------------
server <- function(input, output){
  data(feat1, package='Magellan')
  rv <- reactiveValues(
    dataIn = feat1,
    dataOut = NULL
  )
  
  observe({
    
     # rv$dataOut <- mod_navigation_server(id = 'PipelineA_Process1',
     #                                     nav.mode = 'process',
     #                                     dataIn = reactive({rv$dataIn})
     #                                     )
    
    rv$dataOut <- mod_nav_process_server(id = 'PipelineA_Process1',
                                        dataIn = reactive({rv$dataIn}))
  }, priority=1000)
  
  
  output$UI <- renderUI({
    #mod_navigation_ui('PipelineA_Process1')
    mod_nav_process_ui('PipelineA_Process1')
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

