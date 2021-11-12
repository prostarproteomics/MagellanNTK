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
    mod_Debug_Infos_ui('debug_infos')
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
    
      rv$dataOut <- mod_nav_server(id = 'PipelineA_Process1',
                                   nav.mode = 'process',
                                   dataIn = reactive({rv$dataIn})
                                   )
    
    #rv$dataOut <- mod_nav_process_server(id = 'PipelineA_Process1',
    #                                     dataIn = reactive({rv$dataIn}))
  }, priority=1000)
  
  
  output$UI <- renderUI({
    mod_nav_ui('PipelineA_Process1')
    #mod_nav_process_ui('PipelineA_Process1')
  })
  
  mod_Debug_Infos_server(id = 'debug_infos',
                         title = 'Infos from global shiny app',
                         rv.dataIn = reactive({rv$dataIn}),
                         dataOut = reactive({rv$dataOut$dataOut()}))

}


shinyApp(ui, server)

