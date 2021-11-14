library(shiny)
library(shinyjs)
library(crayon)


options(shiny.fullstacktrace = TRUE)

setwd('~/GitHub/Magellan/inst/scripts')

dirpath <- 'module_examples'
for (l in list.files(path = dirpath, pattern = ".R", recursive = TRUE))
  source(file.path(dirpath, l), local=TRUE)$value


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
     rv$dataOut <- mod_nav_server(id = 'PipelineA',
                                  dataIn = reactive({rv$dataIn}),
                                  timelines = c('v', 'h')
                                  )
    
    output$UI <- renderUI({
      mod_nav_ui('PipelineA')
    })
  }, priority=1000)
  
  
  mod_Debug_Infos_server(id = 'debug_infos',
                         title = 'Infos from global shiny app',
                         rv.dataIn = reactive({rv$dataIn}),
                         dataOut = reactive({rv$dataOut$dataOut()}))
  
}



shinyApp(ui, server)

