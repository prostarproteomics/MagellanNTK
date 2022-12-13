library(shiny)
library(MagellanNTK)

if(interactive()){
example_mod_Pipeline <- function(verbose = FALSE){

  ui <- fluidPage(
  tagList(
    uiOutput('UI'),
    uiOutput('debugInfos_ui')
  )
)

server <- function(input, output){

  dirpath <- system.file('extdata/module_examples', package='MagellanNTK')
  for (l in list.files(path = dirpath, pattern = ".R", recursive = TRUE))
    source(file.path(dirpath, l), local=FALSE)$value
  
  rv <- reactiveValues(
    dataIn = Build_example_dataset(),
    dataOut = NULL
  )
  
  output$UI <- renderUI({mod_nav_ui('PipelineA')})
  
  output$debugInfos_ui <- renderUI({
    req(verbose)
    mod_Debug_Infos_ui('debug_infos')
  })
  
  mod_Debug_Infos_server(
    id = 'debug_infos',
    title = 'Infos from shiny app',
    rv.dataIn = reactive({rv$dataIn}),
    dataOut = reactive({rv$dataOut$dataOut()}))
  
  observe({
     rv$dataOut <- mod_nav_server(id = 'PipelineA',
       dataIn = reactive({rv$dataIn}),
       tl.layout = c('v', 'h'),
       verbose = verbose
       )
  })

}

shinyApp(ui, server)
}

example_mod_Pipeline()
}