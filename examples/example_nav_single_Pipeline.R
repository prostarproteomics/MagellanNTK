library(shiny)

if(interactive()){
example_mod_Pipeline <- function(
    wf.name = 'PipelineA', 
    verbose = FALSE){

  
  LoadCode(wf.name)
  
  
  ui <- fluidPage(
  tagList(
    uiOutput('UI'),
    uiOutput('debugInfos_ui')
  )
)

server <- function(input, output){

  
  path <- system.file('extdata/module_examples', package='MagellanNTK')
  for (l in list.files(path = path, pattern = ".R", recursive = TRUE))
    source(file.path(path, l), local=FALSE)$value
  
  data(data1)
  
  rv <- reactiveValues(
    dataIn = data1,
    dataOut = NULL
  )
  
  output$UI <- renderUI({mod_nav_ui(wf.name)})
  
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
     rv$dataOut <- mod_nav_server(id = wf.name,
       dataIn = reactive({rv$dataIn}),
       tl.layout = c('v', 'h'),
       verbose = verbose
       )
  })

}

shinyApp(ui, server)
}

example_mod_Pipeline(verbose=F)
}