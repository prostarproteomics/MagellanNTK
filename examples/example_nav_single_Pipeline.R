library(shiny)

if(interactive()){

    wf.name <- 'PipelineA'
    path <- system.file('extdata/module_examples', package='MagellanNTK')
    verbose <- FALSE

  ui <- fluidPage(
  tagList(
    uiOutput('UI'),
    uiOutput('debugInfos_ui')
  )
)

server <- function(input, output){

  
  data(data1)
  
  rv <- reactiveValues(
    dataIn = data1,
    dataOut = NULL
  )
  
  output$UI <- renderUI({nav_ui(wf.name)})
  
  output$debugInfos_ui <- renderUI({
    req(verbose)
    Debug_Infos_ui('debug_infos')
  })
  
  Debug_Infos_server(
    id = 'debug_infos',
    title = 'Infos from shiny app',
    rv.dataIn = reactive({rv$dataIn}),
    dataOut = reactive({rv$dataOut$dataOut()}))
  
  observe({
     rv$dataOut <- nav_server(id = wf.name,
       dataIn = reactive({rv$dataIn}),
       tl.layout = c('v', 'h'),
       verbose = verbose,
       path = path
       )
  })

}

shinyApp(ui, server)
}
