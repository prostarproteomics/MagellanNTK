if(interactive()){
  library(shiny)
  
  verbose <- TRUE
  
  path <- system.file('extdata/module_examples', package='MagellanNTK')
  
  
  # Uncomment and Change this for a process workflow
   name <- 'PipelineA_Process1'
   #name <- 'PipelineA_Description'
   layout <- c('h')
  
  
  # Uncomment and Change this for a pipeline workflow
  #name <- 'PipelineA'
  #layout <- c('v', 'h')
  
  
  
  
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
  
  output$UI <- renderUI({nav_ui(name)})
  
  output$debugInfos_ui <- renderUI({
    req(verbose)
    Debug_Infos_ui('debug_infos')
  })
  
  Debug_Infos_server(id = 'debug_infos',
    title = 'Infos from shiny app',
    rv.dataIn = reactive({rv$dataIn}),
    dataOut = reactive({rv$dataOut$dataOut()})
  )
  
  observe({
    rv$dataOut <- nav_server(id = name,
      dataIn = reactive({rv$dataIn}),
      tl.layout = layout,
      verbose = verbose,
      path = path
      )
  })
}


shinyApp(ui, server)


}

