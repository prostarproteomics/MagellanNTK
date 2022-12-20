if(interactive()){
  library(shiny)
library(MagellanNTK)

verbose = TRUE


  require(shiny)
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
  
  output$UI <- renderUI({mod_nav_ui('PipelineA_Process1')})
  
  output$debugInfos_ui <- renderUI({
    req(verbose)
    mod_Debug_Infos_ui('debug_infos')
  })
  
  mod_Debug_Infos_server(id = 'debug_infos',
    title = 'Infos from shiny app',
    rv.dataIn = reactive({rv$dataIn}),
    dataOut = reactive({rv$dataOut$dataOut()})
  )
  
  observe({
    rv$dataOut <- mod_nav_server(id = 'PipelineA_Process1',
      dataIn = reactive({rv$dataIn}),
      tl.layout = c('h'),
      verbose = verbose
      )
  })
}


shinyApp(ui, server)

}
