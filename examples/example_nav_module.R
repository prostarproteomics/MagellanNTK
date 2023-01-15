#if(interactive()){
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
    selectInput('chooseDataset', 'Choose dataset', choices = c('None', 'data1', 'data_na')),
    uiOutput('UI'),
    uiOutput('debugInfos_ui')
  )
)

server <- function(input, output){
  
  data(data1)
  data(data_na)
  
  rv <- reactiveValues(
    dataIn = NULL,
    dataOut = NULL
  )
  
  
  observeEvent(input$chooseDataset, { 
    if (input$chooseDataset == 'None') rv$dataIn <- NULL
    else if (input$chooseDataset == 'data1') rv$dataIn <- data1
    else if (input$chooseDataset == 'data_na') rv$dataIn <- data_na
  })
  
  output$UI <- renderUI({nav_ui(name)})
  
  output$debugInfos_ui <- renderUI({
    req(verbose)
    Debug_Infos_server(id = 'debug_infos',
                       title = 'Infos from shiny app',
                       rv.dataIn = reactive({rv$dataIn}),
                       dataOut = reactive({rv$dataOut$dataOut()})
                       )
    Debug_Infos_ui('debug_infos')
  })
  
  
  
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


#}

