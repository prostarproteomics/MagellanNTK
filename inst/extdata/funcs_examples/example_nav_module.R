if(interactive()){
  library(shiny)
  server_env <- environment() # will see all dtwclust functions
  server_env$dev_mode <- FALSE
  
  # Uncomment and Change this for a process workflow
  # name <- 'PipelineDemo_Process1'
   #name <- 'PipelineDemo_Description'
  # layout <- c('h')
  
  
  # Uncomment and Change this for a pipeline workflow
  name <- 'PipelineDemo'
  layout <- c('v', 'h')
  
  
  path <- system.file('workflow', package='MagellanNTK')
  files <- list.files(file.path(path, name, 'R'), full.names = TRUE)
  for(f in files)
    source(f, local = TRUE, chdir = TRUE)
  
  
  
  ui <- fluidPage(
  tagList(
    fluidRow(
      column(width=2, actionButton('simReset', 'Remote reset',  class='info')),
      column(width=2, actionButton('simEnabled', 'Remote enable/disable', class='info')),
      column(width=2, actionButton('simSkipped', 'Remote is.skipped', class='info')),
      column(width=2, selectInput('chooseDataset', 'Choose dataset', 
                                  choices = c('None', 'data1', 'data_na')))
    ),
    hr(),
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
    req(dev_mode)
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
                             remoteReset = reactive({input$simReset}),
                             is.skipped = reactive({input$simSkipped%%2 != 0}),
                             is.enabled = reactive({input$simEnabled%%2 == 0}),
                             tl.layout = layout)
  })
}



shiny::shinyApp(ui, server)


}

