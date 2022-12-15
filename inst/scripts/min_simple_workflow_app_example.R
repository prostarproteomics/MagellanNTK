library(MagellanNTK)

f <- system.file("extdata", "module_examples/mod_PipelineA_Process1.R", package="MagellanNTK")
source(f, local=TRUE)$value

ui <- fluidPage(
  mod_nav_ui('Process1')
)


server <- function(input, output){
  data(data1, package='MagellanNTK')
  rv <- reactiveValues(
    dataIn = data1,
    dataOut = NULL
  )
  
  observe({
    rv$dataOut <- mod_nav_server(id = 'Process1',
      dataIn = reactive({rv$dataIn})
      )
    }, priority=1000)
}


shinyApp(ui, server)
