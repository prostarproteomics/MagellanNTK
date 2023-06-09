library(MagellanNTK)

f <- system.file("extdata", "workflow/PipelineA/R/PipelineA_Process1.R", package="MagellanNTK")
source(f, local=TRUE)$value

ui <- fluidPage(
  nav_ui('PipelineA_Process1')
)


server <- function(input, output){
  data(data1, package='MagellanNTK')
  rv <- reactiveValues(
    dataIn = data1,
    dataOut = NULL
  )
  
  observe({
    rv$dataOut <- nav_server(id = 'PipelineA_Process1',
      dataIn = reactive({rv$dataIn})
      )
    }, priority=1000)
}


shinyApp(ui, server)
