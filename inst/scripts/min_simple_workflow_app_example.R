library(MagellanNTK)

f <- system.file("module_examples", "extdata/mod_Process1.R", package="MagellanNTK")
source(file(f), local=TRUE)$value

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
