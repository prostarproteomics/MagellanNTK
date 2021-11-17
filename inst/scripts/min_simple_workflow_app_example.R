library(Magellan)

f <- system.file("scripts/module_examples", "mod_Process1.R", package="Magellan")
source(file(f), local=TRUE)$value

ui <- fluidPage(
  mod_nav_ui('Process1')
)


server <- function(input, output){
  data(feat1, package='Magellan')
  rv <- reactiveValues(
    dataIn = feat1,
    dataOut = NULL
  )
  
  observe({
    rv$dataOut <- mod_nav_server(id = 'Process1',
                                 dataIn = reactive({rv$dataIn})
                                 )
    }, priority=1000)
}


shinyApp(ui, server)
