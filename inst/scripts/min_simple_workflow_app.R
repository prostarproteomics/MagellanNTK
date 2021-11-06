library(Magellan)

# Load here the package containing the ui of steps
# library(foo)


# Replace 'parent_name' by whathever you want to identify the module. 
# It must be a `character(1)` which reflects both the name of the
# workflow and the name of its parent.
# See xxx for more details
ui <- fluidPage(
  mod_navigation_ui('parent_name')
)


server <- function(input, output){
  data(feat1, package='Magellan')
  
  # Load a dataset (i.e. with data())
  # data(xxx)
  
  rv <- reactiveValues(
    # Replace xxx by the name of the dataset
    dataIn = xxx,
    dataOut = NULL
  )
  
  observe({
    rv$dataOut <- mod_navigation_server(id = 'parent_name',
                                        nav.mode = 'process',
                                        dataIn = reactive({rv$dataIn})
    )
  }, priority=1000)
}


shinyApp(ui, server)