#' if (interactive()){
ui <- fluidPage(
tagList(
  mod_open_demoDataset_ui("demo"),
  textOutput('res')
)
)


path <- '/home/samuel/Github/MagellanNTK/inst/Workflows_examples'
server <- function(input, output, session) {
  rv <- reactiveValues(
    obj = NULL
  )
  rv$obj <- mod_open_demoDataset_server("demo", path = reactive({path}))

  output$res <- renderText({
    rv$obj()
    paste0('Names of the datasets: ', names(rv$obj()))
  })
}

shinyApp(ui, server)
#' }