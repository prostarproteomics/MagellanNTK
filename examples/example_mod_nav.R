library(shiny)
library(shinyBS)

ui <- fluidPage(
  mod_nav_ui("Protein_Description")
  )

server <- function(input, output) {
  mod_nav_server(
    id = "Protein_Description",
    dataIn = reactive({Build_example_dataset()})
  )
  }

shinyApp(ui, server)

