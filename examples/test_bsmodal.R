
#if (interactive()) {
      library(shiny)
      library(shinyBS)

      ui <- fluidPage(
          tagList(
            bsmodal_ui("tbl")
            
          )
      )
      server <- function(input, output) {
          bsmodal_server(
              id = "tbl",
              title = "test",
              uiContent = p("test")
          )}
      shinyApp(ui, server)
 # }