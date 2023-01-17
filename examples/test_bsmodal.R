
#if (interactive()) {
      library(shiny)
      library(shinyBS)

      ui <- fluidPage(
          tagList(
            absolutePanel(
              h3('toto'), 
              bsmodal_ui("tbl"),
              top = '10px', left = NULL, right = '10px', bottom = NULL,
              width = NULL, height = NULL,
              draggable = TRUE, fixed = FALSE,
              cursor = c("auto", "move", "default", "inherit"))
            
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