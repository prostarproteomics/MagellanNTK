
#if (interactive()) {
library(shiny)
library(shinyBS)
library(markdown)

ui <- fluidPage(
  #mod_insert_md_ui("tbl")
  includeMarkdown('/home/samuel/R/x86_64-pc-linux-gnu-library/4.2/MagellanNTKExample/workflows/PipelineA/md/links.md')
)

server <- function(input, output) {

  
  }

shinyApp(ui, server)
# }