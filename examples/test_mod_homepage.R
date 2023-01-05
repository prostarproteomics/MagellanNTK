if (interactive()){
  
  library(shiny)
  options(shiny.fullstacktrace = TRUE)
  
  
  ui <- fluidPage(
    mod_homepage_ui("demo")
  )
  
  server <- function(input, output, session) {
    
    base_URL <- "http://www.prostar-proteomics.org/md/"
    
    mod_homepage_server("demo",
      file.md = file.path("http://www.prostar-proteomics.org/md", "presentation.md"))
  }
  
  shinyApp(ui, server)
}