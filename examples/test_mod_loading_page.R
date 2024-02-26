if(interactive()){
library(shinyjs)
library(shiny)
ui <- mod_loading_page_ui("mod_info")
server <- function(input, output, session) {
  mod_loading_page_server("mod_info", 'test title')
}

shinyApp(ui, server)
}
