launch_app <- function(dev_mode = FALSE) {
  file_path <- system.file("myapp.R", package = "mypackage")
  if (!nzchar(file_path)) stop("Shiny app not found")
  ui <- server <- NULL # avoid NOTE about undefined globals
  source(file_path, local = TRUE)
  server_env <- environment(server)
  
  # Here you add any variables that your server can find
  server_env$dev_mode <- dev_mode
  
  app <- shiny::shinyApp(ui, server)
  shiny::runApp(app, ...)
}