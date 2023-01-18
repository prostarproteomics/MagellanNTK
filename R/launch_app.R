#' @title xxxx
#' 
#' @description xxxx
#' 
#' @param dev_mode xxxx
#' 
#' @export
#' 
launch_app <- function(dev_mode = FALSE) {
  ui_file_path <- system.file("app/ui.R", package = "MagellanNTK")
  if (!nzchar(ui_file_path)) stop("ui() function not found")
  server_file_path <- system.file("app/server.R", package = "MagellanNTK")
  if (!nzchar(server_file_path)) stop("Server() function not found")
  
  ui <- server <- NULL # avoid NOTE about undefined globals
  
  source(ui_file_path, local = TRUE)
  source(server_file_path, local = TRUE)
  
  server_env <- environment(server)
  
  # Here you add any variables that your server can find
  server_env$dev_mode <- dev_mode
  
  app <- shiny::shinyApp(ui, server)
  shiny::runApp(app)
}