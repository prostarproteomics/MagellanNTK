#' @title Main Shiny application
#' @description xxx
#' 
#' @export
#' @examples
#' if(interactive()){
#' 
#' # launch without initial config
#' MagellanNTK()
#' 
#' # launch with config
#' funcs <- list(convert = "DaparToolshed::convert",
#' open_dataset = "DaparToolshed::open_dataset",
#' open_demoDataset = "DaparToolshed::open_demoDataset",
#' view_dataset = "DaparViz::view_dataset",
#' infos_dataset = "DaparToolshed::infos_dataset")
#' 
#' funcs <- list(convert = "MagellanNTK::convert",
#'   open_dataset = "MagellanNTK::open_dataset",
#'   open_demoDataset = "MagellanNTK::open_demoDataset",
#'   view_dataset = "MagellanNTK::view_dataset",
#'   infos_dataset = "MagellanNTK::infos_dataset")
#' 
#' MagellanNTK(funcs)
#' }
#' 
MagellanNTK <- function(config = default.config) {
  
  file_path_ui <- system.file("app/ui.R", package = "MagellanNTK")
  file_path_server <- system.file("app/server.R", package = "MagellanNTK")
  if (!nzchar(file_path_ui) || !nzchar(file_path_server)) 
    stop("Shiny app not found")
  
  ui <- server <- NULL # avoid NOTE about undefined globals
  source(file_path_ui, local = TRUE)
  source(file_path_server, local = TRUE)
  server_env <- environment(server)
  
  # Here you add any variables that your server can find
  server_env$funcs <- config$funcs
  server_env$title <- config$title
  server_env$base_URL <- config$base_URL
  
  app <- shiny::shinyApp(ui_MagellanNTK, server_MagellanNTK)
  shiny::runApp(app)
  
  
}