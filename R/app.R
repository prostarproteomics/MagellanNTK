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
MagellanNTK <- function() {
  source(system.file('app/global.R', package = 'MagellanNTK'))
  source(system.file('app/ui.R', package = 'MagellanNTK'))
  source(system.file('app/server.R', package = 'MagellanNTK'))
  
  app <- shiny::shinyApp(ui = ui_MagellanNTK, server = server_MagellanNTK)
  shiny::runApp(app)
}