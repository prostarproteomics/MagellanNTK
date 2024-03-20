#' @title Main Shiny application
#' @description xxx
#' 
#' @param config A list optionaly containing th following items:
#' * title: xxx
#' * base_URL: xxx
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
MagellanNTK <- function(
    funcs = default.funcs,
    workflow = default.workflow,
    base_URL = default.base.URL,
  verbose = FALSE) {
  
  file_path_ui <- system.file("app/ui.R", package = "MagellanNTK")
  file_path_server <- system.file("app/server.R", package = "MagellanNTK")
  file_path_global <- system.file("app/global.R", package = "MagellanNTK")
  if (!nzchar(file_path_ui) || !nzchar(file_path_server) || !nzchar(file_path_global)) 
    stop("Shiny app not found")
  
   ui <- server <- NULL # avoid NOTE about undefined globals
   source(file_path_ui, local = FALSE)
   source(file_path_server, local = FALSE)
   source(file_path_global, local = FALSE)

   .GlobalEnv$funcs <- funcs
   .GlobalEnv$workflow <- workflow
   .GlobalEnv$base_URL <- base_URL

   on.exit(rm(funcs, envir=.GlobalEnv))
   on.exit(rm(workflow, envir=.GlobalEnv))
   on.exit(rm(base_URL, envir=.GlobalEnv))
   
   # Source workflow files
   dirpath <- file.path(workflow$path, 'R')
   files <- list.files(dirpath, full.names = FALSE)
   for(f in files){
     if(verbose)
       cat('sourcing ', file.path(dirpath, f), '...')
     source(file.path(dirpath, f), local = FALSE, chdir = FALSE)
   }
   
   
   app <- shiny::shinyApp(ui_MagellanNTK, server_MagellanNTK)
  shiny::runApp(app)
  
  
}