#' @title Main Shiny application
#' @description xxx
#' 
#' @param funcs An list containing th following items:
#' * title: xxx
#' * base_URL: xxx
#' @param workflow A list containing the following items:
#' * name : The name of the workflow. If it is in a package, the functions
#' name_ui(), name_server() and name_config() must be be available in its 
#' environment
#' * xxx
#' 
#' @export
#' @examplesIf
#' 
#' # launch without initial config
#' MagellanNTK()
#' 
#' # launch with config
#' funcs <- list(convert = "DaparToolshed::convert",
#' open_dataset = "DaparToolshed::open_dataset",
#' open_demoDataset = "DaparToolshed::open_demoDataset",
#' view_dataset = "DaparViz::view_dataset",
#' infos_dataset = "DaparToolshed::infos_dataset",
#' addDatasets = "DaparToolshed::addDatasets",
#'   keepDatasets = "DaparToolshed::infos_dataset" )
#' 
#' funcs <- list(convert = "MagellanNTK::convert",
#'   open_dataset = "MagellanNTK::open_dataset",
#'   open_demoDataset = "MagellanNTK::open_demoDataset",
#'   view_dataset = "MagellanNTK::view_dataset",
#'   infos_dataset = "MagellanNTK::infos_dataset",
#'   addDatasets = "MagellanNTK::addDatasets",
#'   keepDatasets = "MagellanNTK::infos_dataset")
#' 
#' MagellanNTK(funcs)
#' 
#' 
MagellanNTK <- function(
    funcs = default.funcs,
    verbose = FALSE) {
  
  source_shinyApp_files()

   # Set global variables to global environment
   .GlobalEnv$funcs <- funcs
   #.GlobalEnv$workflow <- workflow
   
   on.exit(rm(funcs, envir=.GlobalEnv))
   #on.exit(rm(workflow, envir=.GlobalEnv))
   
   #source_wf_files(workflow$path)

   # Launch app
   app <- shiny::shinyApp(ui_MagellanNTK, server_MagellanNTK)
  shiny::runApp(app)
  
  
}