

#' @export
default.funcs <- function()
  list(
    convert_dataset = "MagellanNTK::convert_dataset",
    open_dataset = "MagellanNTK::open_dataset",
    open_demoDataset = "MagellanNTK::open_demoDataset",
    view_dataset = "MagellanNTK::view_dataset",
    download_dataset = "MagellanNTK::download_dataset",
    export_dataset = "MagellanNTK::export_dataset",
    build_report = "MagellanNTK::build_report",
    infos_dataset = "MagellanNTK::infos_dataset",
    addDatasets = "MagellanNTK::addDatasets",
    keepDatasets = "MagellanNTK::keepDatasets")


#' @export
default.base.URL <- function()
  system.file('app/md', package = 'MagellanNTK')


#' @export
default.workflow  <- function()
  list(
  name = 'PipelineDemo_Process1',
  path = system.file("workflow/PipelineDemo", package = "MagellanNTK")
)