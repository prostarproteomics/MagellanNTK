#' @title xxx
#' @description xxx
#' @export
demo_workflow <- function(){
  data(data_na)
  
  path <- system.file("extdata/workflow/PipelineA", package = "MagellanNTK")
  files <- list.files(file.path(path, 'R'), full.names = TRUE)
  for(f in files)
    source(f, local = FALSE, chdir = TRUE)
  run_workflow("PipelineA_Process1", dataIn = data_na)
}