#' @title xxx
#' @description xxx
#' @export
demo_workflow <- function(name=NULL){
  if(is.null(name)){
    cat('Available examples are:\n')
    cat("* PipelineA_Process2\n")
    cat("* PipelineA_Process1\n")
    cat("* PipelineA_Process3\n")
    cat("* PipelineA\n")
    return()
  } else {
    data(data_na)
    path <- system.file("extdata/workflow/PipelineA", package = "MagellanNTK")
    files <- list.files(file.path(path, 'R'), full.names = TRUE)
    for(f in files)
      source(f, local = FALSE, chdir = TRUE)
    run_workflow(name, dataIn = data_na)
  }
}