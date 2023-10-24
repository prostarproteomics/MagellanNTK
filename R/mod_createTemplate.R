
#' @title xxx
#' @description xxx
#' @param name xxx
#' @rdname example_workflow
#' @export
Tools_Templates <- function(){
  data(data_na)
  path <- system.file("extdata/workflow/Tools", package = "MagellanNTK")
  files <- list.files(file.path(path, 'R'), full.names = TRUE)
  for(f in files)
    source(f, local = FALSE, chdir = TRUE)
  run_workflow('Tools_Templates', dataIn = data_na)
}