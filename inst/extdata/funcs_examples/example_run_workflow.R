if (interactive()) {
  data(data_na)
  
  path <- system.file("extdata/workflow/PipelineA", package = "MagellanNTK")
  files <- list.files(file.path(path, 'R'), full.names = TRUE)
  for(f in files)
    source(f, local = FALSE, chdir = TRUE)

  # Nothing happens when dataIn is NULL
  run_workflow("PipelineA_Process1", dataIn = NULL)
  
  run_workflow("PipelineA_Process1", dataIn = data_na)

  run_workflow("PipelineA_Process1", dataIn = data.frame(), tl.layout = "v")
  
  run_workflow("PipelineA", dataIn = data_na, tl.layout = c("v", "h"))
    
  run_workflow("PipelineB", tl.layout = c("v", "h"))
}


