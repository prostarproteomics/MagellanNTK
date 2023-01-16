if (interactive()) {
  data(data_na)
  
  path <- system.file("extdata/module_examples", package = "MagellanNTK")
    run_workflow("PipelineA_Process1", dataIn = NULL, path = path)

    run_workflow("PipelineA", dataIn = data_na, path = path, tl.layout = c("v", "h"))
    
    run_workflow("PipelineB", path = path, tl.layout = c("v", "h"))
}