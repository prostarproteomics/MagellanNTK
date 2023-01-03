if (interactive()) {
    path <- system.file("extdata/module_examples", package = "MagellanNTK")
    data(data_na)
    run_workflow("PipelineA_Process1", path = path, verbose = TRUE)

    run_workflow("PipelineA", dataIn = data_na, path = path, tl.layout = c("h", "h"),verbose = TRUE)
    
    run_workflow("PipelineB", path = path, tl.layout = c("v", "h"))
}