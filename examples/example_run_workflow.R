if (interactive()) {
    path <- system.file("extdata/module_examples", package = "MagellanNTK")
    
    run_workflow("PipelineA_Process1", path = path, verbose = TRUE)

    run_workflow("PipelineA",  path = path, tl.layout = c("h", "h"))
    
    run_workflow("PipelineB", path = path, tl.layout = c("v", "h"))
}