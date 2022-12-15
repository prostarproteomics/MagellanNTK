if (interactive()) {
    # dirpath <- system.file("extdata/module_examples", package = "MagellanNTK")
    # for (l in list.files(path = dirpath, pattern = ".R", recursive = TRUE)) {
    #     source(file.path(dirpath, l), local = FALSE)$value
    # }

    path <- system.file("extdata/module_examples", package = "MagellanNTK")
    
    run_workflow("PipelineA_Process1", path = path, verbose = TRUE)

    run_workflow("PipelineA",  path = path, tl.layout = c("h", "h"))
    
    run_workflow("PipelineA", path = path, tl.layout = c("v", "h"))
}