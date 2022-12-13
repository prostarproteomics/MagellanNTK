if (interactive()) {
    dirpath <- system.file("extdata/module_examples", package = "Magellan")
    for (l in list.files(path = dirpath, pattern = ".R", recursive = TRUE)) {
        source(file.path(dirpath, l), local = FALSE)$value
    }

    run_workflow("Process1", verbose = TRUE)

    run_workflow("PipelineA", tl.layout = c("h", "h"))
}