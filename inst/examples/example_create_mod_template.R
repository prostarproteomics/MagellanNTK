conf.process <- Config(
  name = 'ProcessX',
  mode = "process",
  steps = c('Description', "Step 1", "Step 2", "Save"),
  mandatory = c(TRUE, TRUE, FALSE, TRUE),
  path_to_md_dir = '.'
  )
createModuleTemplate(conf.process)
run_workflow("ProcessX", verbose = TRUE)
  
  
conf.pipeline <- Config(
  mode = "pipeline",
  name = 'PipelineA',
  steps = c('Description', "Process 1", "Process 2", "Process 3"),
  mandatory = c(TRUE, TRUE, FALSE, TRUE),
  path_to_md_dir = system.file("module_examples/md/", package = "Magellan")
)
createModuleTemplate(conf.pipeline)
run_workflow("PipelineA", verbose = TRUE)