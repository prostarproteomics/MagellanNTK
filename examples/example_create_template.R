if(interactive()){
  
  conf.process <- Config(
  name = 'ProcessX',
  mode = "process",
  steps = c("Step 1", "Step 2"),
  mandatory = c(TRUE, FALSE)
    )


file <- createModuleTemplate(conf.process)
source(file)
run_workflow("ProcessX", verbose = TRUE)
unlink(file)
  
  
conf.pipeline <- Config(
  mode = "pipeline",
  name = 'PipelineA',
  steps = c("Process 1", "Process 2", "Process 3"),
  mandatory = c(TRUE, FALSE, TRUE)
  )
createModuleTemplate(conf.pipeline)
run_workflow("PipelineA", verbose = TRUE)

}