if(interactive()){
  
  ll.process <- list(
    fullname = 'PipeA_ProcessX',
    mode = "process",
    steps = c("Step 1", "Step 2"),
    mandatory = c(TRUE, FALSE)
    )


file <- createModuleTemplate(ll.process)
source(file)
run_workflow("ProcessX", verbose = TRUE)
unlink(file)
  
  
ll.pipeline <- list(
  mode = "pipeline",
  fullname = 'PipelineA',
  steps = c("Process 1", "Process 2", "Process 3"),
  mandatory = c(TRUE, FALSE, TRUE)
  )
createModuleTemplate(ll.pipeline)
run_workflow("PipelineA", verbose = TRUE)

}