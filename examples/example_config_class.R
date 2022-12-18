config.proc <- Config(
  name = 'Process1',
  parent = 'PipelineA',
  mode = 'process',
  steps = c('Step 1', 'Step 2'),
  mandatory = c(TRUE, FALSE)
  )



config.pipe <- Config(
  name = 'PipelineA',
  mode = 'pipeline',
  steps = c('Process 1', 'Process 2'),
  mandatory = c(TRUE, FALSE)
  )


conf <- Config(
  mode = 'pipeline',
  name = 'PipelineA',
  parent = '',
  # List of all steps of the process
  # Here, each step is a workflow
  steps = c('Process1', 'Process2', 'Process3'),
  # A vector of boolean indicating if the steps are mandatory or not.
  mandatory = c(FALSE, FALSE, TRUE)
)


conf.desription <- Config(
  name = 'Description',
  parent = 'PipelineA',
  mode = 'process',
  steps = '',
  mandatory = ''
)


