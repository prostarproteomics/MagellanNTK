# Example of a generic process
generic.proc <- Config(
  name = 'Process1',
  parent = 'PipelineA',
  mode = 'process',
  steps = c('Step 1', 'Step 2'),
  mandatory = c(TRUE, FALSE)
  )


# Example of a generic pipeline
generic.pipe <- Config(
  name = 'PipelineA',
  mode = 'pipeline',
  steps = c('Process 1', 'Process 2'),
  mandatory = c(TRUE, FALSE)
  )

# Example of a root pipeline
root.pipe <- Config(
  mode = 'pipeline',
  name = 'PipelineA',
  parent = '',
  steps = c('Process1', 'Process2', 'Process3'),
  mandatory = c(FALSE, FALSE, TRUE)
)


description.process <- Config(
  name = 'Description',
  parent = 'PipelineA',
  mode = 'process',
  steps = '',
  mandatory = ''
)


generic.proc
generic.pipe
root.pipe
description.process