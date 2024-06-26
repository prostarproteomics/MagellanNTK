# Example of a generic process
generic.proc <- Config(
  fullname = 'PipelineDemo_Process1',
  mode = 'process',
  steps = c('Step 1', 'Step 2'),
  mandatory = c(TRUE, FALSE)
  )


# Example of a generic pipeline
generic.pipe <- Config(
  fullname = 'Pipe1_PipelineDemo',
  mode = 'pipeline',
  steps = c('Process 1', 'Process 2'),
  mandatory = c(TRUE, FALSE)
  )

# Example of a root pipeline (process has no parent)
root.pipe <- Config(
  mode = 'pipeline',
  fullname = 'PipelineDemo',
  steps = c('Process1', 'Process2', 'Process3'),
  mandatory = c(FALSE, FALSE, TRUE)
)


# Example of a description module (process has no steps)
description.process <- Config(
  fullname = 'PipelineDemo_Description',
  mode = 'process',
  steps = '',
  mandatory = ''
)


generic.proc
generic.pipe
root.pipe
description.process