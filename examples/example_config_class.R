config.proc <- Config(
  name = 'Process1',
  parent = 'PipelineA',
  mode = 'process',
  steps = c('Step 1', 'Step 2'),
  mandatory = c(TRUE, FALSE),
  path = system.file('extdata/module_examples/', package='MagellanNTK')
  )



config.pipe <- Config(
  name = 'PipelineA',
  mode = 'pipeline',
  steps = c('Process 1', 'Process 2'),
  mandatory = c(TRUE, FALSE),
  path = system.file('extdata/module_examples/', package='MagellanNTK')
  )
