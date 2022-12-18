.path <- system.file('extdata/module_examples', package='MagellanNTK')

.name <- 'PipelineA_Process1'
LoadCode(name = .name, path = .path)

.path <- system.file('extdata/module_examples', package='MagellanNTK')
.name <- 'PipelineA'
LoadCode(name = .name, path = .path)