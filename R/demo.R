

launch_demo <- function(){
  
  data(lldata)

  return(
    list(
      funcs = default.funcs(),
      current.obj = lldata,
      workflow.name = 'PipelineA',
      workflow.path = system.file('workflow/PipelineA', package='MagellanNTK')
      )
    )
}