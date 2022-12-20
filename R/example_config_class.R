#' Example of a generic process
#' @export
#' 
gen.proc <- function()
    Config(
  name = 'PipelineA_Process1',
  mode = 'process',
  steps = c('Step 1', 'Step 2'),
  mandatory = c(TRUE, FALSE)
)



#' Example of a generic pipeline
#' @export
#'
gen.pipe <- function()
  Config(
  name = 'Pipe1_PipelineA',
  mode = 'pipeline',
  steps = c('Process 1', 'Process 2'),
  mandatory = c(TRUE, FALSE)
)

#' Example of a root pipeline (process has no parent)
#' @export
#'
root.pipe <- function()
  Config(
  mode = 'pipeline',
  name = 'PipelineA',
  steps = c('Process1', 'Process2', 'Process3'),
  mandatory = c(FALSE, FALSE, TRUE)
)


#' Example of a description module (process has no steps)
#' @export
#' 
desc.proc <- function()
  Config(
  name = 'PipelineA_Description',
  mode = 'process',
  steps = '',
  mandatory = ''
)

