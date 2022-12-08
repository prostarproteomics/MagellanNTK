#' @title Example dataset builder for examples in this package.
#' 
#' @description xxx
#' 
#' @examples 
#' Build_example_dataset()
#' 
#' @export
#' 
Build_example_dataset <- function(){
  data <- list(
    original = iris[1:10,],
    processed_1 = iris[1:10,]
  )
  
  return(data)
}