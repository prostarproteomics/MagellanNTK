

#' @title 
#' xxx
#' 
#' @description xxx
#' 
#' Check if the rv$config is correct
#'
#' @param config A list containing the rv$configuration of the current object.
#' See xxx
#' 
#' @return A list of two items:
#' * `passed`: A boolean that indicates if the config is correct or not.
#' * `msg`: A `character(1)` as message.
#' 
#' @export
#' 
#' @author Samuel Wieczorek
#' 
#' @examples
#' conf <- list(parent = "pipeline",
#' name = "process",
#' steps = c('Description', "Step 1", "Step 2", "Save"),
#' mandatory = c(TRUE, TRUE, FALSE, TRUE)
#' )
#' CheckConfig(conf)
#' 

CheckConfig <- function(config){
  passed <- TRUE
  msg <- ""
  
  if (!is.list(config)){
    passed <- FALSE
    msg <- c(msg, "'rv$config' is not a list")
  }
  
  names.config <- c("parent", "name", "steps", "mandatory")
  if (!all(sapply(names.config, function(x){x %in% names(config)}))){
    passed <- FALSE
    msg <- c(msg, "The names of elements in 'rv$config' must be the following: 'parent', 'name', 'steps', 'mandatory'")
  }
  if (length(config$steps) != length(config$mandatory)){
    passed <- FALSE
    msg <- c(msg, "The length of 'steps' and 'mandatory' must be equal.")
  }
  
  list(passed = passed,
       msg = msg)
}



#' @title Check source code of a module process
#' 
#' @description Check if the source code of a process module is correct
#' 
#' @details xxxx
#' xxx
#' xxxx
#'
#' @param sourcefile xxx
#' 
#' @return A list of two items:
#' * `passed`: A boolean that indicates if the config is correct or not.
#' * `msg`: A `character(1)` as message.
#' 
#' @export
#' 
#' @author Samuel Wieczorek
#' 
#' @examples
#' f <- system.file("scripts/module_examples", "example_module_PipelineA_Process1.R", 
#' package="Magellan")
#' CheckProcessCode(f)
#' 
CheckProcessCode <- function(sourcefile = NULL){
  
  s.code <- readLines(file(sourcefile))
  # remove every white space from the file so as to make easier the search of keywords
  
  
}