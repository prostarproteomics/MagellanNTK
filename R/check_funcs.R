

#' @title xxx
#' 
#' @description xxx
#' 
#' @param id xxx
#' 
Found_Mod_Funcs <- function(id){
  server.func <- paste0('mod_', id, '_server')
  server.exists = exists(server.func, mode='function')
  
  ui.func <- paste0('mod_', id, '_ui')
  ui.exists = exists(ui.func, mode='function')
  
  if (!server.exists)
    warning(paste0("Cannot found ", server.func, '()'))
  
  if (!ui.exists)
    warning(paste0("Cannot found ", ui.func, '()'))
  
  return(server.exists && ui.exists)
}

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



#' @title Clean source code before syntax analysis
#' xxx
#' 
#' @description xxx
#' 
#' Check if the rv$config is correct
#'
#' @param source A vector in which each element is a line read from source code file
#' 
#' @return NA
#' @author Samuel Wieczorek
#' 
#' @examples
#' conf <- list(parent = "pipeline",
#' name = "process",
#' steps = c('Description', "Step 1", "Step 2", "Save"),
#' mandatory = c(TRUE, TRUE, FALSE, TRUE)
#' )
#' 
#' 
CleanSourceCode <- function(source = NULL){
  
  #toto <- readLines(f)
  
  source1  <- unlist(lapply(source, function(x) gsub(" ", "", x) ))
  
  
  # Remove empty lines
  source2 <- source1[-which(source1=="")]
  
  # Remove comments lines
  res <- which(unlist(lapply(source2, function(x) unlist(gregexpr("#", x))[1]==1)))
  
  source3 <- source2[-res]
  
  
  # Remove white spaces
  source1  <- unlist(lapply(source, function(x) gsub(" ", "", x) ))
  
  # Replace " by '
  source1  <- unlist(lapply(source1, function(x) gsub("\"", "'", x) ))
  
  # Remove empty lines
  source2 <- source1[-which(source1=="")]
  
  # Remove comments lines
  res <- which(unlist(lapply(source2, function(x) unlist(gregexpr("#", x))[1]==1)))
  source3 <- source2[-res]
  
  # Concatenate in one vector
  source4 <- paste0(source3, collapse = "")
  
  
  
}



#' @title xxx
#' 
#' @description xxx
#' 
#' @param source xxx
#' 
#' @author Samuel Wieczorek
#' 
GetConfigCode <- function(source){
  
  config <- NULL
  
  return(config)
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