

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
  
  names.config <- c("mode", "steps", "mandatory")
  if (!all(sapply(names.config, function(x){x %in% names(config)}))){
    passed <- FALSE
    msg <- c(msg, "The names of elements in 'config' must be the following: 'mode', 'steps', 'mandatory'")
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
#' file <-system.file("scripts/module_examples", "mod_Process1.R", package="Magellan")
#' s <- CleanSourceCode(file)
#' 
CleanSourceCode <- function(file = NULL){
  
  source <- readLines(file)
  
  source  <- unlist(lapply(source, function(x) gsub(" ", "", x) ))
  
  # Replace " by '
  source  <- unlist(lapply(source, function(x) gsub("\"", "'", x) ))
  
  
  # Remove empty lines
  source <- source[-which(source=="")]
  
  # Remove comments lines
  res <- which(unlist(lapply(source, function(x) unlist(gregexpr("#", x))[1]==1)))
  
  source <- source[-res]

  # Concatenate in one vector
  source <- paste0(source, collapse = "")
  
  source
  
}


#' @title xxx
#' 
#' @param text xxx
#' @param openPos xxx
#' 
#' @examples
#' text <- "myfunc <- function(a, b){ return (a+b)}"
#' posParam <- 19
#' FindClosingParenthesis(text, posParam)
#' 
FindClosingParenthesis <- function(text, openPos){
  
  closePos <- openPos
  counter <- 1
  while (counter > 0) {
    c <- substr(text, closePos + 1, closePos + 1)
    if (c == '(')
      counter <- counter +1
     else if (c == ')') 
        counter <- counter - 1
    closePos <- closePos + 1
  
  }
  return(closePos)
}





#' @title xxx
#' 
#' @description Analyze the source code to extract the config variable
#' 
#' @param source The complete source code of the process module
#' 
#' @return A list containing the configuration of the module
#' 
#' @author Samuel Wieczorek
#' 
#' @importFrom stringi stri_locate
#' 
GetConfig <- function(s){
  
  config <- NULL
  
  keyword <- 'config<-list('
  len <- nchar(keyword)
  start.index <- stri_locate(pattern = keyword, s, fixed = TRUE)[1]
  end.index <- FindClosingParenthesis(s, start.index + len)
  
  config.string <- substr(s, start.index, end.index)
  
  # Get mode value
  keyword <- 'mode='
  len <- nchar(keyword)
  mode.start <- stri_locate(pattern = keyword, config.string, fixed = TRUE)[1]
  tmp <- substr(config.string, mode.start + len, nchar(config.string))
  end_g <- stri_locate_all(pattern = substr(tmp, 1, 1),
                           tmp,
                           fixed = TRUE)[[1]][2, 1]
  
  mode <- substr(tmp, 2, end_g - 1)
  
  
  # Get steps values
  
  steps.start <- stri_locate(pattern = 'steps=', config.string, fixed = TRUE)[1]
  
  
  
  # Get mandatory slot values
  mandatory.start <- stri_locate(pattern = 'mandatory=', config.string, fixed = TRUE)[1]
  
  
  config <- list(
    mode = mode,
    steps = steps,
    mandatory = mandatory
  )
  
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