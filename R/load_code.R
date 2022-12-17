#' @title Load workflows functions
#' 
#' @description This function xx
#' 
#' @param name A `character()` to indicate the name of the workflow. It can be
#' either a single string (which represents a workflow without a parent) or two
#' strings separated by '_' (in that case, it is a workflow with a parent 
#' workflow).
#' @param path A `character()` to indicate the directory where to find the source
#' files of all workflows. This directory must also contains a 'md' directory
#' that contains the md files corresponding to the workflows.
#' 
#' @example examples/test_load_code.R
#' 
#' @export
#' 

LoadCode <- function(name, path, recursive = FALSE){
  #Load source files
  if (is.null(path)){
    warning("The parameter 'path' is NULL. Abort.")
    return (NULL)
  }

  fifo <- c()
  
  # Load the code for name module
  fname <- paste0(path, '/', name, '.R')
  fifo <- push_fifo(fifo, fname)
  while (length(fifo) > 0){

    if (file.exists(fname)){
      source(fname, local=FALSE)$value
      if (!Found_Mod_Funcs(name))
        return(NULL)
      fifo <- pull_fifo(fifo)$fifo
      if(isTRUE(recursive)){
        tmp.config <- do.call(paste0(name, '_conf'), list())
        for (i in tmp.config@steps.source.file)
          fifo <- push_fifo(fifo, i)
      }
    }
  }

}

#' @title Push item in a FIFO
#' 
#' @param fifo A `vector` of strings
#' @param value A string
#' 
#' @return A `vector` of strings
#' 
#' @export
#' 
push_fifo <- function(fifo, value){
  fifo[1+length(fifo)] <- value
  return(fifo)
}


#' @title Push item in a FIFO
#' 
#' @param fifo A `vector` of strings
#' 
#' @return A `list(2)` of two items:
#' * xxx
#' * xxx
#' 
#' @export
#' 
pull_fifo <- function(fifo){
  if (length(fifo)==0){
    warning('Nothing to pull: fifo is empty')
  return(fifo)
  }
  
  res <- list(
    fifo = fifo[-1],
    value = fifo[length(fifo)]
    )
  
  return(res)
}

