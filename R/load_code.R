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
#' @param recursive xxx
#' 
#' @export
#' 
#' @example inst/extdata/funcs_examples/test_load_code.R
#' 

LoadCode <- function(name, path, recursive = FALSE){
  #Load source files
  if (is.null(path)){
    warning("The parameter 'path' is NULL. Abort.")
    return (NULL)
  }

  fifo <- c()
  
  # Load the code for name module
  fifo <- push_fifo(fifo, name)
  
  while (length(fifo) > 0){
    pull <- pull_fifo(fifo)
    fifo <- pull$fifo
    filename <- paste0(path, '/', pull$value, '.R')
    
    # Source file if exists and check
    if (file.exists(filename)){
      source(filename, local=FALSE)$value
      if (!Found_Mod_Funcs(pull$value))
        return(NULL)
      
      #Get the config of current module
      tmp.config <- do.call(paste0(pull$value, '_conf'), list())
       
      if (tmp.config@mode == 'pipeline'){
        #Parse the children of the current workflow
        ll.steps <- names(tmp.config@steps)
        # for (pattern in c('Description', 'Save'))
        #   if (pattern %in% ll.steps)
        #     ll.steps <- ll.steps[-which(ll.steps==pattern)]
        # 
        ll.steps <- paste0(name, '_', ll.steps)
        for (i in ll.steps)
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
    value = fifo[1]
    )
  
  return(res)
}

