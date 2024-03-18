#' @title xxx
#' @description xxx
#' 
#' @param path.dir xxx
#' @param add_func xxx
#' @param keep_func xxx
#' 
#' 
#' @name create_custom_funcs
#' 
#' 
#' 
NULL


#' @export
#' @rdname create_custom_funcs
#' 
default_add_func <- function() {
default <- "
  #' @title Adds a dataset to the list
  #' @description This function appends a dataset in the list with customization
  if necessary
  #' @param object An instance of type list. Must get TRUE to inherits(object, 'list')
  #' @param dataset xxx
  #' @param name the name to associate to the dataset in the object list
  #' @export
  addDatasets <- function(object, dataset, name) {
  #stopifnot(!is.list(object))
  if (is.null(object))
    setNames(list(dataset), nm = name)
  else
    append(object, setNames(list(dataset), nm = name))
  }
  
  "

return(default)
}


#' @export
#' @rdname create_custom_funcs
#' 
default_keep_func <- function() {
  default <- "
  #' @title Get a subset of the object
  #' @description This function deletes the items not included in the
  range parameter
  #' @param object An instance of type list. Must get TRUE to inherits(object, 'list')
  #' @param range xxx
  #' @export
  #'
  keepDatasets <- function(object, range) {
  #stopifnot(!inherits(object, 'list'))
  if (missing(range))
    stop('Provide range of array to be processed')
  
  if (is.null(object)) {
    return()
    }
  
  if (is.numeric(range))
    range <- names(object)[range]
  
  object[range]
  }
  "
  
  return(default)
}



#' @export
#' @rdname create_custom_funcs
#'
createExtraFunctions <- function(path.dir = '.',
                                 add_func = default_add_func(),
                                 keep_func = default_keep_func()) {
  
  
  # Create template module file
  mod.filename <- file.path(path.dir, "R", "extra_functions.R")
  if (file.exists(mod.filename)) {
    file.remove(mod.filename)
  }
  con <- file(mod.filename, open = "a")
  
  # Write code to file
  writeLines(add_func, con)
  writeLines(keep_func, con)
  
  close(con)
  return("extra_functions.R")
}
