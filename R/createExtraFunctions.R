
#' @export
#' @rdname create_template
#'
createExtraFunctions <- function(path = '.') {
  
  # Create template module file
  mod.filename <- file.path(path, "extra_functions.R")
  if (file.exists(mod.filename)) {
    file.remove(mod.filename)
  }
  con <- file(mod.filename, open = "a")
  
  # Write code to file
  write_addDatasets_func(con)
  write_keepDatasets_func(con)
  
  close(con)
  return(mod.filename)
}



#' @rdname create_template
#' 
write_addDatasets_func <- function(con){
  
  code <- "
  #' @title Adds a dataset to the list
  #' @description This function appends a dataset in the list with customization
  if necessary
  #' @param object An instance of type list. Must get TRUE to inherits(object, 'list')
  #' @param dataset xxx
  #' @param name the name to associate to the dataset in the object list
  #' @export
  addDatasets <- function(object, dataset, name) {
  stopifnot(!inherits(object, 'list'))
  if (is.null(object))
    setNames(list(dataset), nm = name)
  else
    append(object, setNames(list(dataset), nm = name))
  }
  
  "
  
  writeLines(code, con)
  
}


#' @rdname create_template
#' 
write_keepDatasets_func <- function(con){
  
  code <- "
  #' @title Get a subset of the object
  #' @description This function deletes the items not included in the
  range parameter
  #' @param object An instance of type list. Must get TRUE to inherits(object, 'list')
  #' @param range xxx
  #' @export
  #'
  keepDatasets <- function(object, range) {
  stopifnot(!inherits(object, 'list'))
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
  writeLines(code, con)
  
}

