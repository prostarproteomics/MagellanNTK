

#' @title xxx
#' 
#' @description xxx
#' 
#' @param object xxx
#' @param file A `characte(1)` naming the file to save the object.
#' 
#' @export
#' 
Save_Object <- function(object, file) {
  if (is.null(object))
    return()
  if (missing(file))
    return()
  
  saveRDS(object, file = file)
}


#' @title xxx
#' 
#' @description xxx
#' 
#' @param file xxx
#' 
#' @export
#' 
Upload_Object <- function(file) {
  if (missing(file))
    return()
  
  readRDS(file)
}

