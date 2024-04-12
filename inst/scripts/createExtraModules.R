#' @title Creates the template extar module
#' @description Creates an exemplary extra module to be included into
#' a step. 
#' Int the code that will be produced, the parts that are not commented 
#' are mandatory ans should not be modified.
#' The commented parts are just for example purpose and need to be
#' customize to your function
#' @param name The name of the module. This will lead to two functions 
#' 'name_ui()' and 'name_server()'
#' @param path xxx
#' 
#' @name createExtraModule
#' 
NULL


#' @export
#' @rdname createExtraModule
#'
createExtraModule <- function(name, path = '.') {
  
  # Create template module file
  module.name <- file.path(path, "R", paste0(name, ".R"))
  if (file.exists(module.name)) {
    file.remove(module.name)
  }
  con <- file(module.name, open = "a")
  
  # Write code to file
  code <- gsub('NAME', name, default_extraModule_code())
  writeLines(code, con)
  
  close(con)
  return(paste0(name, ".R"))
}



#' @rdname createExtraModule
#' @export
default_extraModule_code <- function(){
  file <- system.file('scripts', 'create_Extra_Module.R', package = 'MagellanNTK')
  code <- readLines(file)
  code <- paste(code, collapse = '\n') 
  return(code)

}