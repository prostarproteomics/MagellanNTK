#' @title xxx
#' @description xxx
#' 
#' @param mode xxx
#' @param fullname xxx
#' @param path xxx
#' @param rawText xxx
#' 
#' @export
Create_md_file <- function(mode, fullname, path, rawText){
  ###
  ### Create the Description md file
  ###
  
  # In this particularly case, on add the prefix '_Description' to the name of the
  # process. Here, it is considered as an independant step of the pipeline and
  # all steps names are built on the same pattern : the name of the workflow
  # suffixed with the name of the step
  if (mode == 'pipeline')
    fullname <- paste0(fullname, '_Description')
  
  
  desc.dir <- file.path(path, 'md')
  if (!dir.exists(desc.dir)) 
    dir.create(desc.dir)          
  
  md.file <- paste0(fullname, ".md")
  desc.filename <- file.path(desc.dir, md.file)
  if (file.exists(desc.filename)) {
    file.remove(desc.filename)
  }
  con.desc <- file(desc.filename, open = "a")
  
  
  if (is.null(rawText))
  rawText <- "
  ## Overview
  
  This page describes the workflow.
  
  "
  writeLines(rawText, con.desc)
  
  
  close(con.desc)
  
  return(md.file)
  
}