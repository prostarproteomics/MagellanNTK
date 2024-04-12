#' @title xxx
#' 
#' @description
#' From: https://stackoverflow.com/questions/56942384/error-in-clipboard-on-x11-requires-that-the-display-envvar-be-configured
#' 
#' @param x xxx
#' @param sep xxx
#' @param ... xxx
#' 
#' 
#' @export
#' 
clipboard <- function(x, sep = '\t', ...){
  con <- pipe("xclip -selection clipboard -i  -display :1", open="w") # note the 1 here
  #writeChar(x, con)  # for strings
  write.table(x, con, sep = sep, ...) # for table
  close(con)
}
