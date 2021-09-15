#' @title 
#' xxx
#' 
#' @description xxx
#' 
#' @param data xxx
#' 
#' @export
#' 
# Send_Result_to_Caller = function(){
#   if(verbose) cat(paste0('::Send_Result_to_Caller() from - ', id, '\n\n'))
#   dataOut$trigger <- Timestamp()
#   dataOut$value <- rv.process$dataIn
# }

#' #' @description xxxx
#' #' @return xxx
#' #' @examples
#' #' @export
#' Send_Result_to_Caller = function(data){
#'   list(trigger = as.numeric(Sys.time()),
#'        value = data
#'   )
#' }


#' @title 
#' xxx
#' @description 
#' Returns the date and time in timestamp UNIX format.
#' 
#' @return NA
#' 
#' @export
#' 
Timestamp <- function(){ 
  if(verbose) cat(paste0('::Timestamp() from - ', id, "\n\n"))
  as.numeric(Sys.time())
}


#' @title 
#' xxx
#' 
#' @description xxx
#' 
#' @return NA
#' 
AddItemToDataset <- function(dataset, name){
  addAssay(dataset, 
           dataset[[length(dataset)]], 
           name=name)
}

