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

#' @description xxxx
#' @return xxx
#' @examples
#' Send_Result_to_Caller(3)
Send_Result_to_Caller = function(data){
  list(trigger = as.numeric(Sys.time()),
       value = data
  )
}