


  observeEvent_steps.status <- function(){
  # https://github.com/daattali/shinyjs/issues/166
  # https://github.com/daattali/shinyjs/issues/25
  cat(crayon::yellow(paste0(id, '::observeEvent(rv$steps.status)\n\n')))
  rv$steps.status <- Discover_Skipped_Steps(rv$steps.status)
  
  # rv$steps.enabled <- Update_State_Screens(is.skipped = is.skipped(),
  #                                          steps.status = rv$steps.status, 
  #                                          steps.enabled = rv$steps.enabled, 
  #                                          is.enabled = is.enabled(),
  #                                          current.pos = rv$current.pos,
  #                                          mandatory = rv$config$mandatory
  # )
  rv$steps.enabled <- Update_State_Screens(is.skipped = is.skipped(),
                                           is.enabled = is.enabled(),
                                           rv = rv)
  
  if (rv$steps.status[rv$length] == global$VALIDATED){
    # Set current position to the last one
    rv$current.pos <- rv$length
    
    # Send result
    dataOut <- Send_Result_to_Caller(rv$dataIn)
  }
  
  }

