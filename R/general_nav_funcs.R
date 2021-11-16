


#' @title xxx
#' 
#' @description xxx
#' 
#' @param steps.status xxx
#' @param current.pos xxx
#' @param config xxx
#' 
#' @return A `wellPanel`
#' 
Build_SkippedInfoPanel <- function(steps.status, current.pos, config){
  
   req(steps.status[current.pos] == global$SKIPPED)
  process_entirely_skipped <- isTRUE(sum(steps.status) == global$SKIPPED * length(config$steps))
  
  if (process_entirely_skipped){
    # This case appears when the process has been skipped from the
    # pipeline. Thus, it is not necessary to show the info box because
    # it is shown below the timeline of the pipeline
  } else {
    txt <- paste0("This ", config$type, " is skipped so it has been disabled.")
    wellPanel(
      style = "background-color: #7CC9F0; opacity: 0.72; padding: 0px;
                   align: center; vertical-align: center;",
      height = 100,
      width = 300,
      align = "center",
      p(style = "color: black;", paste0("Info: ",txt))
    )
  }
  
}



#' @title xxx
#' 
#' @description xxx
#' 
#' @param ns xxx
#' @param id xxx
#' @param config xxx
#' 
#' @return A `renderUI` function
#' 
Build_EncapsulateScreens_ui <- function(ns, 
                                        id, 
                                        config){
  len <- length(config$ll.UI)
  renderUI({
    tagList(
      lapply(seq_len(len), function(i) {
        if (i==1)
          div(id = ns(config$steps[i]),
              class = paste0('page_', id),
              config$ll.UI[[i]]
          )
        else
          shinyjs::hidden(
            div(id =  ns(config$steps[i]),
                class = paste0('page_', id),
                config$ll.UI[[i]]
            )
          )
      }
      )
    )
  })
}





#' @title xxx
#' 
#' @description xxx
#' 
#' @param pos xxx
#' @param rv xxx
#' 
#' @return xxx
#' 
GetMaxValidated_BeforePos <- function(pos = NULL,
                                      rv){
  
  if (is.null(pos))
    pos <- rv$current.pos
  
  ind.max <- NULL
  indices.validated <- which(rv$steps.status == global$VALIDATED)
  if (length(indices.validated) > 0){
    ind <- which(indices.validated < pos)
    if(length(ind) > 0)
      ind.max <- max(ind)
  }
  
  
  return(ind.max)
}



#' @title xxx
#' 
#' @description xxx
#' 
#' @param steps.status xxx
#' 
#' @return xxx
#' 
GetMaxValidated_AllSteps <- function(steps.status){
  val <- 0
  ind <- grep(global$VALIDATED, steps.status)
  if (length(ind) > 0)
    val <-max(ind)
  
  return(val)
}



#' @title xxx
#' 
#' @description Updates the status of steps in range
#' 
#' @param cond xxx
#' @param range xxx
#' @param steps.status xxx
#' @param is.enabled xxx
#' @param steps.enabled xxx
#' 
#' @return xxx
#' 
ToggleState_Screens <- function(cond, 
                                range, 
                                is.enabled,
                                rv){
  
  if (isTRUE(is.enabled))
    rv$steps.enabled[range] <- cond && !(rv$steps.status[range] == global$SKIPPED)
    
    return(rv$steps.enabled)
}


#' @title xxx
#' 
#' @description xxx
#' 
#' @param name xxx
#' 
#' @return xxx
#' 
GetStringStatus <- function(name){
  string <- if (name == global$VALIDATED) 'Validated'
  else if (name == global$UNDONE) 'Undone'
  else if (name == global$SKIPPED) 'Skipped'
  
  return(string)
}



#' @title xxx
#' 
#' @description xxx
#' 
#' @param dataIn xxx
#' 
#' @return xxx
#' 
Send_Result_to_Caller = function(dataIn){
  return(
    list(trigger = Timestamp(),
         value = dataIn
         )
  )
}


#' @title xxx
#' 
#' @description xxx
#' 
#' @param direction xxx
#' @param current.pos xxx
#' @param len xxx
#' 
#' @return xxx
#' 
NavPage <- function(direction, current.pos, len) {
  newval <- current.pos + direction
  newval <- max(1, newval)
  newval <- min(newval, len)
  current.pos <- newval
  
  return(current.pos)
}



#' @title xxx
#' 
#' @description xxx
#' 
#' @param ns xxx
#' @param mode xxx
#' 
#' @return A tag div for ui
#' 
dataModal <- function(ns, mode) {
  # Used to show an explanation for the reset feature whether the navigation mode is 'process' nor 'pipeline'.
  template_reset_modal_txt <- 'This action will reset this mode. The input dataset will be the output of the last previous
validated process and all further datasets will be removed'
  
  tags$div(id = 'modal1',
           modalDialog(
             span(gsub('mode', mode, template_reset_modal_txt)),
             footer = tagList(
               actionButton(ns('closeModal'), 'Cancel', class='btn-info'),
               actionButton(ns('modal_ok'), 'OK')
             )
           )
  )
}




#' @title xxx
#' 
#' @description xxx
#' 
#' @param steps.status xxx
#' 
#' @return xxx
#' 
Discover_Skipped_Steps <- function(steps.status){
  for (i in seq_len(length(steps.status))){
    max.val <- GetMaxValidated_AllSteps(steps.status)
    if (steps.status[i] != global$VALIDATED && max.val > i)
      steps.status[i] <- global$SKIPPED
  }
  
  return(steps.status)
}




#' @title xxx
#' 
#' @description xxx
#' 
#' @param steps.status xxx
#' @param tag xxx
#' 
#' @return xxx
#'
All_Skipped_tag <- function(steps.status, tag){
  steps.status <- setNames(rep(tag, length(steps.status)), steps.status)
  
  return(steps.status)
  
}

#' @title xxx
#' 
#' @description xxx
#' 
#' @param range xxx
#' @param rv
#' 
#' @return xxx
#'
GetFirstMandatoryNotValidated <- function(range, 
                                          rv){
  res <- NULL
  first <- NULL
  first <- unlist((lapply(range,
                          function(x){rv$config$mandatory[x] && !rv$steps.status[x]})))
  res <- if (sum(first) > 0)
    min(which(first == TRUE))
  else
    NULL
  
  return(res)
}



#' @title xxx
#' 
#' @description xxx
#' 
#' @param current.pos xxx
#' @param steps xxx
#' 
#' @return xxx
#' 
CurrentStepName <- function(current.pos, steps){
  return(steps[current.pos])
}


#' @title xxx
#' 
#' @description xxx
#' 
#' @param is.skipped xxx
#' @param is.enabled xxx
#' @param rv xxx
#' 
Update_State_Screens = function(is.skipped,
                                is.enabled,
                                rv){
  
  len <- length(rv$steps.status)
  
  if (isTRUE(is.skipped)){
    steps.enabled <- ToggleState_Screens(cond = FALSE,
                                         range = seq_len(len),
                                         is.enabled = is.enabled,
                                         rv = rv)
  } else {
    
    # Ensure that all steps before the last validated one are disabled
    ind.max <- GetMaxValidated_AllSteps(rv$steps.status)
    if (ind.max > 0)
      steps.enabled <- ToggleState_Screens(cond = FALSE, 
                                           range = seq_len(ind.max),
                                           is.enabled = is.enabled,
                                           rv = rv)
    
    if (ind.max < len){
      # Enable all steps after the current one but the ones
      # after the first mandatory not validated
      firstM <- GetFirstMandatoryNotValidated(range = (ind.max+1):len,
                                              rv = rv)
      if (is.null(firstM)){
        steps.enabled <- ToggleState_Screens(cond = TRUE, 
                                             range = (1 + ind.max):(len),
                                             is.enabled = is.enabled,
                                             rv = rv)
      } else {
        steps.enabled <- ToggleState_Screens(cond = TRUE, 
                                             range = (1 + ind.max):(ind.max + firstM),
                                             is.enabled = is.enabled,
                                             rv = rv )
        if (ind.max + firstM < len)
          steps.enabled <- ToggleState_Screens(cond = FALSE,
                                               range = (ind.max + firstM + 1):len,
                                               is.enabled = is.enabled,
                                               rv = rv )
      }
    }
    
    ToggleState_NavBtns(rv = rv)
  }
  
  return(steps.enabled)
}


#' @title xxx
#' 
#' @description xxx
#' 
#' @param current.pos xxx
#' @param len xxx
#' 
#' @return NA
#' 
ToggleState_NavBtns <- function(rv){
  
  # If the cursor is not on the first position, show the 'prevBtn'
  shinyjs::toggleState(id = 'prevBtn', condition = rv$current.pos != 1)
  
  # If the cursor is set before the last step, show the 'nextBtn'
  shinyjs::toggleState(id = 'nextBtn', condition = rv$current.pos < length(rv$config$steps))
}


#' @title xxx
#' 
#' @description xxx
#' 
#' @param cond xxx
#' 
#' @return NA
#' 
ToggleState_ResetBtn <- function(cond){
  shinyjs::toggleState('rstBtn', condition = cond)
}




#' @title xxx
#' 
#' @description xxx
#' 
#' @param mode xxx
#' @param rv xxx
#' 
#' @return A list of four items:
#' * dataIn:  xx
#' * dataOut: xxx
#' * current.pos: xxx
#' * steps.status: xxx
#'
# LocalReset <- function(mode,
#                        rv
#                        ){
#   L
#   #rv$temp.dataIn <- NULL
#   
#   # The cursor is set to the first step
#   current.pos <- 1
#   
#   # The status of the steps are reinitialized to the default configuration of the process
#   steps.status <- setNames(rep(global$UNDONE, length(rv$config$steps)), rv$config$steps)
#   
#   # If the current module is a pipeline type (node and not leaf),
#   # then sent to its children the information that they must reset themself
#   resetChildren <- NULL
#   if (mode == 'pipeline')
#     resetChildren <- ResetChildren(range = range,
#                                    resetChildren = rv$resetChildren
#     )
#   
#   
#   # Return the NULL value as dataset
#   dataOut <- Send_Result_to_Caller(rv$dataIn)
#   
#   return(
#     list(dataIn = dataIn,
#          dataOut = dataOut,
#          current.pos = current.pos,
#          steps.status = steps.status,
#          resetChildren = resetChildren
#          )
#   )
# }



