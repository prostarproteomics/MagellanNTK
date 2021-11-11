
#' @title xxx
#' 
#' @description xxx
#' 
#' @param steps.status xxx
#' @param current.pos xxx
#' @param config xxx
#' 
Build_SkippedInfoPanel <- function(steps.status, current.pos, config){
  
  renderUI({
    current_step_skipped <- steps.status[current.pos] == global$SKIPPED
  req(current_step_skipped)
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
})
  
}


#' @title xxx
#' 
#' @description xxx
#' 
#' @param steps xxx
#' @param ll.UI xxx
#' 
Build_EncapsulateScreens_ui <- function(steps, ll.UI){
  len <- length(ll.UI)
  renderUI({
  tagList(
    lapply(seq_len(len), function(i) {
      if (i==1)
        div(id = ns(steps[i]),
            class = paste0('page_', id),
            ll.UI[[i]]
        )
      else
        shinyjs::hidden(
          div(id =  ns(steps[i]),
              class = paste0('page_', id),
              ll.UI[[i]]
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
#' @param cond xxx
#' 
ToggleState_ResetBtn <- function(cond){
  if(verbose) cat(crayon::yellow(paste0(id, '::ToggleState_ResetBtn(', cond, '))\n\n')))
  
  shinyjs::toggleState('rstBtn', condition = cond)
}



#' @title xxx
#' 
#' @description xxx
#' 
#' @param current.pos xxx
#' @param pos xxx
#' @param steps.status xxx
#' 
GetMaxValidated_BeforePos <- function(current.pos, 
                                      pos = NULL,
                                      steps.status
                                      ){
  if(verbose) cat(crayon::yellow(paste0(id, 'GetMaxValidated_BeforePos()\n\n')))
  
  if (is.null(pos))
    pos <- current.pos
  
  ind.max <- NULL
  indices.validated <- which(rv$steps.status == global$VALIDATED)
  if (length(indices.validated) > 0){
    ind <- which(indices.validated < pos)
    if(length(ind) > 0)
      ind.max <- max(ind)
  }
  ind.max
}



#' @title xxx
#' 
#' @description xxx
#' 
#' @param steps.status xxx
#' 
GetMaxValidated_AllSteps <- function(steps.status){
  if(verbose) cat(crayon::yellow(paste0( id, '::GetMaxValidated_AllSteps()\n\n')))
  val <- 0
  ind <- grep(global$VALIDATED, steps.status)
  if (length(ind) > 0)
    val <-max(ind)
  val
}



#' @title xxx
#' 
#' @description xxx
#' 
#' @param cond xxx
#' @param range xxx
#' @param steps.status xxx
#' @param is.enabled xxx
#' 
ToggleState_Screens = function(cond, 
                               range, 
                               steps.status, 
                               is.enabled){
  if(verbose) cat(crayon::yellow(paste0(id, '::ToggleState_Screens(cond = ', cond, ', range = ', paste0(range, collapse = ' '), ')\n\n')))
  
  if (isTRUE(is.enabled))
    lapply(range, function(x){
      cond <- cond && !(steps.status[x] == global$SKIPPED)
      
      #Send to TL the enabled/disabled tags
      steps.enabled[x] <- cond
    })
  
  steps.enabled
}



#' @title xxx
#' 
#' @description xxx
#' 
#' @param current.pos xxx
#' @param len xxx
#' 
ToggleState_NavBtns = function(current.pos, 
                               len){
  if(verbose) cat(crayon::yellow(paste0(id, '::ToggleState_NavBtns()\n\n')))
  
  # If the cursor is not on the first position, show the 'prevBtn'
  cond <-  current.pos != 1
  shinyjs::toggleState(id = 'prevBtn', condition = cond)
  
  # If the cursor is set before the last step, show the 'nextBtn'
  cond <- current.pos < len
  shinyjs::toggleState(id = 'nextBtn', condition = cond)
}




#' @title xxx
#' 
#' @description xxx
#' 
#' @param steps xxx
#' @param ll.UI xxx
#' 
Update_State_Screens = function(is.skipped,
                                len,
                                steps.status, 
                                is.enabled,
                                current.pos,
                                ...
                                ){
  if(verbose) cat(crayon::yellow(paste0(id, '::Update_State_Screens()\n\n')))
  
  if (isTRUE(is.skipped)){
    ToggleState_Screens(cond = FALSE, 
                        range = seq_len(len),
                        steps.status = steps.status, 
                        is.enabled = is.enabled)
  } else {
    
    # Ensure that all steps before the last validated one are disabled
    ind.max <- GetMaxValidated_AllSteps(steps.status)
    if (ind.max > 0)
      ToggleState_Screens(cond = FALSE, 
                          range = seq_len(ind.max),
                          steps.status = steps.status, 
                          is.enabled = is.enabled)
    
    if (ind.max < len){
      # Enable all steps after the current one but the ones
      # after the first mandatory not validated
      firstM <- GetFirstMandatoryNotValidated(range = (ind.max+1):len,
                                              mandatory = mandatory,
                                              steps.status = steps.status
      )
      if (is.null(firstM)){
        ToggleState_Screens(cond = TRUE, 
                            range = (1 + ind.max):(len),
                            steps.status = steps.status, 
                            is.enabled = is.enabled
                            )
      } else {
        ToggleState_Screens(cond = TRUE, 
                            range = (1 + ind.max):(ind.max + firstM),
                            steps.status = steps.status, 
                            is.enabled = is.enabled
                            )
        if (ind.max + firstM < len)
          ToggleState_Screens(cond = FALSE,
                              range = (ind.max + firstM + 1):len,
                              steps.status = steps.status, 
                              is.enabled = is.enabled
                              )
      }
    }
    ToggleState_NavBtns(current.pos = current.pos,
                        len = len
                        )
  }
}


GetStringStatus <- function(name){
  string <- if (name == global$VALIDATED) 'Validated'
  else if (name == global$UNDONE) 'Undone'
  else if (name == global$SKIPPED) 'Skipped'
  
  string
}


Send_Result_to_Caller = function(dataIn){
  if(verbose) cat(crayon::yellow(paste0(id, '::Send_Result_to_Caller()\n\n')))
  
  list(trigger = Timestamp(),
       value = dataIn
       )
}



LocalReset <- function(steps, nav.mode){
  if(verbose) cat(crayon::yellow(paste0(id, '::LocalReset()\n\n')))
  #browser()
  dataIn <- NULL
  #rv$temp.dataIn <- NULL
  
  # The cursor is set to the first step
  current.pos <- 1
  
  # The status of the steps are reinitialized to the default configuration of the process
  steps.status <- setNames(rep(global$UNDONE, length(steps)), steps)
  
  # If the current module is a pipeline type (node and not leaf),
  # then sent to its children the information that they must reset themself
  if (nav.mode == 'pipeline')
    rv$resetChildren <- ResetChildren()
  
  # Return the NULL value as dataset
  dataOut <- Send_Result_to_Caller(dataIn)
  #dataOut <- reactive({Send_Result_to_Caller(rv$dataIn)})
  
  list(dataIn = dataIn,
       dataOut = dataOut,
       current.pos = current.pos,
       steps.status = steps.status
       )
}


NavPage <- function(direction, current.pos, len) {
  newval <- rcurrent.pos + direction
  newval <- max(1, newval)
  newval <- min(newval, len)
  current.pos <- newval
  
  current.pos
}


dataModal <- function(nav.mode) {
  # Used to show an explanation for the reset feature whether the navigation mode is 'process' nor 'pipeline'.
  template_reset_modal_txt <- 'This action will reset this nav.mode. The input dataset will be the output of the last previous
validated process and all further datasets will be removed'
  
  tags$div(id='modal1',
           modalDialog(
             span(gsub('nav.mode', nav.mode, template_reset_modal_txt)),
             footer = tagList(
               actionButton(ns('closeModal'), 'Cancel', class='btn-info'),
               actionButton(ns('modal_ok'), 'OK')
             )
           )
  )
}











Discover_Skipped_Steps <- function(len,
                                   steps.status
                                   ){
  if(verbose) cat(crayon::yellow(paste0(id, '::Discover_Skipped_Steps()\n\n')))
  for (i in seq_len(len)){
    max.val <- GetMaxValidated_AllSteps(steps.status)
    if (steps.status[i] != global$VALIDATED && max.val > i)
      steps.status[i] <- global$SKIPPED
  }
  
  steps.status
}





Unskip_All_Steps <- function(len,
                             steps.status
                             ){
  if(verbose) cat(crayon::yellow(paste0(id, '::Unskip_All_Steps()\n\n')))
  steps.status <- setNames(rep(global$UNDONE, len), steps.status)
  
  steps.status
}



Set_All_Skipped <- function(len,
                            steps.status
                            ){
  if(verbose) cat(crayon::yellow(paste0(id, '::Set_All_Skipped()\n\n')))
  steps.status <- setNames(rep(global$SKIPPED, len), steps.status)
  
  steps.status
}





GetFirstMandatoryNotValidated <- function(range, 
                                          mandatory,
                                          steps.status
                                          ){
  if(verbose) cat(crayon::yellow(paste0(id, '::GetFirstMandatoryNotValidated()\n\n')))
  
  res <- NULL
  first <- NULL
  first <- unlist((lapply(range,
                          function(x){mandatory[x] && !steps.status[x]})))
  res <- if (sum(first) > 0)
    min(which(first == TRUE))
  else
    NULL
}


CurrentStepName <- function(current.pos, steps){
  cat(crayon::yellow(paste0('::GetCurrentStepName() from - ', id, '\n')))
  steps[current.pos]
}
