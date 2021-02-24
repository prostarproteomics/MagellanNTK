verbose <- F
redBtnClass <- "btn-danger"
PrevNextBtnClass <- "btn-info"
btn_success_color <- "btn-success"
optionsBtnClass <- "info"

btn_style <- "display:inline-block; vertical-align: middle; padding: 7px"

#' @field global xxxx
global <- list(
  VALIDATED = 1,
  UNDONE = 0,
  SKIPPED = -1
)











rv.process <- reactiveValues(
  status = NULL,
  dataIn = NULL,
  temp.dataIn = NULL,
  current.pos = 1,
  tl.tags.enabled = NULL,
  test = NULL,
  length = NULL,
  config = NULL
)


#' @field dataOut xxx
dataOut <- reactiveValues(
  trigger = 0,
  value = NULL
)

# Check if the rv.process$config is correct
#'
#' @param conf A list containing the rv.process$configuration of the current object.
#' See xxx
#' 
CheckConfig = function(conf){
  if(verbose) cat(paste0('::Checkrv.process$config() from - ', id, '\n\n'))
  passed <- T
  msg <- ""
  if (!is.list(conf)){
    passed <- F
    msg <- c(msg, "'rv.process$config' is not a list")
  }
  if (length(conf)!=3){
    passed <- F
    msg <- c(msg, "The length of 'rv.process$config' is not equal to 4")
  }
  names.conf <- c("name", "steps", "mandatory")
  if (!all(sapply(names.conf, function(x){x %in% names(conf)}))){
    passed <- F
    msg <- c(msg, "The names of elements in 'rv.process$config' must be the following: 'name', 'steps', 'mandatory'")
  }
  if (length(conf$steps) != length(conf$mandatory)){
    passed <- F
    msg <- c(msg, "The length of 'steps' and 'mandatory' must be equal.")
  }
  
  passed <- T
  list(passed=passed,
       msg = msg)
}



#' @description
#' xxxx
#'
Send_Result_to_Caller = function(){
  if(verbose) cat(paste0('::Send_Result_to_Caller() from - ', id, '\n\n'))
  dataOut$trigger <- Timestamp()
  dataOut$value <- rv.process$dataIn
}

#' @description 
#' xxx
#' 
InitializeDataIn = function(){ 
  if(verbose) cat(paste0('InitializeDataIn() from - ', id, '\n\n'))
  rv.process$dataIn <- rv.process$temp.dataIn
}


#' @description
#' Validate a given position. To be used by xxx
#' 
#' @return Nothing.
#' 
ValidateCurrentPos <- function(){
  #browser()
  rv.process$status[rv.process$current.pos] <- global$VALIDATED
  # Either the process has been validated, one can prepare data to be sent to caller
  # Or the module has been reseted
  if (rv.process$current.pos == length(config$steps))
    Send_Result_to_Caller()
}



#' @description
#' Gives the name of the status corresponding to the code (integer).
#'
#' @param name A number
#' 
GetStringStatus = function(name){
  if (name==global$VALIDATED) "Validated"
  else if (name==global$UNDONE) "Undone"
  else if (name==global$SKIPPED) 'Skipped'
}

#' @description 
#' Returns the date and time in timestamp UNIX format.
#' 
Timestamp = function(){ 
  if(verbose) cat(paste0('::Timestamp() from - ', id, '\n\n'))
  as.numeric(Sys.time())
}


#' @description 
#' xxx
#' 
Update_State_Screens = function(){
  if(verbose) cat(paste0('::', 'Update_State_Screens() from - ', id, '\n\n'))
  
  ind.max <- GetMaxValidated_AllSteps()
  #browser()
  if (ind.max > 0) 
    ToggleState_Screens(cond = FALSE, range = 1:ind.max)
  
  
  if (ind.max < rv.process$length){
    # Enable all steps after the current one but the ones
    # after the first mandatory not validated
    firstM <- GetFirstMandatoryNotValidated((ind.max+1):rv.process$length)
    if (is.null(firstM)){
      ToggleState_Screens(cond = TRUE, range = (1 + ind.max):(rv.process$length))
    } else {
      ToggleState_Screens(cond = TRUE, range = (1 + ind.max):(ind.max + firstM))
      if (ind.max + firstM < rv.process$length)
        ToggleState_Screens(cond = FALSE, range = (ind.max + firstM + 1):rv.process$length)
    }
  }
  # browser()
}


#' @description 
#' xxx
#' 
GetMaxValidated_AllSteps = function(){
  if(verbose) cat(paste0( '::', 'GetMaxValidated_AllSteps() from - ', id, '\n\n'))
  val <- 0
  ind <- grep(global$VALIDATED, rv.process$status)
  if (length(ind) > 0) 
    val <-max(ind)
  val
}



#' @description 
#' xxx
#'
#' @param range xxx
#' 
GetFirstMandatoryNotValidated = function(range){
  if(verbose) cat(paste0('::', 'GetFirstMandatoryNotValidated() from - ', id, '\n\n'))
  first <- NULL
  first <- unlist((lapply(range, 
                          function(x){config$mandatory[x] && !rv.process$status[x]})))
  if (sum(first) > 0)
    min(which(first == TRUE))
  else
    NULL
}



#' @description
#' xxx
#'
#' @param cond A number
#' @param range A number
#' 
#' @return Nothing.
#' 
ToggleState_Screens = function(cond, range){
  if(verbose) cat(paste0('::ToggleState_Steps() from - ', id, '\n\n'))
  #browser()
  if (tag.enabled())
    lapply(range, function(x){
      cond <- cond && !(rv.process$status[x] == global$SKIPPED)
      #shinyjs::toggleState(config$steps[x], condition = cond  )
      
      #Send to TL the enabled/disabled tags
      rv.process$tl.tags.enabled[x] <- cond
    })
}

#' @description 
#' xxx
#' 
#' @param i xxx
#' 
Change_Current_Pos = function(i){ rv.process$current.pos <- i}


#' @description
#' et to skipped all steps of the current object
#' 
#' @return Nothing.
#' 
Set_All_Skipped = function(){
  if(verbose) cat(paste0('::', 'Set_All_Skipped() from - ', id, '\n\n'))
  rv.process$status <- setNames(rep(global$SKIPPED, rv.process$length), rv.process$config$steps)
}

#' @description
#' et to skipped all steps of the current object
#' 
#' @return Nothing.
#' 
Discover_Skipped_Steps = function(){
  if(verbose) cat(paste0('::Discover_Skipped_Status() from - ', id, '\n\n'))
  for (i in 1:rv.process$length){
    max.val <- GetMaxValidated_AllSteps()
    if (rv.process$status[i] != global$VALIDATED && max.val > i)
      rv.process$status[i] <- global$SKIPPED
  }
}


observeEvent(tag.enabled(), ignoreNULL = FALSE, ignoreInit = TRUE, {
  # browser()
  if (!isTRUE(tag.enabled()))
    rv.process$tl.tags.enabled <- setNames(rep(FALSE, rv.process$length), rv.process$config$steps)
})



#' @description 
#' Return the UI for a modal dialog with data selection input. If 'failed' is
#' TRUE, then display a message that the previous value was invalid.
#' 
dataModal = function() {
  
  tags$div(id="modal1", 
           modalDialog(
             span(modal_txt),
             footer = tagList(
               actionButton(ns("closeModal"), "Cancel", class='btn-info'),
               actionButton(ns("modal_ok"), "OK")
             )
           )
  )
}




#' @description 
#' xxx
#' 
#' @param cond xxx
#' 
ToggleState_ResetBtn = function(cond){
  if(verbose) cat(paste0( '::', 'ToggleState_ResetBtn(', cond, ')) from - ', id, '\n\n'))
  
  shinyjs::toggleState('rstBtn', condition = cond)
}








#' @description
#' Change current position.
#' 
#' @param direction xxx
#'
NavPage = function(direction) {
  newval <- rv.process$current.pos + direction 
  newval <- max(1, newval)
  newval <- min(newval, length(config$steps))
  rv.process$current.pos <- newval
}

observeEvent(input$prevBtn, ignoreInit = TRUE, {NavPage(-1)})
observeEvent(input$nextBtn, ignoreInit = TRUE, {NavPage(1)})

# Catch new status event

observeEvent(rv.process$status, ignoreInit = T, {
  # https://github.com/daattali/shinyjs/issues/166
  # https://github.com/daattali/shinyjs/issues/25
  if (verbose) cat(paste0('::observe((rv$status) from - ', id, '\n\n'))
  
  Discover_Skipped_Steps()
  Update_State_Screens()
})

observeEvent(input$rstBtn, {
  if (verbose) cat(paste0('::observeEvent(input$rstBtn) from - ', id, '\n\n'))
  showModal(dataModal())
})

observeEvent(input$closeModal, {removeModal() })


observeEvent(req(input$modal_ok > 0), ignoreInit=F, {
  if (verbose) cat(paste0('::observeEvent(req(c(input$modal_ok))) from - ', id, '\n\n'))
  rv.process$local.reset <- input$rstBtn
  #Set_All_Reset()
  BasicReset()
  
  removeModal()
})






