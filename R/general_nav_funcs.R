
GetCode_ToggleState_Screens <- function(){
  
  # This function changes the state (enabled, disabled) of the steps in the process
  # The parameter 'cond' is the new state
  # The parameter 'range' corresponds to the range of steps to update
  code.string <- "ToggleState_Screens = function(cond, range){
  if(verbose) cat(crayon::yellow(paste0(id, '::ToggleState_Screens(cond = ', cond, ', range = ', paste0(range, collapse = ' '), ')\n\n')))
  #browser()
  if (isTRUE(is.enabled()))
    lapply(range, function(x){
      cond <- cond && !(rv$steps.status[x] == Magellan::global$SKIPPED)
      
      #Send to TL the enabled/disabled tags
      rv$steps.enabled[x] <- cond
    })
  }
  
  "
  code.string 
}


GetCode_ToggleState_NavBtns <- function(){
  
  code.string <- "
  ToggleState_NavBtns = function(){
    if(verbose) cat(crayon::yellow(paste0(id, '::ToggleState_NavBtns()\n\n')))
    
    # If the cursor is not on the first position, show the 'prevBtn'
    cond <-  rv$current.pos != 1
    shinyjs::toggleState(id = 'prevBtn', condition = cond)
    
    # If the cursor is set before the last step, show the 'nextBtn'
    cond <- rv$current.pos < rv$length
    shinyjs::toggleState(id = 'nextBtn', condition = cond)
  }
  
  "

  code.string 
  
}


GetCode_ActionOn_NewPosition <- function(){
  
  code.string <- "
  ActionOn_NewPosition = function(){
    if(verbose) cat(crayon::yellow(paste0(id, '::ActionOn_NewPosition()\n\n')))
    
    # Send dataset to child process only if the current position is enabled
    #if(rv$steps.enabled[rv$current.pos])
    PrepareData2Send()
    #browser()
    # If the current step is validated, set the child current position to the last step
    if (rv$steps.status[rv$current.pos] == Magellan::global$VALIDATED)
      rv.child$position[rv$current.pos] <- paste0('last_', Timestamp())
  }
  
  "

  code.string 
  
}



GetCode_LocalReset <- function(){
 code.string <- " 
 LocalReset = function(){
    if(verbose) cat(crayon::yellow(paste0(id, '::LocalReset()\n\n')))
    #browser()
    rv$dataIn <- NULL
    #rv$temp.dataIn <- NULL
    
    # The cursor is set to the first step
    rv$current.pos <- 1
    
    # The status of the steps are reinitialized to the default configuration of the process
    rv$steps.status <- setNames(rep(Magellan::global$UNDONE, rv$length), 
                                rv$config$steps)
    
    # If the current module is a pipeline type (node and not leaf),
    # then sent to its children the information that they must reset themself
    if (nav.mode == 'pipeline')
      ResetChildren()
    
    # Return the NULL value as dataset
    Send_Result_to_Caller()
    #dataOut <- reactive({Send_Result_to_Caller(rv$dataIn)})
  }
  
  "

  code.string 
}




GetCode_NavPage <- function(){
  # @title Change current position.
  # 
  # @description
  # Change current position.
  # 
  # @param direction xxx
  #
  code.string <- "
  NavPage = function(direction) {
    newval <- rv$current.pos + direction 
    newval <- max(1, newval)
    newval <- min(newval, rv$length)
    rv$current.pos <- newval
  }
  
"
  code.string 
}



GetCode_dataModal <- function(){
# @title 
# xxx
# 
# @description 
# Return the UI for a modal dialog with data selection input. If 'failed' is
# TRUE, then display a message that the previous value was invalid.
# 
  
code.string <- 
  
"
dataModal = function() {
  
  tags$div(id='modal1', 
           modalDialog(
             span(gsub('nav.mode', nav.mode, modal_txt)),
             footer = tagList(
               actionButton(ns('closeModal'), 'Cancel', class='btn-info'),
               actionButton(ns('modal_ok'), 'OK')
             )
           )
  )
}

"

code.string 
}


GetCode_Discover_Skipped_Steps <- function(){
# @title 
# Discover new skipped steps.
# @description
# This function looks for new skipped steps after the vector status has been updated.
#
# @return Nothing.
# 
  code.string <- 
  "
  Discover_Skipped_Steps = function(){
  if(verbose) cat(crayon::yellow(paste0(id, '::Discover_Skipped_Steps()\n\n')))
  for (i in seq_len(rv$length)){
    max.val <- GetMaxValidated_AllSteps()
    if (rv$steps.status[i] != Magellan::global$VALIDATED && max.val > i)
      rv$steps.status[i] <- Magellan::global$SKIPPED
  }
}

"

code.string 
}




GetCode_Unskip_All_Steps <- function(){
 code.string <- "
 
 Unskip_All_Steps = function(){
    if(verbose) cat(crayon::yellow(paste0(id, '::Unskip_All_Steps()\n\n')))
    rv$steps.status <- setNames(rep(Magellan::global$UNDONE, rv$length), 
                                rv$config$steps)
    Update_State_Screens()
  }
  
 " 

  code.string 
}


GetCode_Set_All_Skipped <- function(){
  # @title 
  # Set all steps of the current process to skipped
  # @description
  # Set to skipped all steps of the current object
  # 
  # @return Nothing.
  # 
  code.string <- "
  
  Set_All_Skipped = function(){
    if(verbose) cat(crayon::yellow(paste0(id, '::Set_All_Skipped()\n\n')))
    rv$steps.status <- setNames(rep(Magellan::global$SKIPPED, rv$length), 
                                rv$config$steps)
  }
  
"
  code.string 
}




GetCode_GetFirstMandatoryNotValidated <- function(){
  # @title 
  # Get first mandatory step not yet validated
  # @description 
  # This function analyses the vectors mandatory and status to find the first
  # step which is mandatory and not yet validated in a range of integers
  #
  # @param range xxx
  # 
  code.string <- "
  
  GetFirstMandatoryNotValidated = function(range){
    if(verbose) cat(crayon::yellow(paste0(id, '::GetFirstMandatoryNotValidated()\n\n')))
    
    first <- NULL
    first <- unlist((lapply(range, 
                            function(x){rv$config$mandatory[x] && !rv$steps.status[x]})))
    if (sum(first) > 0)
      min(which(first == TRUE))
    else
      NULL
  }
  
"
  code.string 
}


GetCode_Change_Current_Pos <- function(){
  # @title 
  # Set current position
  # @description 
  # Change the cursor position to a given position
  # 
  # @param i An integer that corresponds to the new position
  #
  code.string <- "
  Change_Current_Pos = function(i){ rv$current.pos <- i}
  
"
  code.string 
  
}


GetCode_ToggleState_ResetBtn <- function(){
  
  # @title xxx
  # 
  # @description 
  # xxx
  # 
  # @param cond xxx
  # 
  code.string <- "
  
  ToggleState_ResetBtn = function(cond){
    if(verbose) cat(crayon::yellow(paste0(id, '::ToggleState_ResetBtn(', cond, '))\n\n')))
    
    shinyjs::toggleState('rstBtn', condition = cond)
  }
  
"
  code.string 
  
}


GetCode_GetMaxValidated_BeforePos <- function(){
  
  # @title 
  # Get the last validated step before a given position
  # @description
  # This function analyzes the reactive variable rv$steps.status
  # to find the indice of the last validated step among all steps before
  # the current position (parameter pos set to NULL) or a given position (
  # parameter pos set to an integer).
  #
  # @param pos xxx
  # 
  # @return Nothing
  #
  code.string <- "
  
  GetMaxValidated_BeforePos = function(pos = NULL){
    if(verbose) cat(crayon::yellow(paste0(id, 'GetMaxValidated_BeforePos()\n\n')))
    
    if (is.null(pos))
      pos <- rv$current.pos
    
    ind.max <- NULL
    indices.validated <- which(rv$steps.status == Magellan::global$VALIDATED)
    if (length(indices.validated) > 0){
      ind <- which(indices.validated < pos)
      if(length(ind) > 0)
        ind.max <- max(ind)
    }
    ind.max
  }
  
"
  code.string 
  
  
}



GetCode_GetMaxValidated_AllSteps <- function(){
  # @title 
  # Get the last validated step among all the steps
  # @description 
  # This function analyzes the reactive variable rv$steps.status
  # to find the indice of the last validated step among all steps
  # 
  code.string <- "
  
  GetMaxValidated_AllSteps = function(){
    if(verbose) cat(crayon::yellow(paste0( id, '::GetMaxValidated_AllSteps()\n\n')))
    val <- 0
    ind <- grep(Magellan::global$VALIDATED, rv$steps.status)
    if (length(ind) > 0) 
      val <-max(ind)
    val
  }
  
"
  code.string 
}


GetCode_EncapsulateScreens <- function(){
  code.string <- "
  
  EncapsulateScreens = function(){
    tagList(
      lapply(seq_len(rv$length), function(i) {
        if (i==1)
          div(id = ns(rv$config$steps[i]),
              class = paste0('page_', id),
              rv$config$ll.UI[[i]]
          )
        else
          shinyjs::hidden(
            div(id =  ns(rv$config$steps[i]),
                class = paste0('page_', id),
                rv$config$ll.UI[[i]]
            )
          )
      }
      )
    )
  }
  
"
  code.string 
}


GetCode_GetStringStatus <- function(){
  # @title 
  # xxx
  # @description
  # Converts the numerical code for status into string.
  #
  # @param name A number
  # 
  code.string <- "
  
  GetStringStatus = function(name){
    if (name == Magellan::global$VALIDATED) 'Validated'
    else if (name == Magellan::global$UNDONE) 'Undone'
    else if (name == Magellan::global$SKIPPED) 'Skipped'
  }
  
  "
code.string
  
}


GetCode_Update_State_Screens <- function(){
  # This function is updated each time the status vector is changed. It is 
  # used to decide which steps must be enabled or disabled w.r.t the new
  # status vector value. 
  # The behaviour is the following:
  # * All the steps before the last validated one are disabled
  # * all the steps before a undone mandatory step are enable and the ones
  # after this mandatory step are disabled
  # * xxx 
  #' 
  code.string <- "
  
  Update_State_Screens = function(){
    if(verbose) cat(crayon::yellow(paste0(id, '::Update_State_Screens()\n\n')))
    
    if (isTRUE(is.skipped())){
      ToggleState_Screens(cond = FALSE, range = seq_len(rv$length))
    } else {
      
      # Ensure that all steps before the last validated one are disabled
      ind.max <- GetMaxValidated_AllSteps()
      if (ind.max > 0)
        ToggleState_Screens(cond = FALSE, range = seq_len(ind.max))
      
      if (ind.max < rv$length){
        # Enable all steps after the current one but the ones
        # after the first mandatory not validated
        firstM <- GetFirstMandatoryNotValidated((ind.max+1):rv$length)
        if (is.null(firstM)){
          ToggleState_Screens(cond = TRUE, range = (1 + ind.max):(rv$length))
        } else {
          ToggleState_Screens(cond = TRUE, range = (1 + ind.max):(ind.max + firstM))
          if (ind.max + firstM < rv$length)
            ToggleState_Screens(cond = FALSE, range = (ind.max + firstM + 1):rv$length)
        }
      }
      ToggleState_NavBtns()
    }
  }

  "
  code.string
  
}


GetCode_Send_Result_to_Caller <- function(){
  # #' @title 
  # #' xxx
  # #' 
  # #' @description xxx
  # #' 
  # #' @export
  # #' 
  code.string <- "
  
  Send_Result_to_Caller = function(){
    if(verbose) cat(crayon::yellow(paste0(id, '::Send_Result_to_Caller()\n\n')))
    dataOut$trigger <- Timestamp()
    dataOut$value <- rv$dataIn
  }

  "
  code.string
}


GetCode_Update_Data2send_Vector <- function(){
  code.string <- "
  
  Update_Data2send_Vector = function(){
    # One only update the current position because the vector has been entirely
    # initialized to NULL so the other processes are already ready to be sent
    ind.last.validated <- GetMaxValidated_BeforePos()
    if (is.null(ind.last.validated))
      data <- rv$temp.dataIn
    else
      data <- Keep_Datasets_from_Object(object = rv$dataIn,
                                        range = seq_len(ind.last.validated + rv$original.length -1)
      )
    return(data)
  }
  
"
code.string
}


