
GetCode_SkippedInfoPanel_UI <- function(){
  
  code <- "
  
  output$SkippedInfoPanel <- renderUI({
      if (verbose) cat(paste0(class(self)[1], \"::output$SkippedInfoPanel from - \", self$id, \"\n\n\"))
      
      current_step_skipped <- rv$steps.status[rv$current.pos] == global$SKIPPED
      req(current_step_skipped)
      process_entirely_skipped <- isTRUE(sum(rv$steps.status) == global$SKIPPED * rv$length)
      
      if (process_entirely_skipped){
        # This case appears when the process has been skipped from the
        # pipeline. Thus, it is not necessary to show the info box because
        # it is shown below the timeline of the pipeline
      } else {
        txt <- paste0(\"This \", rv$config$type, \" is skipped so it has been disabled.\")
        wellPanel(
          style = \"background-color: #7CC9F0; opacity: 0.72; padding: 0px;
                   align: center; vertical-align: center;\",
          height = 100,
          width = 300,
          align = \"center\",
        p(style = \"color: black;\", paste0(\"Info: \",txt))
    )
}
})

"

code
      }


GetCode_observeEvent_dataIn <- function(){
  
  code <- "
  
  # Catch a new value on the parameter 'dataIn()' variable, sent by the caller. This value 
    # may be NULL or contain a dataset.
    # The first action is to store the dataset in the temporary variable 
    # temp.dataIn. Then, two behaviours:
    # * if the variable is NULL. xxxx
    # * if the variable contains a dataset. xxx
    #
    observeEvent(dataIn(), ignoreNULL = FALSE, ignoreInit = FALSE,{
      #observe({
      if (verbose) cat(yellow(paste0(id, \"::observe(dataIn())\n\n\")))
      #browser()
      isolate({
        # A new value on dataIn() means a new dataset sent to the process
        Change_Current_Pos(1)
        
        # Get the new dataset in a temporary variable
        rv$temp.dataIn <- dataIn()
        #ActionOn_New_DataIn() # Used by class pipeline
        
        # The mode pipeline is a node and has to send
        # datasets to its children
        if (nav.mode == 'pipeline')
          if (is.null(rv$dataIn))
            PrepareData2Send() # Used by class pipeline
        
        
        
        if(is.null(dataIn())){# The process has been reseted or is not concerned
          cat(blue('In observe(dataIn()) : dataIn() is NULL\n\n'))
          # Disable all screens of the process
          ToggleState_Screens(FALSE, seq_len(rv$length))
        } else { # A new dataset has been loaded
          cat(blue('In observe(dataIn()) : dataIn() is not NULL\n\n'))
          # Update the different screens in the process
          Update_State_Screens()
        }
        
        # Update the initial length of the dataset with the length
        # of the one that has been received
        rv$original.length <- length(dataIn())
        
        
        # Enable the first screen
        ToggleState_Screens(TRUE, 1)
      })
    })
    
    "
  
  code
  
  }

GetCode_observeEvent_isEnabled <- function(){
  
  code <- "
  
  # Disable an entire process
    # @description 
    # The parameter is.enabled() is updated by the caller and tells the process
    # if it is enabled or disabled (remote action from the caller)
    observeEvent(is.enabled(), ignoreNULL = TRUE, ignoreInit = TRUE, {
      if (verbose) cat(yellow(paste0(id, '::is.enabled()\n\n')))
      if (isTRUE(is.enabled())){
        Update_State_Screens()
      } else {
        rv$steps.enabled <- setNames(rep(is.enabled(), rv$length), 
                                     rv$config$steps)
      }
    })
  
  "
  
  code
}

GetCode_observeEvent_stepsStatus <- function(){
  
  code <- "
  
  # Catch new status event
    observeEvent(rv$steps.status, ignoreInit = TRUE, {
      # https://github.com/daattali/shinyjs/issues/166
      # https://github.com/daattali/shinyjs/issues/25
      if (verbose) cat(yellow(paste0(id, '::observeEvent(rv$steps.status)\n\n')))
      
      Discover_Skipped_Steps()
      Update_State_Screens()
      if (rv$steps.status[rv$length] == global$VALIDATED){
        rv$current.pos <- rv$length
        Send_Result_to_Caller()
      }
    })
  
  "
  
  code
}


GetCode_observeEvent_isSkipped <- function(){
  
  code <- "
  
  #' @title
    #' Skipping an entire process
    #' @description 
    #' The parameter is.skipped() is set by the caller and tells the process
    #' if it is skipped or not (remote action from the caller)
    observeEvent(is.skipped(), ignoreNULL = FALSE, ignoreInit = TRUE,{
      # Catches a new value on the remote parameter `Reset`. A TRUE value indicates
      # that the caller program wants this module to reset itself.
      if (verbose) cat(yellow(paste0(id, '::observeEvent(is.skipped()). Value = ', is.skipped(), '\n\n')))
      if (isTRUE(is.skipped()))
        Set_All_Skipped()
      else{
        Unskip_All_Steps()
        Update_State_Screens()
      }
    })"
  
  code
}

GetCode_observeEvent_remoteReset <- function(){
  
  code <- "
  
  observeEvent(remoteReset(), ignoreInit = TRUE, {
      # Catches a new value on the remote parameter `Reset`. A TRUE value indicates
      # that the caller program wants this module to reset itself. 
      if (verbose) cat(yellow(paste0(id, '::observeEvent(remoteReset()). Value = ', remoteReset(), '\n\n')))
      LocalReset()
    })
    
    "
  code
  
  }
    
    
    
GetCode_observeEvent_rstBtn <- function(){
  
  code <- "
  
  observeEvent(input$rstBtn, ignoreInit = TRUE, {
      # Catches a new value on the remote parameter `Reset`. A TRUE value indicates
      # that the caller program wants this module to reset itself. 
      if (verbose) cat(yellow(paste0('::observeEvent(input$rstBtn) from - ', id, '\n\n')))
      showModal(dataModal())
    })
    
    "
  
  code
  
  }


GetCode_observeEvent_modal_ok <- function(){
  
  code <- "
  
  observeEvent(input$modal_ok, ignoreInit=FALSE, ignoreNULL = TRUE, {
  # Catches a clic on the `Ok` button of the modal for resetting a module
  if (verbose) cat(yellow(paste0(id, '::observeEvent(input$modal_ok)\n\n')))
  LocalReset()
  removeModal()
})

"

  code
  
  }




#' 
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

#' 
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




GetCode_NavPage_Managment <- function(){
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
  
  observeEvent(input$prevBtn, ignoreInit = TRUE, {NavPage(-1)})
  observeEvent(input$nextBtn, ignoreInit = TRUE, {NavPage(1)})
    

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

observeEvent(input$closeModal, {removeModal() })
    

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

#' 
#' 
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


