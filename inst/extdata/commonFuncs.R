verbose <- FALSE

global = list(VALIDATED = 1,
              SKIPPED = -1,
              UNDONE = 0
)

default_pos =list(VALIDATED = 1,
                  SKIPPED = 1,
                  UNDONE = 1
)

redBtnClass = "btn-danger"
PrevNextBtnClass = "btn-info"
btn_success_color = "btn-success"
optionsBtnClass = "info"

btn_style = "display:inline-block; vertical-align: middle; padding: 7px"







# 
# output$EncapsulateScreens <- renderUI({
#   tagList(
#     lapply(seq_len(length(rv.process$config$ll.UI)), function(i) {
#       if (i==1)
#         div(id = ns(rv.process$config$steps[i]),
#             class = paste0("page_", id),
#             rv.process$config$ll.UI[[i]]
#         )
#       else
#         shinyjs::hidden(
#           div(id =  ns(rv.process$config$steps[i]),
#               class = paste0("page_", id),
#               rv.process$config$ll.UI[[i]]
#           )
#         )
#     }
#     )
#   )
#   
#   
# })

# Reactive values that will be used to output the current dataset when 
# the last step is validated
dataOut = reactiveValues(
  trigger = NULL,
  value = NULL
)


#These reactive values are specific to this instance of mod_nav_process_server
rv.process <- reactiveValues(
  #' @field proc contains the return value of the process module that has been called 
  proc = NULL,
  
  #' @field status A booelan vector which contains the status (validated,
  #' skipped or undone) of the steps
  steps.status = NULL,
  
  #' @field dataIn Contains the dataset passed by argument to the module server
  dataIn = NULL,
  
  #' @field temp.dataIn This variable is used to serves as a tampon between 
  #' the input of the module and the functions. 
  temp.dataIn = NULL,
  
  #' @field steps.enabled Contains the value of the parameter 'is.enabled'
  steps.enabled = NULL,
  
  #' @field current.pos Stores the current cursor position in the timeline and 
  #' indicates which of the process' steps is active
  current.pos = 1,
  
  length = NULL,
  config = NULL
)



#' @title 
#' xxx
#' 
#' @description xxx
#' 
# Check if the rv.process$config is correct
#'
#' @param conf A list containing the rv.process$configuration of the current object.
#' See xxx
#' 
CheckConfig = function(conf){
  if(verbose) cat(crayon::yellow(paste0(id, '::CheckConfig()\n\n')))
  passed <- TRUE
  msg <- ""
  if (!is.list(conf)){
    passed <- FALSE
    msg <- c(msg, "'rv.process$config' is not a list")
  }
  if (length(conf)!=3){
    passed <- FALSE
    msg <- c(msg, "The length of 'rv.process$config' is not equal to 4")
  }
  names.conf <- c("name", "steps", "mandatory")
  if (!all(sapply(names.conf, function(x){x %in% names(conf)}))){
    passed <- FALSE
    msg <- c(msg, "The names of elements in 'rv.process$config' must be the following: 'name', 'steps', 'mandatory'")
  }
  if (length(conf$steps) != length(conf$mandatory)){
    passed <- FALSE
    msg <- c(msg, "The length of 'steps' and 'mandatory' must be equal.")
  }
  
  passed <- TRUE
  list(passed = passed,
       msg = msg)
}

# #' @title 
# #' xxx
# #' 
# #' @description xxx
# #' 
# #' @export
# #' 
Send_Result_to_Caller = function(){
  if(verbose) cat(crayon::yellow(paste0(id, '::Send_Result_to_Caller()\n\n')))
  dataOut$trigger <- Timestamp()
  dataOut$value <- rv.process$dataIn
}




# Catch a new value on the parameter 'dataIn()' variable, sent by the caller. This value 
# may be NULL or contain a dataset.
# The first action is to store the dataset in the temporary variable 
# temp.dataIn. Then, two behaviours:
# * if the variable is NULL. xxxx
# * if the variable contains a dataset. xxx
#
observeEvent(dataIn(), ignoreNULL = FALSE, ignoreInit = FALSE,{
#observe({
  if (verbose) cat(crayon::yellow(paste0(id, "::observe(dataIn())\n\n")))
  #browser()
  isolate({
    # A new value on dataIn() means a new dataset sent to the process
    Change_Current_Pos(1)
    
    # Get the new dataset in a temporary variable
    rv.process$temp.dataIn <- dataIn()
    #ActionOn_New_DataIn() # Used by class pipeline
    
    # The mode pipeline is a node and has to send
    # datasets to its children
    if (nav.mode == 'pipeline')
      if (is.null(rv.process$dataIn))
        PrepareData2Send() # Used by class pipeline
    
    
    
    if(is.null(dataIn())){# The process has been reseted or is not concerned
      cat(crayon::blue('In observe(dataIn()) : dataIn() is NULL\n\n'))
      # Disable all screens of the process
      ToggleState_Screens(FALSE, seq_len(rv.process$length))
    } else { # A new dataset has been loaded
      cat(crayon::blue('In observe(dataIn()) : dataIn() is not NULL\n\n'))
      # Update the different screens in the process
      Update_State_Screens()
    }
    
    # Update the initial length of the dataset with the length
    # of the one that has been received
    rv.process$original.length <- length(dataIn())
    
    
    # Enable the first screen
    ToggleState_Screens(TRUE, 1)
  })
})



# This function is updated each time the status vector is changed. It is 
# used to decide which steps must be enabled or disabled w.r.t the new
# status vector value. 
# The behaviour is the following:
# * All the steps before the last validated one are disabled
# * all the steps before a undone mandatory step are enable and the ones
# after this mandatory step are disabled
# * xxx 
#' 
Update_State_Screens = function(){
  if(verbose) cat(crayon::yellow(paste0(id, '::Update_State_Screens()\n\n')))
  
  if (isTRUE(is.skipped())){
    ToggleState_Screens(cond = FALSE, range = seq_len(rv.process$length))
  } else {
    
    # Ensure that all steps before the last validated one are disabled
    ind.max <- GetMaxValidated_AllSteps()
    if (ind.max > 0)
      ToggleState_Screens(cond = FALSE, range = seq_len(ind.max))
    
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
    ToggleState_NavBtns()
  }
}




EncapsulateScreens = function(){
tagList(
  lapply(seq_len(rv.process$length), function(i) {
    if (i==1)
      div(id = ns(rv.process$config$steps[i]),
          class = paste0("page_", id),
          rv.process$config$ll.UI[[i]]
      )
    else
      shinyjs::hidden(
        div(id =  ns(rv.process$config$steps[i]),
            class = paste0("page_", id),
            rv.process$config$ll.UI[[i]]
        )
      )
  }
  )
)
}

# 
# Send_Result_to_Caller = function(data){
#   list(trigger = as.numeric(Sys.time()),
#        value = data
#   )
# }


# #' @description
# #' Validate a given position. To be used by xxx
# #' 
# #' @return Nothing.
# #' 
# ValidateCurrentPos <- function(){
#   browser()
#   #rv.process$steps.status[rv.process$current.pos] <- global$VALIDATED
#   
#   
#   # Either the process has been validated, one can prepare data to be sent to caller
#   # Or the module has been reseted
#   if (rv.process$current.pos == rv.process$length)
#     Send_Result_to_Caller()
# }



#' @title 
#' xxx
#' @description
#' Converts the numerical code for status into string.
#'
#' @param name A number
#' 
GetStringStatus = function(name){
  if (name == global$VALIDATED) "Validated"
  else if (name == global$UNDONE) "Undone"
  else if (name == global$SKIPPED) 'Skipped'
}

utils::globalVariables("GetStringStatus")




#' @title 
#' Get the last validated step among all the steps
#' @description 
#' This function analyzes the reactive variable rv.process$steps.status
#' to find the indice of the last validated step among all steps
#' 
GetMaxValidated_AllSteps = function(){
  if(verbose) cat(crayon::yellow(paste0( id, '::GetMaxValidated_AllSteps()\n\n')))
  val <- 0
  ind <- grep(global$VALIDATED, rv.process$steps.status)
  if (length(ind) > 0) 
    val <-max(ind)
  val
}




#' @title 
#' Get the last validated step before a given position
#' @description
#' This function analyzes the reactive variable rv.process$steps.status
#' to find the indice of the last validated step among all steps before
#' the current position (parameter pos set to NULL) or a given position (
#' parameter pos set to an integer).
#'
#' @param pos xxx
#' 
#' @return Nothing
#'
GetMaxValidated_BeforePos = function(pos = NULL){
  if(verbose) cat(crayon::yellow(paste0(id, 'GetMaxValidated_BeforePos()\n\n')))
  
  if (is.null(pos))
    pos <- rv.process$current.pos
  
  ind.max <- NULL
  indices.validated <- which(rv.process$steps.status == global$VALIDATED)
  if (length(indices.validated) > 0){
    ind <- which(indices.validated < pos)
    if(length(ind) > 0)
      ind.max <- max(ind)
  }
  ind.max
}



#' @title 
#' Get first mandatory step not yet validated
#' @description 
#' This function analyses the vectors mandatory and status to find the first
#' step which is mandatory and not yet validated in a range of integers
#'
#' @param range xxx
#' 
GetFirstMandatoryNotValidated = function(range){
  if(verbose) cat(crayon::yellow(paste0(id, '::GetFirstMandatoryNotValidated()\n\n')))

  first <- NULL
  first <- unlist((lapply(range, 
                          function(x){rv.process$config$mandatory[x] && !rv.process$steps.status[x]})))
  if (sum(first) > 0)
    min(which(first == TRUE))
  else
    NULL
}




#' @title 
#' Set current position
#' @description 
#' Change the cursor position to a given position
#' 
#' @param i An integer that corresponds to the new position
#' 
Change_Current_Pos = function(i){ rv.process$current.pos <- i}


#' @title 
#' Set all steps of the current process to skipped
#' @description
#' Set to skipped all steps of the current object
#' 
#' @return Nothing.
#' 
Set_All_Skipped = function(){
  if(verbose) cat(crayon::yellow(paste0(id, '::Set_All_Skipped()\n\n')))
  rv.process$steps.status <- setNames(rep(global$SKIPPED, rv.process$length), 
                                rv.process$config$steps)
}


Unskip_All_Steps = function(){
  if(verbose) cat(crayon::yellow(paste0(id, '::Unskip_All_Steps()\n\n')))
    rv.process$steps.status <- setNames(rep(global$UNDONE, rv.process$length), 
                                                     rv.process$config$steps)
    Update_State_Screens()
}



#' @title 
#' Discover new skipped steps.
#' @description
#' This function looks for new skipped steps after the vector status has been updated.
#' 
#' @return Nothing.
#' 
Discover_Skipped_Steps = function(){
  if(verbose) cat(crayon::yellow(paste0(id, '::Discover_Skipped_Steps()\n\n')))
  for (i in seq_len(rv.process$length)){
    max.val <- GetMaxValidated_AllSteps()
    if (rv.process$steps.status[i] != global$VALIDATED && max.val > i)
      rv.process$steps.status[i] <- global$SKIPPED
  }
}







#' @title 
#' xxx
#' 
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



#' @title xxx
#' 
#' @description 
#' xxx
#' 
#' @param cond xxx
#' 
ToggleState_ResetBtn = function(cond){
  if(verbose) cat(crayon::yellow(paste0(id, '::ToggleState_ResetBtn(', cond, '))\n\n')))
  
  shinyjs::toggleState('rstBtn', condition = cond)
}


# 
# 
# output$SkippedInfoPanel <- renderUI({
#   #if (verbose) cat(paste0(class(self)[1], '::output$SkippedInfoPanel from - ', self$id, "\n\n"))
#   
#   current_step_skipped <- rv.process$steps.status[rv.process$current.pos] == global$SKIPPED
#   #entire_process_skipped <- isTRUE(sum(rv.process$steps.status) == global$SKIPPED * rv.process$length)
#   req(current_step_skipped)
#   
#   
#   #if (entire_process_skipped){
#     # This case appears when the process has been skipped from the
#     # pipleine. Thus, it is not necessary to show the info box because
#     # it is shown below the timeline of the pipeline
#   #} else {
#     txt <- paste0("This ", rv.process$config$type, " is skipped so it has been disabled.")
#     wellPanel(
#       style = "background-color: #7CC9F0; opacity: 0.72; padding: 0px; align: center; vertical-align: center;",
#       height = 100,
#       width=300,
#       align="center",
#       p(style = "color: black;", paste0('Info: ',txt))
#     )
#   #}
# })


#' @title Change current position.
#' 
#' @description
#' Change current position.
#' 
#' @param direction xxx
#'
NavPage = function(direction) {
  newval <- rv.process$current.pos + direction 
  newval <- max(1, newval)
  newval <- min(newval, rv.process$length)
  rv.process$current.pos <- newval
}

observeEvent(input$prevBtn, ignoreInit = TRUE, {NavPage(-1)})
observeEvent(input$nextBtn, ignoreInit = TRUE, {NavPage(1)})

# Catch new status event
observeEvent(rv.process$steps.status, ignoreInit = TRUE, {
  # https://github.com/daattali/shinyjs/issues/166
  # https://github.com/daattali/shinyjs/issues/25
  if (verbose) cat(crayon::yellow(paste0(id, '::observeEvent(rv.process$steps.status)\n\n')))
  
  Discover_Skipped_Steps()
  Update_State_Screens()
  if (rv.process$steps.status[rv.process$length] == global$VALIDATED){
    rv.process$current.pos <- rv.process$length
    Send_Result_to_Caller()
  }
})



#' @title
#' Disable an entire process
#' @description 
#' The parameter is.enabled() is updated by the caller and tells the process
#' if it is enabled or disabled (remote action from the caller)
observeEvent(is.enabled(), ignoreNULL = TRUE, ignoreInit = TRUE, {
  if (verbose) cat(crayon::yellow(paste0(id, '::is.enabled()\n\n')))
  if (isTRUE(is.enabled())){
    Update_State_Screens()
  } else {
    rv.process$steps.enabled <- setNames(rep(is.enabled(), rv.process$length), 
                                         rv.process$config$steps)
  }
})


#' @title
#' Skipping an entire process
#' @description 
#' The parameter is.skipped() is set by the caller and tells the process
#' if it is skipped or not (remote action from the caller)
observeEvent(is.skipped(), ignoreNULL = FALSE, ignoreInit = TRUE,{
  # Catches a new value on the remote parameter `Reset`. A TRUE value indicates
  # that the caller program wants this module to reset itself.
  if (verbose) cat(crayon::yellow(paste0(id, '::observeEvent(is.skipped()). Value = ', is.skipped(), "\n\n")))
 if (isTRUE(is.skipped()))
     Set_All_Skipped()
  else{
    Unskip_All_Steps()
    Update_State_Screens()
  }
})


observeEvent(input$closeModal, {removeModal() })


observeEvent(remoteReset(), ignoreInit = TRUE, {
  # Catches a new value on the remote parameter `Reset`. A TRUE value indicates
  # that the caller program wants this module to reset itself. 
  if (verbose) cat(crayon::yellow(paste0(id, '::observeEvent(remoteReset()). Value = ', remoteReset(), "\n\n")))
  #browser()
  LocalReset()
})



observeEvent(input$rstBtn, ignoreInit = TRUE, {
  # Catches a new value on the remote parameter `Reset`. A TRUE value indicates
  # that the caller program wants this module to reset itself. 
  if (verbose) cat(crayon::yellow(paste0('::observeEvent(input$rstBtn) from - ', id, "\n\n")))
  #browser()
  showModal(dataModal())
})


observeEvent(input$modal_ok, ignoreInit=FALSE, ignoreNULL = TRUE, {
  # Catches a clic on the `Ok` button of the modal for resetting a module
  if (verbose) cat(crayon::yellow(paste0(id, '::observeEvent(input$modal_ok)\n\n')))
  #browser()
  #rv.process$steps.reset <- input$rstBtn + reset()
  #Set_All_Reset()
  LocalReset()
  
  removeModal()
})




# Default actions on reset pipeline or process.
# 
LocalReset = function(){
  if(verbose) cat(crayon::yellow(paste0(id, '::LocalReset()\n\n')))
  #browser()
  rv.process$dataIn <- NULL
  #rv.process$temp.dataIn <- NULL
  
  # The cursor is set to the first step
  rv.process$current.pos <- 1
  
  # The status of the steps are reinitialized to the default configuration of the process
  rv.process$steps.status <- setNames(rep(global$UNDONE, rv.process$length), 
                                rv.process$config$steps)
  
  # If the current module is a pipeline type (node and not leaf),
  # then sent to its children the information that they must reset themself
  if (nav.mode == 'pipeline')
    ResetChildren()
  
  # Return the NULL value as dataset
  Send_Result_to_Caller()
  #dataOut <- reactive({Send_Result_to_Caller(rv.process$dataIn)})
}

# This function changes the state (enabled, disabled) of the steps in the process
# The parameter 'cond' is the new state
# The parameter 'range' corresponds to the range of steps to update
ToggleState_Screens = function(cond, range){
  if(verbose) cat(crayon::yellow(paste0(id, '::ToggleState_Screens(cond = ', cond, ', range = ', paste0(range, collapse = " "), ')\n\n')))
  #browser()
  if (isTRUE(is.enabled()))
    lapply(range, function(x){
      cond <- cond && !(rv.process$steps.status[x] == global$SKIPPED)
      
      #Send to TL the enabled/disabled tags
      rv.process$steps.enabled[x] <- cond
    })
  
  
  # Update the state enabled/disabled of the navigation buttons
  #ToggleState_NavBtns()
}

ToggleState_NavBtns = function(){
  if(verbose) cat(crayon::yellow(paste0(id, '::ToggleState_NavBtns()\n\n')))
  
  # If the cursor is not on the first position, show the 'prevBtn'
  cond <-  rv.process$current.pos != 1
  shinyjs::toggleState(id = "prevBtn", condition = cond)
  
  # If the cursor is set before the last step, show the 'nextBtn'
  cond <- rv.process$current.pos < rv.process$length
  shinyjs::toggleState(id = "nextBtn", condition = cond)
}


# 
# 
# output$show_Debug_Infos <- renderUI({
#   tagList(
#     uiOutput(ns('show_tag_enabled')),
#     fluidRow(
#       column(width=2,
#              tags$b(h4(style = 'color: blue;', paste0("Global input of ", rv.process$config$type))),
#              uiOutput(ns('show_dataIn'))),
#       # column(width=2,
#       #        tags$b(h4(style = 'color: blue;', paste0("Temp input of ", rv.process$config$type))),
#       #        uiOutput(ns('show_rv_dataIn'))),
#       column(width=2,
#              tags$b(h4(style = 'color: blue;', paste0("Output of ", rv.process$config$type))),
#              uiOutput(ns('show_rv_dataOut'))),
#       column(width=4,
#              tags$b(h4(style = 'color: blue;', "status")),
#              uiOutput(ns('show_status')))
#     )
#   )
# })
# 
# ###########---------------------------#################
# output$show_dataIn <- renderUI({
#   if (verbose) cat(paste0('::output$show_dataIn from - ', id, "\n\n"))
#   req(dataIn())
#   tagList(
#     # h4('show dataIn()'),
#     lapply(names(dataIn()), function(x){tags$p(x)})
#   )
# })
# 
# # output$show_rv_dataIn <- renderUI({
# #   if (verbose) cat(paste0('::output$show_rv_dataIn from - ', id, "\n\n"))
# #   req(rv.process$dataIn)
# #   tagList(
# #     # h4('show dataIn()'),
# #     lapply(names(rv.process$dataIn), function(x){tags$p(x)})
# #   )
# # })
# 
# output$show_rv_dataOut <- renderUI({
#   if (verbose) cat(paste0('::output$show_rv_dataOut from - ', id, "\n\n"))
#   tagList(
#     #h4('show dataOut$value'),
#     lapply(names(dataOut$value), function(x){tags$p(x)})
#   )
# })
# 
# 
# output$show_status <- renderUI({
#   tagList(lapply(seq_len(rv.process$length), 
#                  function(x){
#                    color <- if(rv.process$steps.enabled[x]) 'black' else 'lightgrey'
#                    if (x == rv.process$current.pos)
#                      tags$p(style = paste0('color: ', color, ';'),
#                             tags$b(paste0('---> ', rv.process$config$steps[x], ' - ', GetStringStatus(rv.process$steps.status[[x]])), ' <---'))
#                    else 
#                      tags$p(style = paste0('color: ', color, ';'),
#                             paste0(rv.process$config$steps[x], ' - ', GetStringStatus(rv.process$steps.status[[x]])))
#                  }))
# })
# 
# output$show_tag_enabled <- renderUI({
#   tagList(
#     p(paste0('steps.enabled = ', paste0(as.numeric(rv.process$steps.enabled), collapse=' '))),
#     p(paste0('enabled() = ', as.numeric(is.enabled())))
#   )
# })
# 
