global = list(VALIDATED = 1,
              SKIPPED = -1,
              UNDONE = 0
)

default_pos =list(VALIDATED = 1,
                  SKIPPED = 1,
                  UNDONE = 1
)

redBtnClass <- "btn-danger"
PrevNextBtnClass <- "btn-info"
btn_success_color <- "btn-success"
optionsBtnClass <- "info"

btn_style <- "display:inline-block; vertical-align: middle; padding: 7px"




#' @title 
#' xxx
#' 
#' @description xxx
#' 
AddItemToDataset <- function(dataset, name){
  addAssay(dataset, 
           dataset[[length(dataset)]], 
           name=name)
}


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




# rv.process <- reactiveValues(
#   status = NULL,
#   dataIn = NULL,
#   #temp.dataIn = NULL,
#   current.pos = 1,
#  steps.enabled = NULL,
#'   test = NULL,
#   length = NULL,
#   config = NULL
# )
# 
# 
# #' @field dataOut xxx
# dataOut <- reactiveValues(
#   trigger = 0,
#   value = NULL
# )


#' @title 
#' xxx
#' 
#' @description xxx
#' 
dataOut <- reactiveValues(
  trigger = NULL,
  value = NULL
)

#' @title 
#' xxx
#' 
#' @description xxx
#' 
rv.process <- reactiveValues(
  #' @field proc contains the return value of the called process 
  proc = NULL,
  #' @field status A booelan vector which contains the status (validated,
  #' skipped or undone) of the steps
  status = NULL,
  #' @field dataIn A dataset
  dataIn = NULL,
  #' @field temp.dataIn This variable is used to serves as a tampon between 
  #' the input of the module and the functions. 
  temp.dataIn = NULL,
  #' @field steps.enabled xxx
  steps.enabled = NULL,
  #' @field current.pos Stores the current cursor position in the timeline
  current.pos = 1
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
  if(verbose) cat(paste0('::Checkrv.process$config() from - ', id, "\n\n"))
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
  list(passed=passed,
       msg = msg)
}



# #' @title 
# #' xxx
# #' 
# #' @description xxx
# #' 
# # Send_Result_to_Caller = function(){
# #   if(verbose) cat(paste0('::Send_Result_to_Caller() from - ', id, "\n\n"))
# #   dataOut$trigger <- Timestamp()
# #   dataOut$value <- rv.process$dataIn
# # }
# 
# Send_Result_to_Caller = function(data){
#   list(trigger = as.numeric(Sys.time()),
#        value = data
#   )
# }

# #' @description 
# #' xxx
# #' 
# InitializeDataIn = function(){ 
#   if(verbose) cat(paste0('InitializeDataIn() from - ', id, "\n\n"))
#   rv.process$dataIn <- rv.process$temp.dataIn
# }


# #' @description
# #' Validate a given position. To be used by xxx
# #' 
# #' @return Nothing.
# #' 
# ValidateCurrentPos <- function(){
#   browser()
#   #rv.process$status[rv.process$current.pos] <- global$VALIDATED
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
#' Gives the name of the status corresponding to the code (integer).
#'
#' @param name A number
#' 
GetStringStatus = function(name){
  if (name==global$VALIDATED) "Validated"
  else if (name==global$UNDONE) "Undone"
  else if (name==global$SKIPPED) 'Skipped'
}


#' @title 
#' xxx
#' @description 
#' Returns the date and time in timestamp UNIX format.
#' 
Timestamp = function(){ 
  if(verbose) cat(paste0('::Timestamp() from - ', id, "\n\n"))
  as.numeric(Sys.time())
}




#' @title 
#' xxx
#' @description 
#' xxx
#' 
GetMaxValidated_AllSteps = function(){
  if(verbose) cat(paste0( '::', 'GetMaxValidated_AllSteps() from - ', id, "\n\n"))
  val <- 0
  ind <- grep(global$VALIDATED, rv.process$status)
  if (length(ind) > 0) 
    val <-max(ind)
  val
}


#' @title 
#' xxx
#' @description
#' This function calls the server part of each module composing the pipeline
#'
#' @return Nothing
#'
GetMaxValidated_BeforeCurrentPos = function(){
  if(verbose) cat(paste0('GetMaxValidated_BeforeCurrentPos() from - ', id, "\n\n"))
  ind.max <- NULL
  indices.validated <- which(rv.process$status == global$VALIDATED)
  if (length(indices.validated) > 0){
    ind <- which(indices.validated < rv.process$current.pos)
    if(length(ind) > 0)
      ind.max <- max(ind)
  }
  ind.max
}



#' @title 
#' xxx
#' @description
#' This function calls the server part of each module composing the pipeline
#'
#' @param pos xxx
#' 
#' @return Nothing
#'
GetMaxValidated_BeforePos = function(pos){
  if(verbose) cat(paste0('GetMaxValidated_BeforeCurrentPos() from - ', id, "\n\n"))
  ind.max <- NULL
  indices.validated <- which(rv.process$status == global$VALIDATED)
  if (length(indices.validated) > 0){
    ind <- which(indices.validated < pos)
    if(length(ind) > 0)
      ind.max <- max(ind)
  }
  ind.max
}



#' @title 
#' xxx
#' @description 
#' xxx
#'
#' @param range xxx
#' 
GetFirstMandatoryNotValidated = function(range){
  if(verbose) cat(paste0('::', 'GetFirstMandatoryNotValidated() from - ', id, "\n\n"))
  #browser()
  first <- NULL
  first <- unlist((lapply(range, 
                          function(x){rv.process$config$mandatory[x] && !rv.process$status[x]})))
  if (sum(first) > 0)
    min(which(first == TRUE))
  else
    NULL
}




#' @title 
#' xxx
#' @description 
#' xxx
#' 
#' @param i xxx
#' 
Change_Current_Pos = function(i){ rv.process$current.pos <- i}


#' @title 
#' xxx
#' @description
#' et to skipped all steps of the current object
#' 
#' @return Nothing.
#' 
Set_All_Skipped = function(){
  if(verbose) cat(paste0('::', 'Set_All_Skipped() from - ', id, "\n\n"))
  rv.process$status <- setNames(rep(global$SKIPPED, rv.process$length), rv.process$config$steps)
}


#' @title 
#' xxx
#' @description
#' et to skipped all steps of the current object
#' 
#' @return Nothing.
#' 
Discover_Skipped_Steps = function(){
  if(verbose) cat(paste0('::Discover_Skipped_Status() from - ', id, "\n\n"))
  for (i in seq_len(rv.process$length)){
    max.val <- GetMaxValidated_AllSteps()
    if (rv.process$status[i] != global$VALIDATED && max.val > i)
      rv.process$status[i] <- global$SKIPPED
  }
}


observeEvent(is.enabled(), ignoreNULL = FALSE, ignoreInit = TRUE, {
  # browser()
  if (!isTRUE(is.enabled()))
    rv.process$steps.enabled <- setNames(rep(FALSE, rv.process$length), rv.process$config$steps)
})



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
  if(verbose) cat(paste0( '::', 'ToggleState_ResetBtn(', cond, ')) from - ', id, "\n\n"))
  
  shinyjs::toggleState('rstBtn', condition = cond)
}


# 
# 
# output$SkippedInfoPanel <- renderUI({
#   #if (verbose) cat(paste0(class(self)[1], '::output$SkippedInfoPanel from - ', self$id, "\n\n"))
#   
#   current_step_skipped <- rv.process$status[rv.process$current.pos] == global$SKIPPED
#   #entire_process_skipped <- isTRUE(sum(rv.process$status) == global$SKIPPED * rv.process$length)
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


#' @title xxx
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

observeEvent(rv.process$status, ignoreInit = TRUE, {
  # https://github.com/daattali/shinyjs/issues/166
  # https://github.com/daattali/shinyjs/issues/25
  if (verbose) cat(paste0('::observe((rv$status) from - ', id, "\n\n"))
  
  Discover_Skipped_Steps()
  Update_State_Screens()
  #browser()
  # if (rv.process$status[rv.process$length])
  #   Send_Result_to_Caller()
  # 
  if (rv.process$status[rv.process$length] == global$VALIDATED){
    rv.process$current.pos <- rv.process$length
    Send_Result_to_Caller()
  }
})



observeEvent(req(is.skipped()), ignoreInit=TRUE,{
  # Catches a new value on the remote parameter `Reset`. A TRUE value indicates
  # that the caller program wants this module to reset itself.
  if (verbose) cat(paste0('::observeEvent(input$rstBtn) from - ', id, "\n\n"))
 print('is.skipped')
 Update_State_Screens()
 #rv.process$status <- rep(global$SKIPPED, rv.process$length)
})


observeEvent(input$closeModal, {removeModal() })


observeEvent(remoteReset(), ignoreInit = TRUE, {
  # Catches a new value on the remote parameter `Reset`. A TRUE value indicates
  # that the caller program wants this module to reset itself. 
  if (verbose) cat(paste0('::observeEvent(input$rstBtn) from - ', id, "\n\n"))
  #browser()
  LocalReset()
})



observeEvent(input$rstBtn, ignoreInit = TRUE, {
  # Catches a new value on the remote parameter `Reset`. A TRUE value indicates
  # that the caller program wants this module to reset itself. 
  if (verbose) cat(paste0('::observeEvent(input$rstBtn) from - ', id, "\n\n"))
  #browser()
  showModal(dataModal())
})


observeEvent(input$modal_ok, ignoreInit=FALSE, ignoreNULL = TRUE, {
  # Catches a clic on the `Ok` button of the modal for resetting a module
  if (verbose) cat(paste0('::observeEvent(req(c(input$modal_ok))) from - ', id, "\n\n"))
  #browser()
  #rv.process$reset <- input$rstBtn + reset()
  #Set_All_Reset()
  LocalReset()
  
  removeModal()
})



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
#                             tags$b(paste0('---> ', rv.process$config$steps[x], ' - ', GetStringStatus(rv.process$status[[x]])), ' <---'))
#                    else 
#                      tags$p(style = paste0('color: ', color, ';'),
#                             paste0(rv.process$config$steps[x], ' - ', GetStringStatus(rv.process$status[[x]])))
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
