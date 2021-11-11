

# Reactive values that will be used to output the current dataset when 
    # the last step is validated
    dataOut <- reactiveValues(
      trigger = NULL,
      value = NULL
    )
    
  rv <- reactiveValues(
      # @field proc contains the return value of the process module that has been called 
      proc = NULL,
      
      # @field status A boolan vector which contains the status (validated,
      # skipped or undone) of the steps
      steps.status = NULL,
      
      # @field dataIn Contains the dataset passed by argument to the module server
      dataIn = NULL,
      
      # @field temp.dataIn This variable is used to serves as a tampon between 
      # the input of the module and the functions. 
      temp.dataIn = NULL,
      
      # @field steps.enabled Contains the value of the parameter 'is.enabled'
      steps.enabled = NULL,
      
      # A vector of boolean where each element indicates whether 
      # the corresponding process is skipped or not
      # ONLY USED WITH PIPELINE
      steps.skipped = NULL,
      
      # A vector of integers that indicates if each step must be reseted
      # This is an information sent to the child processes. Each time a child 
      # process must be reseted, the corresponding element is incremented
      # in order to modify its value. Thus, it can be catched by Shiny observers
      # ONLY USED WITH PIPELINE
      resetChildren = NULL,
      
      # @field current.pos Stores the current cursor position in the timeline and 
      # indicates which of the process' steps is active
      current.pos = 1,
      
      length = NULL,
      config = NULL
    )




# output$SkippedInfoPanel <- renderUI({
#       if (verbose) cat(paste0(class(self)[1], "::output$SkippedInfoPanel from - ", self$id, "\n\n"))
#       
#       current_step_skipped <- rv$steps.status[rv$current.pos] == global$SKIPPED
#       req(current_step_skipped)
#       process_entirely_skipped <- isTRUE(sum(rv$steps.status) == global$SKIPPED * rv$length)
#       
#       if (process_entirely_skipped){
#         # This case appears when the process has been skipped from the
#         # pipeline. Thus, it is not necessary to show the info box because
#         # it is shown below the timeline of the pipeline
#       } else {
#         txt <- paste0("This ", rv$config$type, " is skipped so it has been disabled.")
#         wellPanel(
#           style = "background-color: #7CC9F0; opacity: 0.72; padding: 0px;
#                    align: center; vertical-align: center;",
#           height = 100,
#           width = 300,
#           align = "center",
#         p(style = "color: black;", paste0("Info: ",txt))
#     )
# }
# })




observeEvent(dataIn(), ignoreNULL = FALSE, ignoreInit = FALSE,{
      #observe({
      if (verbose) cat(yellow(paste0(id, "::observe(dataIn())\n\n")))
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



observeEvent(is.enabled(), ignoreNULL = TRUE, ignoreInit = TRUE, {
      if (verbose) cat(yellow(paste0(id, '::is.enabled()\n\n')))
      if (isTRUE(is.enabled())){
        Update_State_Screens()
      } else {
        rv$steps.enabled <- setNames(rep(is.enabled(), rv$length), 
                                     rv$config$steps)
      }
    })



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
    })


observeEvent(remoteReset(), ignoreInit = TRUE, {
      if (verbose) cat(yellow(paste0(id, '::observeEvent(remoteReset()). Value = ', remoteReset(), '\n\n')))
      LocalReset()
    })
    

  

    
    
    
observeEvent(input$rstBtn, ignoreInit = TRUE, {
      if (verbose) cat(yellow(paste0('::observeEvent(input$rstBtn) from - ', id, '\n\n')))
      showModal(dataModal())
    })




observeEvent(input$modal_ok, ignoreInit=FALSE, ignoreNULL = TRUE, {
  if (verbose) cat(yellow(paste0(id, '::observeEvent(input$modal_ok)\n\n')))
  LocalReset()
  removeModal()
})






#' 
ToggleState_Screens = function(cond, range){
  if(verbose) cat(crayon::yellow(paste0(id, '::ToggleState_Screens(cond = ', cond, ', range = ', paste0(range, collapse = ' '), ')\n\n')))
  #browser()
  if (isTRUE(is.enabled()))
    lapply(range, function(x){
      cond <- cond && !(rv$steps.status[x] == Magellan::global$SKIPPED)

      #Send to TL the enabled/disabled tags
      rv$steps.enabled[x] <- cond
    })
  }



#' 
ToggleState_NavBtns = function(){
    if(verbose) cat(crayon::yellow(paste0(id, '::ToggleState_NavBtns()\n\n')))

    # If the cursor is not on the first position, show the 'prevBtn'
    cond <-  rv$current.pos != 1
    shinyjs::toggleState(id = 'prevBtn', condition = cond)

    # If the cursor is set before the last step, show the 'nextBtn'
    cond <- rv$current.pos < rv$length
    shinyjs::toggleState(id = 'nextBtn', condition = cond)
  }





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





NavPage = function(direction) {
    newval <- rv$current.pos + direction
    newval <- max(1, newval)
    newval <- min(newval, rv$length)
    rv$current.pos <- newval
  }
  
  observeEvent(input$prevBtn, ignoreInit = TRUE, {NavPage(-1)})
  observeEvent(input$nextBtn, ignoreInit = TRUE, {NavPage(1)})





dataModal = function() {
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

observeEvent(input$closeModal, {removeModal() })



Discover_Skipped_Steps = function(){
  if(verbose) cat(crayon::yellow(paste0(id, '::Discover_Skipped_Steps()\n\n')))
  for (i in seq_len(rv$length)){
    max.val <- GetMaxValidated_AllSteps()
    if (rv$steps.status[i] != Magellan::global$VALIDATED && max.val > i)
      rv$steps.status[i] <- Magellan::global$SKIPPED
  }
}





Unskip_All_Steps = function(){
    if(verbose) cat(crayon::yellow(paste0(id, '::Unskip_All_Steps()\n\n')))
    rv$steps.status <- setNames(rep(Magellan::global$UNDONE, rv$length),
                                rv$config$steps)
    Update_State_Screens()
  }



Set_All_Skipped = function(){
    if(verbose) cat(crayon::yellow(paste0(id, '::Set_All_Skipped()\n\n')))
    rv$steps.status <- setNames(rep(Magellan::global$SKIPPED, rv$length),
                                rv$config$steps)
  }





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



Change_Current_Pos = function(i){ rv$current.pos <- i}



ToggleState_ResetBtn = function(cond){
    if(verbose) cat(crayon::yellow(paste0(id, '::ToggleState_ResetBtn(', cond, '))\n\n')))

    shinyjs::toggleState('rstBtn', condition = cond)
  }



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


#' 
#' 
GetMaxValidated_AllSteps = function(){
    if(verbose) cat(crayon::yellow(paste0( id, '::GetMaxValidated_AllSteps()\n\n')))
    val <- 0
    ind <- grep(Magellan::global$VALIDATED, rv$steps.status)
    if (length(ind) > 0)
      val <-max(ind)
    val
  }




output$EncapsulateScreens_ui <- renderUI({
  #EncapsulateScreens = function(){
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
  })



GetStringStatus = function(name){
    if (name == global$VALIDATED) 'Validated'
    else if (name == global$UNDONE) 'Undone'
    else if (name == global$SKIPPED) 'Skipped'
  }

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



Send_Result_to_Caller = function(){
    if(verbose) cat(crayon::yellow(paste0(id, '::Send_Result_to_Caller()\n\n')))
    dataOut$trigger <- Timestamp()
    dataOut$value <- rv$dataIn
  }


CurrentStepName <- reactive({
  cat(crayon::yellow(paste0('::GetCurrentStepName() from - ', id, '\n')))
  rv$config$steps[rv$current.pos]
})
