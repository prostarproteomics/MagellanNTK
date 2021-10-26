


#' @title xxx
#' 
#' @description 
#' xxxxxx
#' 
#' @param id xxx
#' 
#' @rdname mod_navigation
#' 
#' @export
#'
mod_navigation_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    uiOutput(ns('process_ui')),
    uiOutput(ns('pipeline_ui'))
  )
}








#' @title xxx
#' 
#' @description xxx
#' 
#' @param id xxx
#' 
#' @param nav.mode xxx
#' 
#' @param dataIn The dataset
#' 
#' @param is.enabled A boolean. This variable is a remote command to specify
#' if the corresponding module is enabled/disabled in the calling module of upper level.
#' For example, if this module is part of a pipeline and the pipeline calculates
#' that it is disabled (i.e. skipped), then this variable is set to TRUE. Then,
#' all the widgets will be disabled. If not, the enabling/disabling of widgets
#' is deciding by this module. 
#' 
#' @param remoteReset It is a remote command to reset the module. A boolen that 
#' indicates is the pipeline has been reseted by a program of higher level
#' Basically, it is the program which has called this module
#' 
#' @param is.skipped xxx
#' 
#' @return A list of four items:
#' * dataOut xxx
#' * steps.enabled xxxxx
#' * status xxxx
#' * reset xxxx
#' 
#' @export
#' 
#' @rdname mod_navigation
#' 
#' @examples
#' \dontrun{
#' library(shiny)
#' library(shinyBS)
#' ui <- fluidPage(
#'   mod_navigation_ui('Protein_Description')
#' )
#' server <- function(input, output){
#'   mod_navigation_server(id = 'Protein_Description',
#'   nav.mode = 'process',
#'   dataIn = reactive({feat1})
#'   )
#' }
#' shinyApp(ui, server)
#' }
#' 
#' @importFrom stats setNames
#' 
mod_navigation_server <- function(id,
                                  nav.mode = NULL,
                                  dataIn = reactive({NULL}),
                                  is.enabled = reactive({TRUE}),
                                  remoteReset = reactive({FALSE}),
                                  is.skipped = reactive({FALSE})
                                  ){
  
  
  
  ###-------------------------------------------------------------###
  ###                                                             ###
  ### ------------------- MODULE SERVER --------------------------###
  ###                                                             ###
  ###-------------------------------------------------------------###
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
    
    
    
    #----------------------------------------------------------------------------
    # Declaration of global variables
    verbose <- FALSE
    
    # global <- list(VALIDATED = 1,
    #                SKIPPED = -1,
    #                UNDONE = 0
    # )
    # 
    # default_pos <- list(VALIDATED = 1,
    #                     SKIPPED = 1,
    #                     UNDONE = 1
    # )

    #--------------------------------------------------------
    
    # Reactive values that will be used to output the current dataset when 
    # the last step is validated
    dataOut = reactiveValues(
      trigger = NULL,
      value = NULL
    )
    
    
    #These reactive values are specific to this instance of mod_navigation_server
    rv.process <- reactiveValues(
      # @field proc contains the return value of the process module that has been called 
      proc = NULL,
      
      # @field status A booelan vector which contains the status (validated,
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
    
    
    # Specific to pipeline module
    # Used to store the return values (lists) of child processes
    # ONLY USED WITH PIPELINE
    tmp.return <- reactiveValues()
    
    # Used to xxx
    # ONLY USED WITH PIPELINE
    rv.child <- reactiveValues(
      # A vector of boolean where each element indicates if the corresponding
      # child if enable or disable
      enabled = NULL,
      
      # xxxx
      reset = NULL,
      
      # A vector of integers where each element denotes the current position 
      # of the corresponding element.
      position = NULL,
      
      # xxxx
      data2send = NULL
    )
    
    modal_txt <- NULL
    
    
    
    # End of declaration of variables
    
    
    
    observeEvent(nav.mode, {
     
      if (is.null(nav.mode) || !(nav.mode %in% c('process', 'pipeline')))
        stop("nav.mode must be either 'process' or 'pipeline'.")
      
      prefix <- "This action will reset this "
      suffix <- ". The input dataset will be the output of the last previous
                      validated process and all further datasets will be removed"
      modal_txt <- paste0(prefix, nav.mode, suffix)
      })
    
  
  output$pipeline_ui <- renderUI({
    req(nav.mode == 'pipeline')
    fluidRow(
      column(width = 2, 
             wellPanel(
               div(style = "padding: 10px",
                   div(style = btn_style,
                       shinyjs::disabled(
                         actionButton(ns("prevBtn"), "<<",
                                      class = PrevNextBtnClass,
                                      style='padding:4px; font-size:80%')
                       ),
                       actionButton(ns("rstBtn"), "Reset",
                                    class = redBtnClass,
                                    style='padding:4px; font-size:80%')
                   ),
                   div(style = btn_style,
                       actionButton(ns("nextBtn"),">>",
                                    class = PrevNextBtnClass,
                                    style='padding:4px; font-size:80%')
                   ),
                   mod_timeline_v_ui(ns('timelinev'))
               )
             )),
      column(width = 10,
             style = " padding-left: 20px;",
             wellPanel(
               div(id = ns('Screens'),
                   uiOutput(ns('SkippedInfoPanel')),
                   uiOutput(ns('EncapsulateScreens_ui'))
                   
               ),
               wellPanel(title = 'foo',
                         uiOutput(ns('show_Debug_Infos'))
               )
             ))
      
    )
  })
  
  
  output$process_ui <- renderUI({
    req(nav.mode == 'process')
    tagList(
    fluidRow(style = "display: flex;
    align-items: center;
             justify-content: center;",
             column(width = 1, shinyjs::disabled(
               actionButton(ns("prevBtn"), "<<",
                            class = PrevNextBtnClass,
                            style = 'font-size:80%')
             )),
             column(width = 1, actionButton(ns("rstBtn"), "Reset",
                                          class = redBtnClass,
                                          style='font-size:80%')),
             column(width = 9, mod_timeline_h_ui(ns('timeline'))),
             column(width = 1, shinyjs::disabled(
               actionButton(ns("nextBtn"),">>",
                            class = PrevNextBtnClass,
                            style='font-size:80%'))
             )
    ),
    div(id = ns('Screens'),
        uiOutput(ns('SkippedInfoPanel')),
        uiOutput(ns('EncapsulateScreens_ui'))
        
    ),
    wellPanel(title = 'foo',
              tagList(
                uiOutput(ns('show_Debug_Infos'))
              )
    )
    )
  })
  
  
  
  # @title 
  # xxx
  # 
  # @description xxx
  # 
  # Check if the rv.process$config is correct
  #
  # @param conf A list containing the rv.process$configuration of the current object.
  # See xxx
  # 
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
  
  
  Update_Data2send_Vector = function(){
    # One only update the current position because the vector has been entirely
    # initialized to NULL so the other processes are already ready to be sent
    ind.last.validated <- GetMaxValidated_BeforePos()
    if (is.null(ind.last.validated))
      data <- rv.process$temp.dataIn
    else
      data <- Keep_Datasets_from_Object(object = rv.process$dataIn,
                                        range = seq_len(ind.last.validated + rv.process$original.length -1)
      )
    return(data)
  }
  
  CurrentStepName <- reactive({
    cat(crayon::yellow(paste0('::GetCurrentStepName() from - ', id, '\n')))
    rv.process$config$steps[rv.process$current.pos]
  })
  
  # @description
  # This function calls the server part of each module composing the pipeline
  #
  # @return Nothing
  #
  PrepareData2Send = function(){
    if(verbose) cat(paste0(id, '::PrepareData2Send()\n\n'))
    #browser()
    # Returns NULL to all modules except the one pointed by the current position
    # Initialization of the pipeline : one send dataIn() to the
    # first module
    
    # The dataset to send is contained in the variable 'rv.process$dataIn'
    
    
    
    # Initialize vector to all NULL values
    rv.child$data2send <- setNames(
      lapply(rv.process$config$steps, function(x){NULL}),
      rv.process$config$steps)
    
    if (is.null(rv.process$dataIn)){ # Init of core engine
      
      # Only the first process will receive the data
      rv.child$data2send[[1]] <- rv.process$temp.dataIn
      
      # The other processes are by default disabled.
      # If they have to be enabled, they will be by another function later
      lapply(seq_len(rv.process$length), function(x){
        rv.process$steps.enabled[x] <- x==1
      })
      
    } else
      rv.child$data2send[[CurrentStepName()]] <- Update_Data2send_Vector()
    
    cat(crayon::blue("<----------------- Data sent to children ------------------> \n"))
    print(rv.child$data2send)
    cat(crayon::blue("<----------------------------------------------------> \n"))
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
  
  
  
  # @title 
  # xxx
  # @description
  # Converts the numerical code for status into string.
  #
  # @param name A number
  # 
  GetStringStatus = function(name){
    if (name == Magellan::global$VALIDATED) "Validated"
    else if (name == Magellan::global$UNDONE) "Undone"
    else if (name == Magellan::global$SKIPPED) 'Skipped'
  }
  
  
  
  
  # @title 
  # Get the last validated step among all the steps
  # @description 
  # This function analyzes the reactive variable rv.process$steps.status
  # to find the indice of the last validated step among all steps
  # 
  GetMaxValidated_AllSteps = function(){
    if(verbose) cat(crayon::yellow(paste0( id, '::GetMaxValidated_AllSteps()\n\n')))
    val <- 0
    ind <- grep(Magellan::global$VALIDATED, rv.process$steps.status)
    if (length(ind) > 0) 
      val <-max(ind)
    val
  }
  
  
  
  
  # @title 
  # Get the last validated step before a given position
  # @description
  # This function analyzes the reactive variable rv.process$steps.status
  # to find the indice of the last validated step among all steps before
  # the current position (parameter pos set to NULL) or a given position (
  # parameter pos set to an integer).
  #
  # @param pos xxx
  # 
  # @return Nothing
  #
  GetMaxValidated_BeforePos = function(pos = NULL){
    if(verbose) cat(crayon::yellow(paste0(id, 'GetMaxValidated_BeforePos()\n\n')))
    
    if (is.null(pos))
      pos <- rv.process$current.pos
    
    ind.max <- NULL
    indices.validated <- which(rv.process$steps.status == Magellan::global$VALIDATED)
    if (length(indices.validated) > 0){
      ind <- which(indices.validated < pos)
      if(length(ind) > 0)
        ind.max <- max(ind)
    }
    ind.max
  }
  
  
  
  
  # @title 
  # Get first mandatory step not yet validated
  # @description 
  # This function analyses the vectors mandatory and status to find the first
  # step which is mandatory and not yet validated in a range of integers
  #
  # @param range xxx
  # 
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
  
  
  
  
  # @title 
  # Set current position
  # @description 
  # Change the cursor position to a given position
  # 
  # @param i An integer that corresponds to the new position
  #
  Change_Current_Pos = function(i){ rv.process$current.pos <- i}
  
  
  # @title 
  # Set all steps of the current process to skipped
  # @description
  # Set to skipped all steps of the current object
  # 
  # @return Nothing.
  # 
  Set_All_Skipped = function(){
    if(verbose) cat(crayon::yellow(paste0(id, '::Set_All_Skipped()\n\n')))
    rv.process$steps.status <- setNames(rep(Magellan::global$SKIPPED, rv.process$length), 
                                        rv.process$config$steps)
  }
  
  
  Unskip_All_Steps = function(){
    if(verbose) cat(crayon::yellow(paste0(id, '::Unskip_All_Steps()\n\n')))
    rv.process$steps.status <- setNames(rep(Magellan::global$UNDONE, rv.process$length), 
                                        rv.process$config$steps)
    Update_State_Screens()
  }
  
  
  
  # @title 
  # Discover new skipped steps.
  # @description
  # This function looks for new skipped steps after the vector status has been updated.
  #
  # @return Nothing.
  # 
  Discover_Skipped_Steps = function(){
    if(verbose) cat(crayon::yellow(paste0(id, '::Discover_Skipped_Steps()\n\n')))
    for (i in seq_len(rv.process$length)){
      max.val <- GetMaxValidated_AllSteps()
      if (rv.process$steps.status[i] != Magellan::global$VALIDATED && max.val > i)
        rv.process$steps.status[i] <- Magellan::global$SKIPPED
    }
  }
  
  
  
  
  
  
  
  # @title 
  # xxx
  # 
  # @description 
  # Return the UI for a modal dialog with data selection input. If 'failed' is
  # TRUE, then display a message that the previous value was invalid.
  # 
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
  
  
  # @title xxx
  # 
  # @description 
  # xxx
  # 
  # @param cond xxx
  # 
  ToggleState_ResetBtn = function(cond){
    if(verbose) cat(crayon::yellow(paste0(id, '::ToggleState_ResetBtn(', cond, '))\n\n')))
    
    shinyjs::toggleState('rstBtn', condition = cond)
  }
  
  
  # @title Change current position.
  # 
  # @description
  # Change current position.
  # 
  # @param direction xxx
  #
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
    if (rv.process$steps.status[rv.process$length] == Magellan::global$VALIDATED){
      rv.process$current.pos <- rv.process$length
      Send_Result_to_Caller()
    }
  })
  
  
  
  # @title
  # Disable an entire process
  # @description 
  # The parameter is.enabled() is updated by the caller and tells the process
  # if it is enabled or disabled (remote action from the caller)
  observeEvent(is.enabled(), ignoreNULL = TRUE, ignoreInit = TRUE, {
    if (verbose) cat(crayon::yellow(paste0(id, '::is.enabled()\n\n')))
    if (isTRUE(is.enabled())){
      Update_State_Screens()
    } else {
      rv.process$steps.enabled <- setNames(rep(is.enabled(), rv.process$length), 
                                           rv.process$config$steps)
    }
  })
  
  
  # @title
  # Skipping an entire process
  # @description 
  # The parameter is.skipped() is set by the caller and tells the process
  # if it is skipped or not (remote action from the caller)
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
  
  
  # @description
  # et to skipped all steps of the current object
  # 
  # @return Nothing.
  # 
  ResetChildren = function(range){
    if(verbose) cat(paste0(id, '::ResetChildren()\n\n'))
    if (nav.mode != 'pipeline')
      return(NULL)
    #browser()
    # lapply(rv.process$config$steps, function(x){
    #   rv.process$resetChildren[x] <- 1 + rv.process$resetChildren[x]
    # })
    rv.process$resetChildren[range] <- 1 + rv.process$resetChildren[range]
  }
  
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
    rv.process$steps.status <- setNames(rep(Magellan::global$UNDONE, rv.process$length), 
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
        cond <- cond && !(rv.process$steps.status[x] == Magellan::global$SKIPPED)
        
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
  
  
   
  InitProcessServer <- function(){
    # source(file.path('example_modules', 'mod_PipelineA_Description.R'), local=TRUE)$value
    # source(file.path('example_modules', 'mod_PipelineA_ProcessA.R'), local=TRUE)$value
    # source(file.path('example_modules', 'mod_PipelineA_ProcessB.R'), local=TRUE)$value
    # source(file.path('example_modules', 'mod_PipelineA_ProcessC.R'), local=TRUE)$value
    # 
    #Call the module server of the process
    # The 'dataIn' parameter correspond to the dataset passed to this nav_process server
    # more specifically, the temporary variable
    # The parameter 'steps.enabled' is xxxx
    # The parameter 'remoteReset' send to the process module the information that it has to 
    # be reseted. It is the sum of the input$rstBtn (the local reset button of the nav_process) and
    # the remoteReset() variable which correspond to the reset button of the container of
    # the nav process (ie the mod_nav_pipeline)
    rv.process$proc <- do.call(paste0('mod_', id, '_server'),
                               list(id = id,
                                    dataIn = reactive({rv.process$temp.dataIn}),
                                    steps.enabled = reactive({rv.process$steps.enabled}),
                                    remoteReset = reactive({input$rstBtn + remoteReset()}))
    )
    
    # Instantiate the local variables
    # Get the config variable from the process that has been called
    # This config contains all the UI for the each steps (config$ll.UI)
    # and the dataset returned by the process (config$dataOut)
    rv.process$config <- rv.process$proc$config()
    
    # Check if the config variable is correct
    check <- CheckConfig(rv.process$config)
    if (!check$passed)
      stop(paste0("Errors in 'rv.process$config'", paste0(check$msg, collapse=' ')))
    
    
    
    rv.process$length <- length(rv.process$config$steps)
    rv.process$current.pos  <- 1
    
    # Get the name of the parent of the process
    # The id variable is composed of two ids separate by '_'. The first id correspond to the parent
    # and the second correspond to the child in the process hierarchy
    rv.process$parent <- unlist(strsplit(id, split='_'))[1]
    
    
    rv.process$config$mandatory <- setNames(rv.process$config$mandatory, rv.process$config$steps)
    rv.process$steps.status <- setNames(rep(Magellan::global$UNDONE, rv.process$length), rv.process$config$steps)
    rv.process$currentStepName <- reactive({rv.process$config$steps[rv.process$current.pos]})
    rv.process$steps.enabled <- setNames(rep(FALSE, rv.process$length), rv.process$config$steps)
    
    
    # Launch the horizontal timeline server
    # The parameter 'config' is used to xxx
    # The parameter 'status' is used to color the bullets
    # the parameter 'position' is used to put the cursor at the current position
    # The parameter 'enabled' is used to modify the bullets whether the corresponding step is enabled or disabled
    mod_timeline_h_server(id = 'timeline',
                          config =  rv.process$config,
                          status = reactive({rv.process$steps.status}),
                          position = reactive({rv.process$current.pos}),
                          enabled = reactive({rv.process$steps.enabled})
    )
    
  }
  
  InitPipelineServer <- function(){
    # Call the server module of the pipeline which name is the parameter 'id'
    # This will give access to its config
    rv.process$proc <- do.call(paste0('mod_', id, '_server'),
                               list(id = id,
                                    dataIn = reactive({rv.process$temp.dataIn}),
                                    steps.enabled = reactive({rv.process$steps.enabled}),
                                    remoteReset = reactive({FALSE}),
                                    steps.status = reactive({rv.process$steps.status})
                               )
    )
    
    
    
    
    
    # Update the reactive value config with the config of the pipeline
    rv.process$config <- rv.process$proc$config()
    
    # TODO Write the CheckPipelineConfig function
    # Check if the config variable is correct
    # check <- CheckPipelineConfig(rv.process$config)
    # if (!check$passed)
    #   stop(paste0("Errors in 'rv.process$config'", paste0(check$msg, collapse=' ')))
    # 
    
    
    rv.process$length <- length(rv.process$config$steps)
    rv.process$current.pos <- 1
    
    # Get the name of the parent of the process
    # The id variable is composed of two ids separate by '_'. The first id correspond to the parent
    # and the second correspond to the child in the process hierarchy
    rv.process$parent.name <- unlist(strsplit(id, split='_'))[1]
    rv.process$child.name <- unlist(strsplit(id, split='_'))[2]
    
    
    rv.process$config$mandatory <- setNames(rv.process$config$mandatory, rv.process$config$steps)
    rv.process$steps.status = setNames(rep(Magellan::global$UNDONE, rv.process$length), rv.process$config$steps)
    rv.process$currentStepName <- reactive({rv.process$config$steps[rv.process$current.pos]})
    
    rv.process$steps.enabled <- setNames(rep(FALSE, length(rv.process$config$steps)), rv.process$config$steps)
    rv.process$steps.skipped <- setNames(rep(FALSE, length(rv.process$config$steps)), rv.process$config$steps)
    rv.process$resetChildren <- setNames(rep(0, length(rv.process$config$steps)), rv.process$config$steps)
    
    rv.child$data2send <- setNames(lapply(as.list(rv.process$config$steps), function(x) NULL), 
                                   nm = rv.process$config$steps)
    
    # Launch the ui for each step of the pipeline
    # This function could be stored in the source file of the pipeline
    # but the strategy is to insert minimum extra code in the files for
    # pipelines and processes. This is useful when other devs will
    # develop other pipelines and processes. Tus, it will be easier.
    rv.process$config$ll.UI <- setNames(lapply(rv.process$config$steps,
                                               function(x){
                                                 mod_navigation_ui(ns(paste0(id, '_', x)))
                                               }),
                                        paste0(rv.process$config$steps)
    )
    
    
    
    #browser()
    # rv.process$config$ll.UI <- setNames(lapply(rv.process$config$steps,
    #                                        function(x){
    #                                          source(file.path('.', paste0('mod_', paste0(id, '_', x), '.R')), local=TRUE)
    #                                          mod_navigation_ui(ns(paste0(id, '_', x)))
    #                                          }),
    #                                 paste0('screen_', rv.process$config$steps)
    # )
    
    lapply(rv.process$config$steps, function(x){
      tmp.return[[x]] <- mod_navigation_server(id = paste0(id, '_', x) ,
                                               nav.mode = 'process',
                                               dataIn = reactive({ rv.child$data2send[[x]] }),
                                               is.enabled = reactive({isTRUE(rv.process$steps.enabled[x])}),
                                               remoteReset = reactive({rv.process$resetChildren[x]}),
                                               is.skipped = reactive({isTRUE(rv.process$steps.skipped[x])})
      )
    })
    
    mod_timeline_v_server(id = 'timelinev',
                          config =  rv.process$config,
                          status = reactive({rv.process$steps.status}),
                          position = reactive({rv.process$current.pos}),
                          enabled = reactive({rv.process$steps.enabled})
    )
  }
  
  
  
  
  observeEvent(id, {
    # Launch of the module process server
    cat(crayon::yellow(paste0("Launching ", paste0('mod_', id, '_server\n\n'))))
    #browser()
    
    switch(nav.mode,
           process = InitProcessServer(),
           pipeline = InitPipelineServer())
  }, priority=1000)
  
  
  # This function uses the UI definition to:
  # * initialize the UI (only the first screen is shown),
  # * encapsulate the UI in a div (used to hide all screens at a time before
  # showing the one corresponding to the current position)
  output$EncapsulateScreens_ui <- renderUI({
    EncapsulateScreens()
  })
  
  
  
  # This function displays a short message under a step if it is disabled
  output$SkippedInfoPanel <- renderUI({
    #if (verbose) cat(paste0(class(self)[1], '::output$SkippedInfoPanel from - ', self$id, "\n\n"))
    
    current_step_skipped <- rv.process$steps.status[rv.process$current.pos] == Magellan::global$SKIPPED
    #entire_process_skipped <- isTRUE(sum(rv.process$steps.status) == global$SKIPPED * rv.process$length)
    req(current_step_skipped)
    
    # This case appears when the process has been skipped from the
    # pipeline. Thus, it is not necessary to show the info box because
    # it is shown below the timeline of the pipeline
    #if (entire_process_skipped){}
    
    txt <- paste0("This ", rv.process$config$type, " is skipped so it has been disabled.")
    wellPanel(
      style = "background-color: #7CC9F0; opacity: 0.72; padding: 0px; align: center; vertical-align: center;",
      height = 100,
      width = 300,
      align = "center",
      p(style = "color: black;", paste0('Info: ',txt))
    )
  })
  
  # Catch the dataset returned by the process module. The event is observed by a change in the 'trigger' value
  # and instantiate the rv$dataOut variable
  # which is the return value of the module.
  # This function is only used to communicate between the process module and and the caller
  observeEvent(rv.process$proc$dataOut()$trigger, ignoreNULL = TRUE, ignoreInit = TRUE, {
    
    # If a value is returned, that is because the current is validated
    rv.process$steps.status[rv.process$current.pos] <- Magellan::global$VALIDATED
    
    #Look for new skipped steps
    Discover_Skipped_Steps()
    
    # If it is the first step (description step), then xxxx
    if (rv.process$current.pos==1)
      rv.process$dataIn <- rv.process$temp.dataIn
    else #if it is the last step of the process
      if (rv.process$current.pos == rv.process$length){
        #Update the work variable of the nav_process with the dataset returned by the process
        rv.process$dataIn <- rv.process$proc$dataOut()$value
        
        #Update the 'dataOut' reactive value to return this dataset to the caller
        # this nav_process is only a bridge between the process and the caller
        Send_Result_to_Caller()
        
        # dataOut$trigger <- rv.process$proc$dataOut()$trigger
        # dataOut$value <- rv.process$proc$dataOut()$value
      }
    
  })
  
  
  
  
  # @description
  # Catch the return value of a module and update the list of isDone modules
  # This list is updated with the names of datasets present in the rv$tmp
  # variable. One set to TRUE all the elements in isDone which have a corresponding
  # element in names(rv$tmp).
  # One cannot simply set to TRUE the last element of rv$tmp because it will does
  # not work in case of a reseted module (it is not in the names(rv$tmp) list
  # anymore)
  # If a value (not NULL) is received, then it corresponds to the module
  # pointed by the current position
  # This function also updates the list isDone
  # This function updates the current dataset (self$rv$dataIn)
  #
  # @return Nothing
  #
  ActionOn_Data_Trigger = function(){
    if(verbose) cat(crayon::yellow(paste0(id, '::ActionOn_Data_Trigger()\n\n')))
    #browser()
    processHasChanged <- newValue <- NULL
    return.trigger.values <- setNames(lapply(rv.process$config$steps, function(x){tmp.return[[x]]$dataOut()$trigger}),
                                      rv.process$config$steps)
    
    # Replace NULL values by NA
    return.trigger.values[sapply(return.trigger.values, is.null)] <- NA
    triggerValues <- unlist(return.trigger.values)
    
    
    return.values <- setNames(lapply(rv.process$config$steps, function(x){tmp.return[[x]]$dataOut()$value}),
                              rv.process$config$steps)
    
    cat(crayon::blue('--------------- Data received from children --------------------\n'))
    print(return.values)
    cat(crayon::blue('-------------------------------------------------------\n'))
    #browser()
    # if (sum(triggerValues)==0){ # Init of core engine
    #   rv.process$dataIn <- rv.process$temp.dataIn
    # } else
    if (is.null(unlist(return.values))) { # The entire pipeline has been reseted
      print('The entire pipeline has been reseted')
      rv.process$dataIn <- NULL
      rv.process$steps.status[seq_len(rv.process$length)] <- Magellan::global$UNDONE
    } else {
      processHasChanged <- rv.process$config$steps[which(max(triggerValues, na.rm = TRUE)==triggerValues)]
      ind.processHasChanged <- which(rv.process$config$steps==processHasChanged)
      newValue <- tmp.return[[processHasChanged]]$dataOut()$value
      
      if (is.null(newValue)){
        #browser()
        # A process has been reseted
        rv.process$steps.status[ind.processHasChanged:rv.process$length] <- Magellan::global$UNDONE
        rv.process$steps.enabled[ind.processHasChanged:rv.process$length] <- FALSE
        rv.process$steps.enabled[ind.processHasChanged] <- TRUE
        rv.process$steps.skipped[ind.processHasChanged:rv.process$length] <- FALSE
        
        #browser()
        # Reset all further steps also
        #ResetChildren(range = ind.processHasChanged:rv.process$length)
        #if (ind.processHasChanged < rv.process$length)
        #  rv.process$resetChildren[(1+ind.processHasChanged):rv.process$length] <- TRUE
        #rv.process$resetChildren[seq_len(ind.processHasChanged-1)] <- FALSE
        
        # Reset all further steps also
        #rv.child$reset[ind.processHasChanged:length(rv.process$config$steps)] <- TRUE
        #ResetChildren(range = ind.processHasChanged:rv.process$length)
        #rv.child$reset[seq_len(ind.processHasChanged-1)] <- FALSE
        
        # browser()
        # One take the last validated step (before the one corresponding to processHasChanges
        # but it is straightforward because we just updates self$rv$status
        ind.last.validated <- NULL
        validated.steps <- which(rv.process$steps.status == Magellan::global$VALIDATED)
        if (length(validated.steps) !=0)
          ind.last.validated <- max(validated.steps)
        
        #There is no validated step (the first step has been reseted)
        if(is.null(ind.last.validated) || ind.last.validated == 1)
          rv.process$dataIn <- rv.process$temp.dataIn
        else{
          name.last.validated <- rv.process$config$steps[ind.last.validated]
          dataIn.ind.last.validated <- which(names(rv.process$dataIn) == name.last.validated)
          #self$rv$dataIn <- self$rv$dataIn[ , , seq_len(dataIn.ind.last.validated)]
          rv.process$dataIn <- Keep_Datasets_from_Object(object = rv.process$dataIn, 
                                                         range = seq_len(dataIn.ind.last.validated))
          
        }
        #Update_State_Screens()
        # In this case, one force the update of the input dataset
        #PrepareData2Send()
      } else {
        # browser()
        # A process has been validated
        rv.process$steps.status[processHasChanged] <- Magellan::global$VALIDATED
        if (ind.processHasChanged < rv.process$length)
          rv.process$steps.status[(1 + ind.processHasChanged):rv.process$length] <- Magellan::global$UNDONE
        
        Discover_Skipped_Steps()
        rv.process$dataIn <- newValue
      }
      
    }
    
    # PrepareData2Send()
    Send_Result_to_Caller()
  }
  
  # Catches a new value of the cursor position
  observeEvent(req(!is.null(rv.process$position)), ignoreInit = TRUE, {
    pos <- strsplit(rv.process$position, '_')[[1]][1]
    if (pos == 'last')
      rv.process$current.pos <- rv.process$length
    else if (is.numeric(pos))
      rv.process$current.pos <- rv.process$position
  })
  
  
  
  # @description
  # xxx
  #
  # @return Nothing
  #
  ActionOn_NewPosition = function(){
    if(verbose) cat(crayon::yellow(paste0(id, '::ActionOn_NewPosition()\n\n')))
    
    # Send dataset to child process only if the current position is enabled
    #if(rv.process$steps.enabled[rv.process$current.pos])
    PrepareData2Send()
    #browser()
    # If the current step is validated, set the child current position to the last step
    if (rv.process$steps.status[rv.process$current.pos] == Magellan::global$VALIDATED)
      rv.child$position[rv.process$current.pos] <- paste0('last_', Timestamp())
  }
  
  
  
  # Catches a new position to show/hide the correct screen. This function
  # also manages the enabling/disabling of the `Prev` and `Next` buttons
  # w.r.t predefined rules (each of these buttons are disabled if there is
  # no more steps in their direction)
  observeEvent(rv.process$current.pos, ignoreInit = TRUE, {
    if (verbose) cat(crayon::yellow(paste0(id, '::observeEvent(rv.process$current.pos)\n\n')))
    
    
    if (nav.mode == 'process'){
    ToggleState_NavBtns()
    # Hide all screens 
    shinyjs::hide(selector = paste0(".page_", id))
    
    #Show the current step which is identified by its name. This point is very important
    # and need that the renderUI functions of the process to be strickly well named
    shinyjs::show(rv.process$config$steps[rv.process$current.pos])
    } else if (nav.mode == 'pipeline'){
      shinyjs::toggleState(id = "prevBtn", condition = rv.process$current.pos > 1)
      shinyjs::toggleState(id = "nextBtn", condition = rv.process$current.pos < rv.process$length)
      shinyjs::hide(selector = paste0(".page_", id))
      shinyjs::show(rv.process$config$steps[rv.process$current.pos])
      
      #Specific to pipeline code
      ActionOn_NewPosition()
    }
    
  })
  
  
  
  # Show/hide an information panel if the process is entirely skipped
  # This functions can be used for both nav_process and nav_pipeline modules
  output$SkippedInfoPanel <- renderUI({
    #if (verbose) cat(paste0(class(self)[1], '::output$SkippedInfoPanel from - ', self$id, "\n\n"))
    
    current_step_skipped <- rv.process$steps.status[rv.process$current.pos] == Magellan::global$SKIPPED
    req(current_step_skipped)
    process_entirely_skipped <- isTRUE(sum(rv.process$steps.status) == Magellan::global$SKIPPED * rv.process$length)
    
    if (process_entirely_skipped){
      # This case appears when the process has been skipped from the
      # pipeline. Thus, it is not necessary to show the info box because
      # it is shown below the timeline of the pipeline
    } else {
      txt <- paste0("This ", rv.process$config$type, " is skipped so it has been disabled.")
      wellPanel(
        style = "background-color: #7CC9F0; opacity: 0.72; padding: 0px; align: center; vertical-align: center;",
        height = 100,
        width=300,
        align="center",
        p(style = "color: black;", paste0('Info: ',txt))
      )
    }
  })
  
  
  
  
  
  # The following functions are only there for dev and debugging reasons
  # They will not be part of the final code
  
  output$show_Debug_Infos <- renderUI({
    tagList(
      h3(paste0('module process "', id, '"')),
      uiOutput(ns('show_tag_enabled')),
      fluidRow(
        column(width=2,
               tags$b(h4(style = 'color: blue;', paste0("dataIn() ", rv.process$config$type))),
               uiOutput(ns('show_dataIn'))),
        column(width=2,
               tags$b(h4(style = 'color: blue;', paste0("rv.process$dataIn ", rv.process$config$type))),
               uiOutput(ns('show_rv_dataIn'))),
        column(width=2,
               tags$b(h4(style = 'color: blue;', paste0("dataOut$value ", rv.process$config$type))),
               uiOutput(ns('show_rv_dataOut'))),
        column(width=4,
               tags$b(h4(style = 'color: blue;', "status")),
               uiOutput(ns('show_status')))
      )
    )
  })
  
  ###########---------------------------#################
  output$show_dataIn <- renderUI({
    # if (verbose) cat(grey(paste0(id, '::output$show_dataIn\n\n')))
    req(dataIn())
    tagList(
      # h4('show dataIn()'),
      lapply(names(dataIn()), function(x){tags$p(x)})
    )
  })
  
  # output$show_rv_dataIn <- renderUI({
  #   if (verbose) cat(paste0('::output$show_rv_dataIn from - ', id, "\n\n"))
  #   req(rv.process$dataIn)
  #   tagList(
  #     # h4('show dataIn()'),
  #     lapply(names(rv.process$dataIn), function(x){tags$p(x)})
  #   )
  # })
  
  output$show_rv_dataOut <- renderUI({
    req(dataOut$value)
    #if (verbose) cat(grey(paste0(id, '::output$show_rv_dataOut\n\n')))
    tagList(
      #h4('show dataOut$value'),
      lapply(names(dataOut$value), function(x){tags$p(x)})
    )
  })
  
  
  output$show_status <- renderUI({
    tagList(lapply(seq_len(rv.process$length), 
                   function(x){
                     color <- if(rv.process$steps.enabled[x]) 'black' else 'lightgrey'
                     if (x == rv.process$current.pos)
                       tags$p(style = paste0('color: ', color, ';'),
                              tags$b(paste0('---> ', rv.process$config$steps[x], ' - ', GetStringStatus(rv.process$steps.status[[x]])), ' <---'))
                     else 
                       tags$p(style = paste0('color: ', color, ';'),
                              paste0(rv.process$config$steps[x], ' - ', GetStringStatus(rv.process$steps.status[[x]])))
                   }))
  })
  
  output$show_tag_enabled <- renderUI({
    tagList(
      p(paste0('steps.enabled = ', paste0(as.numeric(rv.process$steps.enabled), collapse=' '))),
      p(paste0('enabled() = ', as.numeric(is.enabled())))
    )
  })
  
  
  
  output$show_rv_dataIn <- renderUI({
    # if (verbose) cat(paste0('::output$show_rv_dataIn from - ', id, "\n\n"))
    req(rv.process$dataIn)
    tagList(
      # h4('show dataIn()'),
      lapply(names(rv.process$dataIn), function(x){tags$p(x)})
    )
  })
  
  
  
  # The return value of the nav_process module server
  # The item 'dataOut' has been updated by the module process and it is returned to the
  # function that has called this nav_process module (it can be a module, a Shiny app or another nav module
  # for example, nav_pipeline)
  #  observeEvent(dataOut$trigger, { browser()})
  list(dataOut = reactive({dataOut}),
       steps.enabled = reactive({rv.process$steps.enabled}),
       status = reactive({rv.process$steps.status})
  )
  
  
  })
  
}
