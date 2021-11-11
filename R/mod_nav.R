#' @title The ui() function of the module `mod_nav`
#' 
#' @param id A `character(1)` which defines the id of the module. It is the same
#' as for the server() function.
#' 
#' @rdname mod_nav
#' 
#' @author Samuel Wieczorek
#' 
#' @export
#'
mod_nav_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    uiOutput(ns('nav_mod_ui')),
    mod_Debug_Infos_ui(ns('debug_infos'))
  )
}








#' @title The server() function of the module `mod_nav`
#' 
#' @description The module navigation can be launched via a Shiny app.
#' 
#' @param id A `character(1)` which defines the id of the module. It is the same
#' as for the ui() function.
#' 
#' @param nav.mode A `character(1)` indicating the type of workflow. It can be
#' either 'process' (for a simple workflow) or 'pipeline' (for a composed 
#' workflow). Default is NULL: a value is necessary.
#' 
#' @param dataIn The dataset
#' 
#' @param is.enabled A `boolean`. This variable is a remote command to specify
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
#' * status A vector of `integer(1)` of the same length than the config$steps
#'   vector
#' * reset xxxx
#' 
#' @export
#' 
#' @rdname mod_nav
#' 
#' @examples
#' \dontrun{
#' library(shiny)
#' library(shinyBS)
#' ui <- fluidPage(
#'   mod_nav_ui('Protein_Description')
#' )
#' server <- function(input, output){
#'   mod_nav_server(id = 'Protein_Description',
#'   nav.mode = 'process',
#'   dataIn = reactive({feat1})
#'   )
#' }
#' shinyApp(ui, server)
#' }
#' 
#' @importFrom stats setNames
#' 
mod_nav_server <- function(id,
                           nav.mode = NULL,
                           dataIn = reactive({NULL}),
                           is.enabled = reactive({TRUE}),
                           remoteReset = reactive({FALSE}),
                           is.skipped = reactive({FALSE})
                           ){
  
  verbose <- FALSE
  
  ###-------------------------------------------------------------###
  ###                                                             ###
  ### ------------------- MODULE SERVER --------------------------###
  ###                                                             ###
  ###-------------------------------------------------------------###
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    observe({
      if (is.null(nav.mode) || !(nav.mode %in% c('process', 'pipeline'))){
        warning("'nav.mode' must be either 'process' or 'pipeline'.")
        return(NULL)
      }
    }, priority = 10000)
    

    #source(system.file('extdata/general_nav_funcs.R', package='Magellan'), local = TRUE)$value
    
    
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
    
    
    
    
    
    
    
    
    
    observeEvent(input$closeModal, {removeModal() })
    
    observeEvent(input$prevBtn, ignoreInit = TRUE, {
      rv$current.pos <- NavPage(direction = -1,
                                current.pos = rv$current.pos,
                                len = rv$length
      )
    })
    
    
    observeEvent(input$nextBtn, ignoreInit = TRUE, {
      rv$current.pos <- NavPage(direction = 1,
                                current.pos = rv$current.pos,
                                len = rv$length
      )
    })
    
    
    observeEvent(dataIn(), ignoreNULL = FALSE, ignoreInit = FALSE,{
      #observe({
      if (verbose) cat(yellow(paste0(id, "::observe(dataIn())\n\n")))
      #browser()
      isolate({
        # A new value on dataIn() means a new dataset sent to the process
        rv$current.pos <- 1
        
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
          ToggleState_Screens(cond = FALSE, 
                              range = seq_len(rv$length), 
                              steps.status = rv$steps.status, 
                              is.enabled = is.enabled()
          )
        } else { # A new dataset has been loaded
          cat(blue('In observe(dataIn()) : dataIn() is not NULL\n\n'))
          # Update the different screens in the process
          Update_State_Screens(is.skipped = is.skipped(),
                               len = rv$length,
                               steps.status = rv$steps.status, 
                               is.enabled = is.enabled(),
                               current.pos = rv$current.pos,
                               mandatory = rv$config$mandatory
          )
        }
        
        # Update the initial length of the dataset with the length
        # of the one that has been received
        rv$original.length <- length(dataIn())
        
        
        # Enable the first screen
        ToggleState_Screens(cond = TRUE, 
                            range = 1, 
                            steps.status = rv$steps.status, 
                            is.enabled = is.enabled()
        )
      })
    })
    
    
    
    observeEvent(is.enabled(), ignoreNULL = TRUE, ignoreInit = TRUE, {
      if (verbose) cat(yellow(paste0(id, '::is.enabled()\n\n')))
      if (isTRUE(is.enabled())){
        Update_State_Screens(is.skipped = is.skipped(),
                             len = rv$length,
                             steps.status = rv$steps.status, 
                             is.enabled = is.enabled(),
                             current.pos = rv$current.pos,
                             mandatory = rv$config$mandatory
        )
      } else {
        rv$steps.enabled <- setNames(rep(is.enabled(), rv$length), 
                                     rv$config$steps)
      }
    })
    
    
    
    observeEvent(rv$steps.status, ignoreInit = TRUE, {
      # https://github.com/daattali/shinyjs/issues/166
      # https://github.com/daattali/shinyjs/issues/25
      if (verbose) cat(yellow(paste0(id, '::observeEvent(rv$steps.status)\n\n')))
      
      rv$steps.status <- Discover_Skipped_Steps(len = rv$length,
                                                steps.status = rv$steps.status
      )
      Update_State_Screens(is.skipped = is.skipped(),
                           len = rv$length,
                           steps.status = rv$steps.status, 
                           is.enabled = is.enabled(),
                           current.pos = rv$current.pos,
                           mandatory = rv$config$mandatory
      )
      if (rv$steps.status[rv$length] == global$VALIDATED){
        rv$current.pos <- rv$length
        dataOut <- Send_Result_to_Caller(rv$dataIn)
      }
    })
    
    
    
    
    observeEvent(is.skipped(), ignoreNULL = FALSE, ignoreInit = TRUE,{
      # Catches a new value on the remote parameter `Reset`. A TRUE value indicates
      # that the caller program wants this module to reset itself.
      if (verbose) 
        cat(yellow(paste0(id, '::observeEvent(is.skipped()). Value = ', 
                          is.skipped(), 
                          '\n\n')))
      
      if (isTRUE(is.skipped()))
        Set_All_Skipped(len = rv$length, 
                        steps.status = rv$steps.status
        )
      else{
        rv$steps.status <- Unskip_All_Steps(len = rv$length, 
                                            steps.status = rv$steps.status
        )
        Update_State_Screens(len = rv$length, 
                             steps.status = rv$steps.status,
                             is.skipped = is.skipped(),
                             is.enabled = is.enabled(),
                             current.pos = rv$current.pos,
                             mandatory = rv$config$mandatory
        )
      }
    })
    
    
    observeEvent(remoteReset(), ignoreInit = TRUE, {
      if (verbose) 
        cat(yellow(paste0(id, '::observeEvent(remoteReset()). Value = ', 
                          remoteReset(), 
                          '\n\n')))
      
      res <- LocalReset(steps = rv$config$steps, 
                        nav.mode = nav.mode
      )
      
      rv$dataIn <- res$dataIn
      rv$dataOut <- res$dataOut
      rv$current.pos <- res$current.pos
      rv$steps.status <- res$steps.status
    })
    
    
    
    
    
    
    
    observeEvent(input$rstBtn, ignoreInit = TRUE, {
      if (verbose) 
        cat(yellow(paste0('::observeEvent(input$rstBtn) from - ', 
                          id, 
                          '\n\n')))
      showModal(dataModal(nav.mode = nav.mode))
    })
    
    
    
    
    observeEvent(input$modal_ok, ignoreInit=FALSE, ignoreNULL = TRUE, {
      if (verbose) cat(yellow(paste0(id, '::observeEvent(input$modal_ok)\n\n')))
      
      res <- LocalReset(steps = rv$config$steps, 
                        nav.mode = nav.mode
      )
      
      rv$dataIn <- res$dataIn
      rv$dataOut <- res$dataOut
      rv$current.pos <- res$current.pos
      rv$steps.status <- res$steps.status
      
      removeModal()
    })
    
    
    
    
    
    
    
    
    
    output$SkippedInfoPanel <- Build_SkippedInfoPanel(rv$steps.status, rv$current.pos, rv$config)
    output$EncapsulateScreens_ui <- Build_EncapsulateScreens_ui(steps = rv$config$steps,
                                                                ll.UI = rv$config$ll.UI
                                                                )
    
    switch (nav.mode,
            pipeline = {
              observeEvent(rv$current.pos, ignoreInit = TRUE, {
                if (verbose) cat(paste0(id, '::observeEvent(rv$current.pos)\n\n'))
                
                shinyjs::toggleState(id = 'prevBtn', condition = rv$current.pos > 1)
                shinyjs::toggleState(id = 'nextBtn', condition = rv$current.pos < rv$length)
                shinyjs::hide(selector = paste0('.page_', id))
                shinyjs::show(rv$config$steps[rv$current.pos])
                
                #Specific to pipeline code
                ActionOn_NewPosition()
                
              })
              
              
              output$pipeline_ui <- Build_pipeline_ui()
              

              # Catch any event on the 'id' parameter. As this parameter change only
              # when the server is created, this function can be view as the initialization
              # of the server
              observeEvent(id, {
                # The package containing the code for processes is supposed to be
                # already launched. Just check if the server module is ok
                # if (!exists('mod_Protein_server', where='package:DaparToolshed', mode='function')){
                #   warning('This pipeline is not available in DaparToolshed')
                #   return(NULL)
                # }
                req(rv$config$nav.mode == 'pipeline')
                rv$current.pos <- 1
                
                
                # Call the server module of the pipeline which name is the parameter 'id'
                # This will give access to its config
                rv$proc <- do.call(paste0('mod_', id, '_server'),
                                   list(id = id,
                                        dataIn = reactive({rv$temp.dataIn}),
                                        steps.enabled = reactive({rv$steps.enabled}),
                                        remoteReset = reactive({FALSE}),
                                        steps.status = reactive({rv$steps.status})
                                   )
                )
                
                
                
                
                
                # Update the reactive value config with the config of the pipeline
                rv$config <- rv$proc$config()
                
                # TODO Write the CheckPipelineConfig function
                # Check if the config variable is correct
                # check <- CheckPipelineConfig(rv$config)
                # if (!check$passed)
                #   stop(paste0(\"Errors in 'rv$config'\", paste0(check$msg, collapse=' ')))
                # 
                
                
                rv$length <- length(rv$config$steps)
                
                # Get the name of the parent of the process
                # The id variable is composed of two ids separate by '_'. The first id correspond to the parent
                # and the second correspond to the child in the process hierarchy
                rv$parent.name <- unlist(strsplit(id, split='_'))[1]
                rv$child.name <- unlist(strsplit(id, split='_'))[2]
                
                
                rv$config$mandatory <- setNames(rv$config$mandatory, rv$config$steps)
                rv$steps.status = setNames(rep(global$UNDONE, rv$length), rv$config$steps)
                rv$currentStepName <- reactive({rv$config$steps[rv$current.pos]})
                
                rv$steps.enabled <- setNames(rep(FALSE, length(rv$config$steps)), rv$config$steps)
                rv$steps.skipped <- setNames(rep(FALSE, length(rv$config$steps)), rv$config$steps)
                rv$resetChildren <- setNames(rep(0, length(rv$config$steps)), rv$config$steps)
                
                rv.child$data2send <- setNames(lapply(as.list(rv$config$steps), function(x) NULL), 
                                               nm = rv$config$steps)
                
                # Launch the ui for each step of the pipeline
                # This function could be stored in the source file of the pipeline
                # but the strategy is to insert minimum extra code in the files for
                # pipelines and processes. This is useful when other devs will
                # develop other pipelines and processes. Tus, it will be easier.
                rv$config$ll.UI <- setNames(lapply(rv$config$steps,
                                                   function(x){
                                                     mod_nav_process_ui(ns(paste0(id, '_', x)))
                                                   }),
                                            paste0(rv$config$steps)
                )
                
                
                
                #browser()
                # rv$config$ll.UI <- setNames(lapply(rv$config$steps,
                #                                        function(x){
                #                                          source(file.path('.', paste0('mod_', paste0(id, '_', x), '.R')), local=TRUE)
                #                                          mod_nav_process_ui(ns(paste0(id, '_', x)))
                #                                          }),
                #                                 paste0('screen_', rv$config$steps)
                # )
                
                lapply(rv$config$steps, function(x){
                  tmp.return[[x]] <- mod_nav_process_server(id = paste0(id, '_', x) ,
                                                            dataIn = reactive({ rv.child$data2send[[x]] }),
                                                            is.enabled = reactive({isTRUE(rv$steps.enabled[x])}),
                                                            remoteReset = reactive({rv$resetChildren[x]}),
                                                            is.skipped = reactive({isTRUE(rv$steps.skipped[x])})
                  )
                })
                
                mod_timeline_v_server(id = 'timelinev',
                                      config =  rv$config,
                                      status = reactive({rv$steps.status}),
                                      position = reactive({rv$current.pos}),
                                      enabled = reactive({rv$steps.enabled})
                )
                
              }, priority=1000) 
              
              
              
              
              
              
              
              
              
              ActionOn_Data_Trigger = function(){
                if(verbose) cat(crayon::yellow(paste0(id, '::ActionOn_Data_Trigger()\n\n')))
                #browser()
                processHasChanged <- newValue <- NULL
                return.trigger.values <- setNames(lapply(rv$config$steps, function(x){tmp.return[[x]]$dataOut()$trigger}),
                                                  rv$config$steps)
                
                # Replace NULL values by NA
                return.trigger.values[sapply(return.trigger.values, is.null)] <- NA
                triggerValues <- unlist(return.trigger.values)
                
                
                return.values <- setNames(lapply(rv$config$steps, function(x){tmp.return[[x]]$dataOut()$value}),
                                          rv$config$steps)
                
                cat(crayon::blue('--------------- Data received from children --------------------\n'))
                print(return.values)
                cat(crayon::blue('-------------------------------------------------------\n'))
                #browser()
                # if (sum(triggerValues)==0){ # Init of core engine
                #   rv$dataIn <- rv$temp.dataIn
                # } else
                if (is.null(unlist(return.values))) { # The entire pipeline has been reseted
                  print('The entire pipeline has been reseted')
                  rv$dataIn <- NULL
                  rv$steps.status[seq_len(rv$length)] <- Magellan::global$UNDONE
                } else {
                  processHasChanged <- rv$config$steps[which(max(triggerValues, na.rm = TRUE)==triggerValues)]
                  ind.processHasChanged <- which(rv$config$steps==processHasChanged)
                  newValue <- tmp.return[[processHasChanged]]$dataOut()$value
                  
                  if (is.null(newValue)){
                    #browser()
                    # A process has been reseted
                    rv$steps.status[ind.processHasChanged:rv$length] <- Magellan::global$UNDONE
                    rv$steps.enabled[ind.processHasChanged:rv$length] <- FALSE
                    rv$steps.enabled[ind.processHasChanged] <- TRUE
                    rv$steps.skipped[ind.processHasChanged:rv$length] <- FALSE
                    
                    #browser()
                    # Reset all further steps also
                    #ResetChildren(range = ind.processHasChanged:rv$length)
                    #if (ind.processHasChanged < rv$length)
                    #  rv$resetChildren[(1+ind.processHasChanged):rv$length] <- TRUE
                    #rv$resetChildren[seq_len(ind.processHasChanged-1)] <- FALSE
                    
                    # Reset all further steps also
                    #rv.child$reset[ind.processHasChanged:length(rv$config$steps)] <- TRUE
                    #ResetChildren(range = ind.processHasChanged:rv$length)
                    #rv.child$reset[seq_len(ind.processHasChanged-1)] <- FALSE
                    
                    # browser()
                    # One take the last validated step (before the one corresponding to processHasChanges
                    # but it is straightforward because we just updates self$rv$status
                    ind.last.validated <- NULL
                    validated.steps <- which(rv$steps.status == Magellan::global$VALIDATED)
                    if (length(validated.steps) !=0)
                      ind.last.validated <- max(validated.steps)
                    
                    #There is no validated step (the first step has been reseted)
                    if(is.null(ind.last.validated) || ind.last.validated == 1)
                      rv$dataIn <- rv$temp.dataIn
                    else{
                      name.last.validated <- rv$config$steps[ind.last.validated]
                      dataIn.ind.last.validated <- which(names(rv$dataIn) == name.last.validated)
                      #self$rv$dataIn <- self$rv$dataIn[ , , seq_len(dataIn.ind.last.validated)]
                      rv$dataIn <- Keep_Datasets_from_Object(object = rv$dataIn, 
                                                             range = seq_len(dataIn.ind.last.validated))
                      
                    }
                    #Update_State_Screens()
                    # In this case, one force the update of the input dataset
                    #PrepareData2Send()
                  } else {
                    # browser()
                    # A process has been validated
                    rv$steps.status[processHasChanged] <- Magellan::global$VALIDATED
                    if (ind.processHasChanged < rv$length)
                      rv$steps.status[(1 + ind.processHasChanged):rv$length] <- Magellan::global$UNDONE
                    
                    rv$steps.status <- Discover_Skipped_Steps(len = rv$length,
                                                              steps.status = rv$steps.status
                    )
                    rv$dataIn <- newValue
                  }
                  
                }
                
                # PrepareData2Send()
                dataOut <- Send_Result_to_Caller(rv$dataIn)
              }
              
              
              
              
              
              PrepareData2Send = function(){
                if(verbose) cat(paste0(id, '::PrepareData2Send()\n\n'))
                #browser()
                # Returns NULL to all modules except the one pointed by the current position
                # Initialization of the pipeline : one send dataIn() to the
                # first module
                
                # The dataset to send is contained in the variable 'rv$dataIn'
                
                
                
                # Initialize vector to all NULL values
                rv.child$data2send <- setNames(
                  lapply(rv$config$steps, function(x){NULL}),
                  rv$config$steps)
                
                if (is.null(rv$dataIn)){ # Init of core engine
                  
                  # Only the first process will receive the data
                  rv.child$data2send[[1]] <- rv$temp.dataIn
                  
                  # The other processes are by default disabled.
                  # If they have to be enabled, they will be by another function later
                  lapply(seq_len(rv$length), function(x){
                    rv$steps.enabled[x] <- x==1
                  })
                  
                } else
                  rv.child$data2send[[CurrentStepName(rv$current.pos, rv$config$steps)]] <- Update_Data2send_Vector()
                
                cat(crayon::blue('<----------------- Data sent to children ------------------> \n'))
                print(rv.child$data2send)
                cat(crayon::blue('<----------------------------------------------------> \n'))
              }
              
              
              
              
              
              
              
              ActionOn_NewPosition = function(){
                req(rv$config$nav.mode == 'pipeline')
                if(verbose) cat(crayon::yellow(paste0(id, '::ActionOn_NewPosition()\n\n')))
                
                # Send dataset to child process only if the current position is enabled
                #if(rv$steps.enabled[rv$current.pos])
                PrepareData2Send()
                #browser()
                # If the current step is validated, set the child current position to the last step
                if (rv$steps.status[rv$current.pos] == Magellan::global$VALIDATED)
                  rv.child$position[rv$current.pos] <- paste0('last_', Timestamp())
              }
              
              
              
              Update_Data2send_Vector = function(){
                # One only update the current position because the vector has been entirely
                # initialized to NULL so the other processes are already ready to be sent
                ind.last.validated <- GetMaxValidated_BeforePos(current.pos = xxx,
                                                                pos = xxx,
                                                                steps.status = xxx)
                if (is.null(ind.last.validated))
                  data <- rv$temp.dataIn
                else
                  data <- Keep_Datasets_from_Object(object = rv$dataIn,
                                                    range = seq_len(ind.last.validated + rv$original.length -1)
                  )
                return(data)
              }
              
              
              
              
              # Catch the returned values of the process                                                           
              observeEvent(lapply(rv$config$steps, 
                                  function(x){
                                    tmp.return[[x]]$dataOut()$trigger}), ignoreInit = TRUE, {
                                      if(verbose) cat(paste0('observeEvent(trigger) from - ', id, '\n\n'))
                                      #browser()
                                      ActionOn_Data_Trigger()
                                    })
              
              
              
              
              # Used to store the return values (lists) of child processes
              tmp.return <- reactiveValues()
              
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
              
              
              },
            process = {
              
              observeEvent(rv$current.pos, ignoreInit = TRUE, {
                if (verbose) cat(yellow(paste0(id, '::observeEvent(rv$current.pos)\n\n')))
                
                ToggleState_NavBtns(current.pos = current.pos,
                                    len = len
                                    )
                # Hide all screens 
                shinyjs::hide(selector = paste0('.page_', id))
                
                #Show the current step which is identified by its name. This point is very important
                # and need that the renderUI functions of the process to be strickly well named
                shinyjs::show(rv$config$steps[rv$current.pos])
                
              })
              
              
              output$process_ui <- Build_process_ui()
              
              
              observeEvent(id, ignoreNULL = TRUE, {
                # Launch of the module process server
                cat(yellow(paste0("Launching ", paste0('mod_', id, '_server\n\n'))))
                rv$current.pos  <- 1
                
                if (!exists(paste0('mod_', id, '_server')) || !exists(paste0('mod_', id, '_ui')))
                  stop('Cannot found the module functions.')
                
                
                # Call the module server of the process
                # The 'dataIn' parameter correspond to the dataset passed to this nav_process server
                # more specifically, the temporary variable
                # The parameter 'steps.enabled' is xxxx
                # The parameter 'remoteReset' send to the process module the information that it has to 
                # be reseted. It is the sum of the input$rstBtn (the local reset button of the nav_process) and
                # the remoteReset() variable which correspond to the reset button of the container of
                # the nav process (ie the mod_nav_pipeline)
                rv$proc <- do.call(paste0('mod_', id, '_server'),
                                   list(id = id,
                                        dataIn = reactive({rv$temp.dataIn}),
                                        steps.enabled = reactive({rv$steps.enabled}),
                                        remoteReset = reactive({input$rstBtn + remoteReset()}),
                                        current.pos = reactive({rv$current.pos})
                                   )
                )
                
                # Instantiate the local variables
                # Get the config variable from the process that has been called
                # This config contains all the UI for the each steps (config$ll.UI)
                # and the dataset returned by the process (config$dataOut)
                rv$config <- rv$proc$config()
                
                # Check if the config variable is correct
                check <- CheckConfig(rv$config)
                if (!check$passed)
                  stop(paste0("Errors in 'rv$config'", paste0(check$msg, collapse=' ')))
                
                
                
                rv$length <- length(rv$config$steps)
                
                # Get the name of the parent of the process
                # The id variable is composed of two ids separate by '_'. The first id correspond to the parent
                # and the second correspond to the child in the process hierarchy
                rv$parent <- unlist(strsplit(id, split='_'))[1]
                
                
                rv$config$mandatory <- setNames(rv$config$mandatory, rv$config$steps)
                rv$steps.status <- setNames(rep(global$UNDONE, rv$length), rv$config$steps)
                rv$currentStepName <- reactive({rv$config$steps[rv$current.pos]})
                rv$steps.enabled <- setNames(rep(FALSE, rv$length), rv$config$steps)
                
                
                # Launch the horizontal timeline server
                # The parameter 'config' is used to xxx
                # The parameter 'status' is used to color the bullets
                # the parameter 'position' is used to put the cursor at the current position
                # The parameter 'enabled' is used to modify the bullets whether the corresponding step is enabled or disabled
                mod_timeline_h_server(id = 'timeline',
                                      config =  rv$config,
                                      status = reactive({rv$steps.status}),
                                      position = reactive({rv$current.pos}),
                                      enabled = reactive({rv$steps.enabled})
                                      )
                
                
              }, priority=1000) 
              
              
              observeEvent(rv$proc$dataOut()$trigger, ignoreNULL = TRUE, ignoreInit = TRUE, {
                req(rv$config$nav.mode == 'process')
                # If a value is returned, that is because the current is validated
                rv$steps.status[rv$current.pos] <- global$VALIDATED
                
                #Look for new skipped steps
                rv$steps.status <- Discover_Skipped_Steps(len = rv$length,
                                                          steps.status = rv$steps.status
                                                          )
                
                # If it is the first step (description step), then xxxx
                if (rv$current.pos==1)
                  rv$dataIn <- rv$temp.dataIn
                else #if it is the last step of the process
                  if (rv$current.pos == rv$length){
                    #Update the work variable of the nav_process with the dataset returned by the process
                    rv$dataIn <- rv$proc$dataOut()$value
                    
                    #Update the 'dataOut' reactive value to return this dataset to the caller
                    # this nav_process is only a bridge between the process and the caller
                    dataOut <- Send_Result_to_Caller(rv$dataIn)
                  }
                
              })
              
              
              
              observeEvent(req(!is.null(rv$position)), ignoreInit = TRUE, {
                pos <- strsplit(rv$position, '_')[[1]][1]
                if (pos == 'last')
                  rv$current.pos <- rv$length
                else if (is.numeric(pos))
                  rv$current.pos <- rv$position
              })
              
              
              }
        )

    
    
    # Launch the renderUI function for the user interface of the module
    # Apparently, the renderUI() cannot be stored in the expression
    output$nav_mod_ui <- renderUI({
      uiOutput(ns(paste0(nav.mode, '_ui')))
    })

    mod_Debug_Infos_server(id = 'debug_infos',
                           title = paste0('Infos from pipeline : ', id),
                           config = reactive({rv$config}),
                           rv.dataIn = reactive({rv$dataIn}),
                           dataIn = reactive({dataIn()}),
                           dataOut = reactive({dataOut}),
                           steps.status = reactive({rv$steps.status}),
                           current.pos = reactive({ rv$current.pos}),
                           steps.enabled = reactive({rv$steps.enabled}),
                           is.enabled = reactive({is.enabled()}))
    
    # The return value of the nav_process module server
    # The item 'dataOut' has been updated by the module process and it is returned to the
    # function that has called this nav_process module (it can be a module, a Shiny app or another nav module
    # for example, nav_pipeline)
    list(dataOut = reactive({dataOut}),
         steps.enabled = reactive({rv$steps.enabled}),
         status = reactive({rv$steps.status})
    )
    
    
  })
  
}