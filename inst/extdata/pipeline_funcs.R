observeEvent(rv$current.pos, ignoreInit = TRUE, {
      if (verbose) cat(paste0(id, '::observeEvent(rv$current.pos)\n\n'))
      
      shinyjs::toggleState(id = 'prevBtn', condition = rv$current.pos > 1)
      shinyjs::toggleState(id = 'nextBtn', condition = rv$current.pos < rv$length)
      shinyjs::hide(selector = paste0('.page_', id))
      shinyjs::show(rv$config$steps[rv$current.pos])
      
      #Specific to pipeline code
      ActionOn_NewPosition()
      
    })

Build_pipeline_ui <- function(){
  renderUI({
  
  tagList(
        fluidRow(
        column(width=2, 
               wellPanel(
                 div(style = 'padding: 10px',
                     div(style = btn_style,
                         shinyjs::disabled(
                           actionButton(ns('prevBtn'), '<<',
                                        class = PrevNextBtnClass,
                                        style='padding:4px; font-size:80%')
                         ),
                         actionButton(ns('rstBtn'), 'Reset',
                                      class = redBtnClass,
                                      style='padding:4px; font-size:80%')
                     ),
                     div(style = btn_style,
                         actionButton(ns('nextBtn'),'>>',
                                      class = PrevNextBtnClass,
                                      style='padding:4px; font-size:80%')
                     ),
                     mod_timeline_v_ui(ns('timelinev'))
                 )
               )),
        column(width=10,
               style=' padding-left: 20px;',
               wellPanel(
                 div(id = ns('Screens'),
                     uiOutput(ns('SkippedInfoPanel')),
                     uiOutput(ns('EncapsulateScreens_ui'))
                     
                 )
               )
               )
        
      )
      )
    
})
  
}




ResetChildren = function(range){
  if(verbose) cat(paste0(id, '::ResetChildren()\n\n'))
 
  rv$resetChildren[range] <- 1 + rv$resetChildren[range]
  }





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

