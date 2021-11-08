#' @title xxx
#' 
#' @description
#' et to skipped all steps of the current object
#' 
#' @author Samuel Wieczorek
#' 
#' @return Source code for function ResetChildren
#' 
GetCode_ResetChildren <- function(){
  code.string <- "
  
  ResetChildren = function(range){
  if(verbose) cat(paste0(id, '::ResetChildren()\n\n'))
 
  rv$resetChildren[range] <- 1 + rv$resetChildren[range]
  }
  
"
  code.string 
  
}




GetCode_InitPipelineServer <- function(){
  
  code <- "
  
  InitPipelineServer = function(){
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
  rv$current.pos <- 1
  
  # Get the name of the parent of the process
  # The id variable is composed of two ids separate by '_'. The first id correspond to the parent
  # and the second correspond to the child in the process hierarchy
  rv$parent.name <- unlist(strsplit(id, split='_'))[1]
  rv$child.name <- unlist(strsplit(id, split='_'))[2]
  
  
  rv$config$mandatory <- setNames(rv$config$mandatory, rv$config$steps)
  rv$steps.status = setNames(rep(Magellan::global$UNDONE, rv$length), rv$config$steps)
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
                                       mod_navigation_ui(ns(paste0(id, '_', x)))
                                     }),
                              paste0(rv$config$steps)
  )
  
  
  
  #browser()
  # rv$config$ll.UI <- setNames(lapply(rv$config$steps,
  #                                        function(x){
  #                                          source(file.path('.', paste0('mod_', paste0(id, '_', x), '.R')), local=TRUE)
  #                                          mod_navigation_ui(ns(paste0(id, '_', x)))
  #                                          }),
  #                                 paste0('screen_', rv$config$steps)
  # )
  
  lapply(rv$config$steps, function(x){
    tmp.return[[x]] <- mod_navigation_server(id = paste0(id, '_', x) ,
                                             nav.mode = 'process',
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
}

"
code 
}







GetCode_ActionOn_Data_Trigger <- function(){
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
  code.string <- "
  
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
        
        Discover_Skipped_Steps()
        rv$dataIn <- newValue
      }
      
    }
    
    # PrepareData2Send()
    Send_Result_to_Caller()
  }
  
"
  code.string 
}



GetCode_PrepareData2Send <- function(){
# @description
# This function calls the server part of each module composing the pipeline
#
# @return Nothing
#
  code.string <- "
  
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
    rv.child$data2send[[CurrentStepName()]] <- Update_Data2send_Vector()
  
  cat(crayon::blue('<----------------- Data sent to children ------------------> \n'))
  print(rv.child$data2send)
  cat(crayon::blue('<----------------------------------------------------> \n'))
}

"

code.string 
}