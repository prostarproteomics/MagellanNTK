

#' @title xxx
#' 
#' @description 
#' # A process has changed
# Either it has returned a value (newValue contains a dataset)
# or it has been reseted (newValue is NULL)

#' 
#' @param temp.dataIn xxx
#' @param dataIn xxx
#' @param steps.info xxx
#' @param processHasChanged A character(1) which is the name of the process
#' which has changed its return value.
#' @param newValue The new value given by the step which has changed.
#' It can be either NULL (the process has been reseted) or contain
#' a dataset (the process has been validated and returned the result
#' of its calculations) 
#' 
#' @author Samuel Wieczorek
#'
#' @export
#' 
ActionOn_Child_Changed <- function(temp.dataIn,
                                   dataIn,
                                   steps.info,
                                   processHasChanged,
                                   newValue = NULL){
  # Indice of the dataset in the object
  # If the original length is not 1, then this indice is different
  # than the above one
  ind.processHasChanged <- which(rownames(steps.info)==processHasChanged)
  
  len <- nrow(steps.info)
  
  if (is.null(newValue)){
    # A process has been reseted
    
    # One take the last validated step (before the one 
    # corresponding to processHasChanges
    # but it is straightforward because we just updates rv$status
    steps.info[ind.processHasChanged:len, 'status'] <-  global$UNDONE
    
    steps.info[(ind.processHasChanged+1):len, 'enabled'] <- FALSE
    steps.info[ind.processHasChanged, 'enabled'] <- TRUE
    
    steps.info[ind.processHasChanged:len, 'skipped'] <- FALSE
    
    validated.steps <- which(steps.info$status == global$VALIDATED)
    if (length(validated.steps) > 0)
      ind.last.validated <- max(validated.steps)
    else 
      ind.last.validated <- 0
    
    # There is no validated step (the first step has been reseted)
    if(ind.last.validated %in% c(0,1))
      dataIn <- temp.dataIn
    
    else {
      name.last.validated <- rownames(steps.info)[ind.last.validated]
      dataIn.ind.last.validated <- which(names(dataIn) == name.last.validated)
      dataIn <- Keep_Datasets_from_Object(object = dataIn, 
                                          range = seq_len(dataIn.ind.last.validated))
      }
    
  } else {
    # A process has been validated
    steps.info[processHasChanged, 'status'] <- global$VALIDATED
    
    if (ind.processHasChanged < len)
      steps.info[(1 + ind.processHasChanged):len, 'status'] <- global$UNDONE
    
    steps.info$status <- Discover_Skipped_Steps(steps.info$status)
    dataIn <- newValue
  }
  

  return(
    list(dataIn = dataIn,
         steps.info = steps.info
         )
  )
  }



#' @title xxxx
#' @description xxx
#'
#' @param config xxxx
#' @param tmp.return xxx
#' 
#' @export
#' 
GetValuesFromChildren <- function(config,
                                  tmp.return
                                  ){
  # Get the trigger values for each steps of the module
  return.trigger.values <- setNames(lapply(config$steps, function(x){tmp.return[[x]]$dataOut()$trigger}),
                                    nm = config$steps)
  
  # Replace NULL values by NA
  return.trigger.values[sapply(return.trigger.values, is.null)] <- NA
  triggerValues <- unlist(return.trigger.values)
  
  
  # Get the values returned by each step of the modules
  return.values <- setNames(lapply(config$steps, function(x){tmp.return[[x]]$dataOut()$value}),
                            nm = config$steps)
  
  
list(triggers = triggerValues,
     values = unlist(return.values)
     )  
  }


#' @title xxxx
#' @description xxx



#' @title Update Data to send
#' 
#' @description xxx
#'
#' @param current.pos xxx
#' @param steps.info xxx
#' @param temp.dataIn xxx
#' @param dataIn xxx
#' @param original.length xxx
#'
#' @export
#' 
#' 
Update_Data2send_Vector <- function(current.pos,
                                    steps.info,
                                    temp.dataIn,
                                    dataIn,
                                    original.length){
  # One only update the current position because the vector has been entirely
  # initialized to NULL so the other processes are already ready to be sent
  ind.last.validated <- GetMaxValidated_BeforePos(current.pos = current.pos,
                                                  steps.status = steps.info$status)
  if (is.null(ind.last.validated))
    data <- temp.dataIn
  else
    data <- Keep_Datasets_from_Object(object = dataIn,
                                      range = seq_len(ind.last.validated + original.length -1)
    )
  return(data)
}




#' @title Prepare Data to Send
#' @description xxx
#'
#' @param steps.info xxx
#' @param temp.dataIn xxx
#' @param dataIn xxx
#' @param config xxx
#' @param current.pos xxx
#' @param original.length xxx
#' 
#' @export
#' 
PrepareData2Send <- function(steps.info,
                             temp.dataIn,
                             dataIn,
                             config,
                             current.pos,
                             original.length){
  # Returns NULL to all modules except the one pointed by the current position
  # Initialization of the pipeline : one send dataIn() to the
  # first module
  
  # The dataset to send is contained in the variable 'rv$dataIn'
  
  
  
  # Initialize vector to all NULL values
  data2send <- setNames(
    lapply(config$steps, function(x){NULL}),
    nm = config$steps)
  
  if (is.null(dataIn)){ # Init of core engine
    
    # Only the first process will receive the data
    data2send[[1]] <- temp.dataIn
    
    # The other processes are by default disabled.
    # If they have to be enabled, they will be by another function later
    lapply(seq_len(length(config$steps)), function(x){
      steps.info[x, 'enabled'] <- x == 1
    })
    
  } else{
    current.step.name <- config$steps[current.pos]
    data2send[[current.step.name]] <- Update_Data2send_Vector(current.pos = current.pos,
                                                              steps.info = steps.info,
                                                              temp.dataIn = temp.dataIn,
                                                              dataIn = dataIn,
                                                              original.length = original.length)
  }
  
  if (verbose) {
    cat('<----------------- Data sent to children ------------------> \n')
    print(data2send)
    cat('<----------------------------------------------------> \n')
  }
  
  return(
    list(data2send = data2send,
         steps.enabled = steps.info$enabled
         )
  )
}


