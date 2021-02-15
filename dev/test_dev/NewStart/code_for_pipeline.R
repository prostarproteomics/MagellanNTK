
#source(file.path('.', 'mod_timeline_v.R'), local=TRUE)$value
source(file.path('.', 'mod_timeline_v.R'), local=TRUE)$value

source(file.path('.', 'commonFuncs.R'), local=TRUE)$value

#' @field modal_txt xxx
modal_txt <- "This action will reset this process. The input dataset will be the output of the last previous
                      validated process and all further datasets will be removed"

# Specific to pipeline module
tmp.return <- reactiveValues()

rv.child <- reactiveValues(
  enabled = NULL,
  reset = NULL,
  position = NULL
)



observeEvent(id, {
  
  rv.process$config <- config
  rv.process$length <- length(config$steps)
  rv.process$current.pos  <- 1
  
  rv.process$parent <- unlist(strsplit(id, split='_'))[1]
  rv.process$config <- config
  check <- CheckConfig(rv.process$config)
  if (!check$passed)
    stop(paste0("Errors in 'rv.process$config'", paste0(check$msg, collapse=' ')))
  
  rv.process$tl.tags.enabled <- setNames(rep(FALSE, length(rv.process$config$steps)), rv.process$config$steps)
  
  
  
  rv.child$position <- setNames(rep('first', length(rv.process$config$steps)), rv.process$config$steps)
  rv.process$config$mandatory <- setNames(rv.process$config$mandatory, rv.process$config$steps)
  rv.process$status <- setNames(rep(global$UNDONE, length(rv.process$config$steps)), rv.process$config$steps)
  rv.process$currentStepName <- reactive({rv.process$config$steps[rv.process$current.pos]})
  rv.child$enabled <- setNames(rep(TRUE, length(rv.process$config$steps)), rv.process$config$steps)
  rv.child$reset <- setNames(rep(FALSE, length(rv.process$config$steps)), rv.process$config$steps)
  
  for (x in rv.process$config$steps)
    source(file.path('.', paste0('mod_', paste0(id, '_', x), '.R')), local=TRUE)
  
  config$ll.UI <- lapply(rv.process$config$steps,
                         function(x){
                           do.call(paste0('mod_', id, '_', x, '_ui'),
                                   list(ns(paste0(id, '_', x))))
                         })
  
  lapply(rv.process$config$steps, function(x){
    tmp.return[[x]] <- do.call(paste0('mod_', id, '_', x, '_server'),
                               list(id = paste0(id, '_', x) ,
                                    dataIn = reactive({ rv.child$data2send[[x]] }),
                                    tag.enabled = reactive({isTRUE(rv.child$enabled[x])}),
                                    reset = reactive({isTRUE(rv.child$reset[x])}),
                                    position = reactive({rv.child$position[x]}),
                                    skipped = reactive({rv.process$status[x] == global$SKIPPED})
                               )
    )
  })
  
  # Launch_Module_Server()
  
}, priority=1000) 








output$ui <- renderUI({
  tagList(
    shinyjs::useShinyjs(),
    fluidRow(
      column(width=2, 
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
                     mod_timeline_v_ui(ns('timeline'))
                 ),
                 div(style = btn_style,
                     actionButton(ns("nextBtn"),">>",
                                  class = PrevNextBtnClass,
                                  style='padding:4px; font-size:80%')
                 )
             )),
      column(width=10,
             style=" padding-left: 20px;",
             tagList(
               div(id = ns('Screens'),
                   uiOutput(ns('SkippedInfoPanel')),
                   uiOutput(ns('EncapsulateScreens'))
                   
               ),
               wellPanel(title = 'foo',
                         tagList(
                           h3('module process'),
                           uiOutput(ns('show_Debug_Infos'))
                         )
               )
             ))
      
    )
  )
})


mod_timeline_v_server(id = 'timeline',
                      config =  config,
                      status = reactive({rv.process$status}),
                      position = reactive({rv.process$current.pos}),
                      enabled = reactive({rv.process$tl.tags.enabled})
)

# mod_timeline_v_server(id = 'timeline',
#                       config =   rv.process$config,
#                       status = reactive({rv.process$status}),
#                       position = reactive({rv.process$current.pos}),
#                       enabled = reactive({rv.process$tl.tags.enabled})
# )




CurrentStepName <- reactive({
  cat(paste0('::GetCurrentStepName() from - ', id, '\n'))
  #browser()
  rv.process$config$steps[rv.process$current.pos]
})

#' @title xxx
#'
#' @description
#' Adds one or more items to the dataset. This function is specific of the
#' type of dataset.
#'
#' @importFrom QFeatures addAssay
#'
#' @return
#' The dataset minus some items
#'
#' @export
#'
Add_Item_to_Dataset <- function(dataset, name){
  QFeatures::addAssay(dataset,
                      dataset[[length(dataset)]],
                      name=name)
}

#' @title xxx
#'
#' @description
#' Removes one or more items from the dataset. This function is specific of the
#' type of dataset.
#'
#' @return
#' The dataset minus some items
#'
#' @export
#'
Keep_Items_from_Dataset <- function(dataset, range){
  dataset[ , , range]
}



# Catch the returned values of the process                                                           
observeEvent(lapply(rv.process$config$steps, 
                    function(x){
                      tmp.return[[x]]()$trigger}), ignoreInit=T,{
                        if(verbose) cat(paste0('observeEvent(trigger) from - ', id, '\n\n'))
                        #browser()
                        ActionOn_Data_Trigger()
                      })

#' @description
#' This function calls the server part of each module composing the pipeline
#'
#' @return Nothing
#'
PrepareData2Send = function(){
  if(verbose) cat(paste0('PrepareData2Send() from - ', id, '\n\n'))
  # browser()
  # Returns NULL to all modules except the one pointed by the current position
  # Initialization of the pipeline : one send dataIn() to the
  # first module
  #browser()
  
  update <- function(name){
    data <- NULL
    if (name == CurrentStepName()){
      # One treat the dataset for the current position
      #ind.last.validated <- self$GetMaxValidated_BeforeCurrentPos()
      name.last.validated <- names(rv.process$dataIn)[length(rv.process$dataIn)]
      ind.last.validated <- which(names(rv.process$dataIn)== name.last.validated)
      
      if (is.null(ind.last.validated)){
        data <- rv.process$temp.dataIn
      } else {
        data <- Keep_Items_from_Dataset(dataset = rv.process$dataIn, 
                                        range = 1:ind.last.validated)
        #data <- self$rv$dataIn[ , , 1:ind.last.validated]
      }
    }
    return(data)
  }
  
  #browser() 
  rv.child$data2send <- setNames(
    lapply(rv.process$config$steps, function(x){NULL}),
    rv.process$config$steps)
  
  if (is.null(rv.process$dataIn)) # Init of core engine
    rv.child$data2send[[1]] <- rv.process$temp.dataIn
  else
    rv.child$data2send <- setNames(
      lapply(rv.process$config$steps, function(x){update(x)}),
      rv.process$config$steps)
  
  print("<----------------- data2 send ------------------> ")
  print(rv.child$data2send)
  print("<------------------------------------------------> ")
}











#
# Catch a new dataset sent by the caller
#
observeEvent(dataIn(), ignoreNULL = F, ignoreInit = F,{
  if (verbose) cat(paste0('::observeEvent(dataIn()) from --- ', id, '\n\n'))
  #browser()
  
  # action <- function()
  #{
  Change_Current_Pos(1)
  rv.process$temp.dataIn <- dataIn()
  ActionOn_New_DataIn() # Used by class pipeline
  
  if(is.null(dataIn())){
    print('Process : dataIn() NULL')
    
    ToggleState_Screens(FALSE, 1:length(config$steps))
    # ToggleState_ResetBtn(FALSE)
    rv.process$original.length <- 0
  } else { # A new dataset has been loaded
    print('Process : dataIn() not NULL')
    #shinyjs::toggleState('Screens', TRUE)
    #ToggleState_ResetBtn(TRUE) #Enable the reset button
    rv.process$original.length <- length(dataIn())
    
    Update_State_Screens()
    ToggleState_Screens(TRUE, 1)
    
  }
  # }
  
  #shinyjs::delay(100, action())
})


#' @description
#' xxx
#'
#' @return Nothing
#'
ActionOn_New_DataIn = function(){
  if(verbose) cat(paste0('ActionOn_New_DataIn() from - ', id, '\n\n'))
  PrepareData2Send()
}



#' @description
#' Catch the return value of a module and update the list of isDone modules
#' This list is updated with the names of datasets present in the rv$tmp
#' variable. One set to TRUE all the elements in isDone which have a corresponding
#' element in names(rv$tmp).
#' One cannot simply set to TRUE the last element of rv$tmp because it will does
#' not work in case of a reseted module (it is not in the names(rv$tmp) list
#' anymore)
#' If a value (not NULL) is received, then it corresponds to the module
#' pointed by the current position
#' This function also updates the list isDone
#' This function updates the current dataset (self$rv$dataIn)
#'
#' @return Nothing
#'
ActionOn_Data_Trigger <- function(){
  if(verbose) cat(paste0('::', 'ActionOn_Data_Trigger from - ', id, '\n\n'))
  #browser()
  processHasChanged <- newValue <- NULL
  return.trigger.values <- setNames(lapply(rv.process$config$steps, function(x){tmp.return[[x]]()$trigger}),
                                    rv.process$config$steps)
  return.values <- setNames(lapply(rv.process$config$steps, function(x){tmp.return[[x]]()$value}),
                            rv.process$config$steps)
  triggerValues <- unlist(return.trigger.values)
  
  #browser()
  if (sum(triggerValues)==0){ # Init of core engine
    
  } else if (is.null(unlist(return.values))) { # The entire pipeline has been reseted
    print('The entire pipeline has been reseted')
    PrepareData2Send()
    
  } else {
    processHasChanged <- rv.process$config$steps[which(max(triggerValues)==triggerValues)]
    ind.processHasChanged <- which(rv.process$config$steps==processHasChanged)
    newValue <- tmp.return[[processHasChanged]]()$value
    
    
    # process has been reseted
    if (is.null(newValue)){
      rv.process$status[ind.processHasChanged:length(rv.process$config$steps)] <- global$UNDONE
      
      # Reset all further steps also
      rv.child$reset[ind.processHasChanged:length(rv.process$config$steps)] <- TRUE
      rv.child$reset[1:(ind.processHasChanged-1)] <- FALSE
      
      
      
      # browser()
      # One take the last validated step (before the one corresponding to processHasChanges
      # but it is straightforward because we just updates self$rv$status
      ind.last.validated <- NULL
      validated.steps <- which(rv.process$status == global$VALIDATED)
      if (length(validated.steps) !=0)
        ind.last.validated <- max(validated.steps)
      
      #There is no validated step (the first step has been reseted)
      if(is.null(ind.last.validated) || ind.last.validated == 1)
        rv.process$dataIn <- rv.process$temp.dataIn
      else{
        name.last.validated <- rv.process$config$steps[ind.last.validated]
        dataIn.ind.last.validated <- which(names(rv.process$dataIn) == name.last.validated)
        #self$rv$dataIn <- self$rv$dataIn[ , , 1:dataIn.ind.last.validated]
        rv.process$dataIn <- Keep_Items_from_Dataset(dataset = rv.process$dataIn, 
                                                     range = 1:dataIn.ind.last.validated)
        
      }
      
      # In this case, one force the update of the input dataset
      PrepareData2Send()
    } else {
      # browser()
      # process has been validated
      rv.process$status[processHasChanged] <- global$VALIDATED
      if (ind.processHasChanged < length(rv.process$config$steps))
        rv.process$status[(ind.processHasChanged+1):length(rv.process$config$steps)] <- global$UNDONE
      
      Discover_Skipped_Steps()
      rv.process$dataIn <- newValue
    }
    Send_Result_to_Caller()
  }
  
}


#' @description 
#' xxx
#' 
Initialize_Status_Process = function(){
  if(verbose) cat(paste0('::', 'Initialize_Status_Process() from - ', id, '\n\n'))
  rv.process$status <- setNames(rep(global$UNDONE, length(config$steps)), config$steps)
}


#' @description
#' et to skipped all steps of the current object
#' 
#' @return Nothing.
#' 
ResetChildren = function(){
  if(verbose) cat(paste0('::', 'Set_All_Reset() from - ', id, '\n\n'))
  #browser()
  lapply(rv.process$config$steps, function(x){
    rv.child$reset[x] <- TRUE
  })
}


#' @description
#' This function calls the server part of each module composing the pipeline
#'
#' @return Nothing
#'
GetMaxValidated_BeforeCurrentPos = function(){
  if(verbose) cat(paste0('GetMaxValidated_BeforeCurrentPos() from - ', id, '\n\n'))
  ind.max <- NULL
  indices.validated <- which(rv.process$status == global$VALIDATED)
  if (length(indices.validated) > 0){
    ind <- which(indices.validated < rv.process$current.pos)
    if(length(ind) > 0)
      ind.max <- max(ind)
  }
  ind.max
}

#' @description
#' This function calls the server part of each module composing the pipeline
#'
#' @param pos xxx
#' 
#' @return Nothing
#'
GetMaxValidated_BeforePos = function(pos){
  if(verbose) cat(paste0('GetMaxValidated_BeforeCurrentPos() from - ', id, '\n\n'))
  ind.max <- NULL
  indices.validated <- which(rv.process$status == global$VALIDATED)
  if (length(indices.validated) > 0){
    ind <- which(indices.validated < pos)
    if(length(ind) > 0)
      ind.max <- max(ind)
  }
  ind.max
}


#' @description
#' Default actions on reset pipeline or process.
#' 
BasicReset = function(){
  if(verbose) cat(paste0('BasicReset() from - ', id, '\n\n'))
  ResetChildren()
  rv.process$dataIn <- NULL
  rv.process$current.pos <- 1
  Initialize_Status_Process()
  Send_Result_to_Caller()
}













#' @description
#' xxx
#'
#' @return Nothing
#'
ActionOn_NewPosition = function(){
  if(verbose) cat(paste0('::ActionOn_NewPosition() from - ', id, '\n\n'))
  
  print("--- action on New position ---")
  # Send dataset to child process only if the current position is enabled
  if(rv.child$enabled[rv.process$current.pos])
    PrepareData2Send()
  #browser()
  # If the current step is validated, set the child current position to the last step
  if (rv.process$status[rv.process$current.pos] == global$VALIDATED)
    rv.child$position[rv.process$current.pos] <- paste0('last_', Timestamp())
}




#-------------------------------------------------------
observeEvent(rv.process$current.pos, ignoreInit = F,{
  if (verbose) cat(paste0('::observe(rv$current.pos) from - ', id, '\n\n'))
  
  shinyjs::toggleState(id = "prevBtn", condition = rv.process$current.pos > 1)
  shinyjs::toggleState(id = "nextBtn", condition = rv.process$current.pos < length(config$steps))
  shinyjs::hide(selector = paste0(".page_", id))
  shinyjs::show(config$steps[rv.process$current.pos])
  
  ActionOn_NewPosition()
  
})



