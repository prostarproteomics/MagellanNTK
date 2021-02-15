
source(file.path('.', 'commonFuncs.R'), local=TRUE)$value
source(file.path('.', 'mod_timeline_h.R'), local=TRUE)$value





AddItemToDataset <- function(dataset, name){
  addAssay(dataset, 
           dataset[[length(dataset)]], 
           name=name)
}



#' @field modal_txt xxx
modal_txt <- "This action will reset this process. The input dataset will be the output of the last previous
                      validated process and all further datasets will be removed"



observeEvent(id, {
  rv.widgets <- reactiveValues()
  
  rv.process$config <- config
  rv.process$length <- length(rv.process$config$steps)
  rv.process$current.pos  <- 1
  
  rv.process$config$ll.UI <- lapply(rv.process$config$steps,
                                    function(x){
                                      do.call('uiOutput', list(ns(x)))
                                    })
  
  rv.process$parent <- unlist(strsplit(id, split='_'))[1]
  
  check <- CheckConfig(rv.process$config)
  if (!check$passed)
    stop(paste0("Errors in 'rv.process$config'", paste0(check$msg, collapse=' ')))
  rv.process$config$mandatory <- setNames(rv.process$config$mandatory, rv.process$config$steps)
  rv.process$status = setNames(rep(global$UNDONE, rv.process$length), rv.process$config$steps)
  rv.process$currentStepName <- reactive({rv.process$config$steps[rv.process$current.pos]})
  rv.process$tl.tags.enabled <- setNames(rep(FALSE, rv.process$length), rv.process$config$steps)
  
}, priority=1000) 








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
  #ActionOn_New_DataIn() # Used by class pipeline
  
  if(is.null(dataIn())){
    print('Process : dataIn() NULL')
    
    ToggleState_Screens(FALSE, 1:rv.process$length)
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


#' #' @description 
#' #' xxx
#' #' 
#' Initialize_Status_Process = function(){
#'   if(verbose) cat(paste0('::', 'Initialize_Status_Process() from - ', id, '\n\n'))
#'   rv.process$status <- setNames(rep(global$UNDONE, length(config$steps)), config$steps)
#' }





mod_timeline_h_server(id = 'timeline',
                      config =  config,
                      status = reactive({rv.process$status}),
                      position = reactive({rv.process$current.pos}),
                      enabled = reactive({rv.process$tl.tags.enabled})
)


observeEvent(req(!is.null(position())), ignoreInit = T, {
  pos <- strsplit(position(), '_')[[1]][1]
  if (pos == 'last')
    rv.process$current.pos <- rv.process$length
  else if (is.numeric(pos))
    rv.process$current.pos <- position()
})

#' @description
#' Default actions on reset pipeline or process.
#' 
BasicReset = function(){
  if(verbose) cat(paste0('BasicReset() from - ', id, '\n\n'))
  ResetScreens()
  rv.process$dataIn <- NULL
  rv.process$current.pos <- 1
  rv.process$status <- setNames(rep(global$UNDONE, rv.process$length), rv.process$config$steps)
  Send_Result_to_Caller()
}





##
## Common functions
##


#' @description
#' Set widgets of all screens to their default values.
#' 
ResetScreens = function(){
  if(verbose) cat(paste0('::ResetScreens() from - ', id, '\n\n'))
  lapply(names(rv.widgets), function(x){
    rv.widgets[[x]] <- widgets.default.values[[x]]
  })
}



#-------------------------------------------------------
observeEvent(rv.process$current.pos, ignoreInit = F,{
  if (verbose) cat(paste0('::observe(rv$current.pos) from - ', id, '\n\n'))
  
  shinyjs::toggleState(id = "prevBtn", condition = rv.process$current.pos > 1)
  shinyjs::toggleState(id = "nextBtn", condition = rv.process$current.pos < rv.process$length)
  shinyjs::hide(selector = paste0(".page_", id))
  shinyjs::show(rv.process$config$steps[rv.process$current.pos])
  
  #ActionOn_NewPosition()
  
})



observeEvent(req(reset()), ignoreInit=F, ignoreNULL=T, {
  if (verbose) cat(paste0('::observeEvent(req(c(input$modal_ok))) from - ', id, '\n\n'))
  BasicReset()
})


