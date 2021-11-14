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
#'   dataIn = reactive({feat1})
#'   )
#' }
#' shinyApp(ui, server)
#' }
#' 
#' @importFrom stats setNames
#' 
mod_nav_server <- function(id,
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
    
    
    
    # Reactive values that will be used to output the current dataset when 
    # the last step is validated
    dataOut <- reactiveValues(
      trigger = NULL,
      value = NULL
    )
    
    rv <- reactiveValues(
      # @field proc contains the return value of the process module that has been called 
      proc = NULL,
      
      mode = NULL,
      
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
      config = NULL,
      
      # A vector of boolean where each element indicates if the corresponding
      # child if enable or disable
      child.enabled = NULL,
      
      # xxxx
      child.reset = NULL,
      
      # A vector of integers where each element denotes the current position 
      # of the corresponding element.
      child.position = NULL,
      
      # xxxx
      child.data2send = NULL
    )
    
    
    # Specific to pipeline module
    # Used to store the return values (lists) of child processes
    tmp.return <- reactiveValues()
    

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
    
    
    
    
    # @title
    # Disable an entire process
    # @description 
    # The parameter is.enabled() is updated by the caller and tells the process
    # if it is enabled or disabled (remote action from the caller)
    observeEvent(is.enabled(), ignoreNULL = TRUE, ignoreInit = TRUE, {
      cat(yellow('::is.enabled()\n\n'))
      
      if (isTRUE(is.enabled())){
        rv$steps.enabled <- Update_State_Screens(is.skipped = is.skipped(),
                                                 is.enabled = is.enabled(),
                                                 rv = rv)
      } else {
        rv$steps.enabled <- setNames(rep(is.enabled(), rv$length), 
                                     nm = rv$config$steps)
      }
    })
    
    
    # Catch new status event
    observeEvent(rv$steps.status, ignoreInit = TRUE, {
      # https://github.com/daattali/shinyjs/issues/166
      # https://github.com/daattali/shinyjs/issues/25
      
      rv$steps.status <- Discover_Skipped_Steps(rv$steps.status)
      
      rv$steps.enabled <- Update_State_Screens(is.skipped = is.skipped(),
                                               is.enabled = is.enabled(),
                                               rv = rv)
      
      if (rv$steps.status[rv$length] == global$VALIDATED){
        # Set current position to the last one
        rv$current.pos <- rv$length
        
        # Send result
        res <- Send_Result_to_Caller(rv$dataIn)
        dataOut$trigger <- res$trigger
        dataOut$value <- res$value
      }
    })
    
    
    # @description 
    # The parameter is.skipped() is set by the caller and tells the process
    # if it is skipped or not (remote action from the caller)
    
    observeEvent(is.skipped(), ignoreNULL = FALSE, ignoreInit = TRUE,{
      
      if (isTRUE(is.skipped()))
        rv$steps.status <- All_Skipped_tag(rv$steps.status, global$SKIPPED)
      else{
        rv$steps.status <- All_Skipped_tag(rv$steps.status, global$UNDONE)
        rv$steps.enabled <- Update_State_Screens(is.skipped = is.skipped(),
                                                 is.enabled = is.enabled(),
                                                 rv = rv)
        }
    })
    
    
    
    # Catch a click of a the button 'Ok' of a reset modal. This can be in the local module
    # or in the module parent UI (remoteReset)
    observeEvent(c(remoteReset(), req(input$modal_ok)), ignoreInit = FALSE, ignoreNULL = TRUE, {
      
      res <- LocalReset(mode = rv$mode, rv = rv)
      
      rv$dataIn = res$dataIn
      dataOut = res$dataOut
      rv$current.pos = res$current.pos
      rv$steps.status = res$steps.status
      rv$resetChildren = res$resetChildren
      
      removeModal()
    })
    
    
    # Catch a click on the 'Reset' button. Then, open the modal for info on resetting.
    observeEvent(input$rstBtn, ignoreInit = TRUE, {
      showModal(
        dataModal(ns, rv$mode)
        )
    })
    
    
    # Show the ui for a skipped module
    output$SkippedInfoPanel <- renderUI({
      Build_SkippedInfoPanel(steps.status = rv$steps.status,
                             current.pos = rv$current.pos,
                             config = rv$config
                             )
      })
     
     # This function uses the UI definition to:
     # * initialize the UI (only the first screen is shown),
     # * encapsulate the UI in a div (used to hide all screens at a time before
     # showing the one corresponding to the current position)
     output$EncapsulateScreens_ui <- renderUI({
       Build_EncapsulateScreens_ui(ns = ns,
                                   id = id,
                                   config = rv$config
                                   )
       })
     
     
     # Launch the renderUI function for the user interface of the module
     # Apparently, the renderUI() cannot be stored in the function 'Build..'
     output$nav_mod_ui <- renderUI({
       do.call(paste0('Build_', rv$mode, '_ui'), list(ns))
     })
     
     
     # Catch any event on the 'id' parameter. As this parameter is static and is
     # attached to the server, this function can be view as the initialization
     # of the server module
     # This code is generic to both process and pipeline modules
     observeEvent(id, {
       # The function of the module server (and ui) are supposed to be already
       # loaded. Check if it is the case. If not, show a message and abort
      if (!Found_Mod_Funcs(id)){
          warning(paste0('Cannot find functions for the module ', id))
          return(NULL)
        }
 
       rv$current.pos <- 1
       
       
       # Call the server module of the pipeline which name is the parameter 'id'
       # This will give access to its config
       rv$proc <- do.call(paste0('mod_', id, '_server'),
                          list(id = id,
                               dataIn = reactive({rv$temp.dataIn}),
                               steps.enabled = reactive({rv$steps.enabled}),
                               remoteReset = reactive({input$rstBtn + remoteReset()}),
                               steps.status = reactive({rv$steps.status}),
                               current.pos = reactive({rv$current.pos})
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
       # Check if the config variable is correct
       # check <- CheckConfig(rv$config)
       # if (!check$passed)
       #   stop(paste0("Errors in 'rv$config'", paste0(check$msg, collapse=' ')))
       # 
       # 
       
       rv$length <- length(rv$config$steps)
       #browser()
       rv$mode <- rv$config$mode
       rv$parent.name <- rv$config$parent
       rv$module.name <- rv$config$name
       
       
       
       rv$config$mandatory <- setNames(rv$config$mandatory, rv$config$steps)
       rv$steps.status <- setNames(rep(global$UNDONE, rv$length), rv$config$steps)
       
       rv$steps.enabled <- setNames(rep(FALSE, rv$length), rv$config$steps)
       rv$steps.skipped <- setNames(rep(FALSE, rv$length), rv$config$steps)
       rv$resetChildren <- setNames(rep(0, rv$length), rv$config$steps)
       
       rv$child.data2send <- setNames(lapply(as.list(rv$config$steps), 
                                             function(x) NULL), 
                                      nm = rv$config$steps)
       
       rv$currentStepName <- reactive({rv$config$steps[rv$current.pos]})
      }, priority=1000) 
     
     
     
     
     #-----------------------
     # Catch a new value on the parameter 'dataIn()' variable, sent by the caller. This value 
     # may be NULL or contain a dataset.
     # The first action is to store the dataset in the temporary variable 
     # temp.dataIn. Then, two behaviours:
     # * if the variable is NULL. xxxx
     # * if the variable contains a dataset. xxx
     observeEvent(dataIn(), ignoreNULL = FALSE, ignoreInit = FALSE,{
       cat(crayon::yellow(paste0(id, "::observe(dataIn())\n\n")))
       isolate({
         # A new value on dataIn() means a new dataset sent to the process
         #browser()
         rv$current.pos <- 1
         
         # Get the new dataset in a temporary variable
         rv$temp.dataIn <- dataIn()
         #ActionOn_New_DataIn() # Used by class pipeline
         
         # The mode pipeline is a node and has to send
         # datasets to its children
         if (rv$mode == 'pipeline'){
           if (is.null(rv$dataIn)){
             res <-  PrepareData2Send(rv = rv, pos = rv$current.pos)
             rv$child.data2send <- res$data2send
             rv$steps.enabled <- res$steps.enabled
           }
         }
         
         if(is.null(dataIn())){
           # The process has been reseted or is not concerned
           cat(blue('In observe(dataIn()) : dataIn() is NULL\n\n'))
           # Disable all screens of the process
           rv$steps.enabled <- ToggleState_Screens(cond = FALSE, 
                                                   range = seq_len(rv$length),
                                                   is.enabled = is.enabled,
                                                   rv = rv)
         } else { 
           # A new dataset has been loaded
           cat(blue('In observe(dataIn()) : dataIn() is not NULL\n\n'))
           # Update the different screens in the process
           rv$steps.enabled <- Update_State_Screens(is.skipped = is.skipped(),
                                                    is.enabled = is.enabled(),
                                                    rv = rv)
         }
         
         # Update the initial length of the dataset with the length
         # of the one that has been received
         rv$original.length <- length(dataIn())
         # Enable the first screen
         rv$steps.enabled <- ToggleState_Screens(cond = TRUE, 
                                                 range = 1,
                                                 is.enabled = is.enabled(),
                                                 rv = rv)
         
       })
     })
     
     
     # Catch the time when the mode is defined
     #{ Then, launch observers and fucntions specific to 
     # processes nor pipelines
     observeEvent(req(rv$mode), {
       
        if (!(rv$mode %in% c('process', 'pipeline'))){
           warning("'mode' must be either 'process' or 'pipeline'.")
           return(NULL)
         }
       
       switch (rv$mode,
            default = {},
            pipeline = {
              # Before continuing the initialization, check if all modules functions
              # are found in the environment
              for (i in rv$config$steps)
                if (!Found_Mod_Funcs(paste0(rv$module.name, '_',i))){
                  warning(paste0('Cannot find functions ui() and server() for the module ', i))
                  return(NULL)
                }
              
              rv$steps.skipped <- setNames(rep(FALSE, rv$length), rv$config$steps)
              rv$resetChildren <- setNames(rep(0, rv$length), rv$config$steps)
              
              # Launch the ui for each step of the pipeline
              # This function could be stored in the source file of the pipeline
              # but the strategy is to insert minimum extra code in the files for
              # pipelines and processes. This is useful when other devs will
              # develop other pipelines and processes. Thus, it will be easier.
              rv$config$ll.UI <- setNames(lapply(rv$config$steps,
                                                 function(x){
                                                   mod_nav_ui(ns(paste0(id, '_', x)))
                                                 }),
                                          paste0(rv$config$steps)
              )
              
              
              lapply(rv$config$steps, function(x){
                tmp.return[[x]] <- mod_nav_server(id = paste0(id, '_', x) ,
                                                  dataIn = reactive({rv$child.data2send[[x]]}),
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
              
              
              
              
              observeEvent(rv$current.pos, ignoreInit = TRUE, {
                ToggleState_NavBtns(rv)
                shinyjs::hide(selector = paste0('.page_', id))
                shinyjs::show(rv$config$steps[rv$current.pos])
                
                #Specific to pipeline code
                res <-  PrepareData2Send(rv = rv, pos = NULL)
                rv$child.data2send <- res$data2send
                rv$steps.enabled <- res$steps.enabled
                
                if (rv$steps.status[rv$current.pos] == global$VALIDATED)
                  rv$child.position[rv$current.pos] <- paste0('last_', Timestamp())

              })
              
              
              
             
              
              
              ActionOn_Data_Trigger = function(){
                processHasChanged <- newValue <- NULL
                
                # Get the values returned by all children (steps) of the module
                values.children <- GetValuesFromChildren(tmp.return = tmp.return, config = rv$config)
                triggerValues <- values.children$triggers
                return.values <- values.children$values
                
                cat(crayon::blue('--------------- Data received from children --------------------\n'))
                print(return.values)
                cat(crayon::blue('-------------------------------------------------------\n'))
                
                
                if (is.null(return.values)) { # The entire pipeline has been reseted
                  rv$dataIn <- NULL
                  rv$steps.status[seq_len(rv$length)] <- global$UNDONE
                } else {
                  # A process has changed
                  # Either it has returned a value (newValue contains a dataset)
                  # or it has been reseted (newValue is NULL)
                  ind.process.has.changed <- which(max(triggerValues, na.rm = TRUE)==triggerValues)
                  processHasChanged <- rv$config$steps[ind.process.has.changed]
                  
                  # Indice of the dataset in the object
                  # If the original length is not 1, then this indice is different
                  # than the above one
                  #ind.processHasChanged <- which(rv$config$steps==processHasChanged)
                  
                  # Get the new value
                  newValue <- tmp.return[[processHasChanged]]$dataOut()$value
                  
                  ret <- ActionOn_Child_Changed(temp.dataIn = rv$temp.dataIn,
                                         dataIn = rv$dataIn,
                                         steps.status = rv$steps.status,
                                         steps = rv$config$steps,
                                         steps.enabled = rv$steps.enabled,
                                         steps.skipped = rv$steps.skipped,
                                         processHasChanged = processHasChanged,
                                         newValue = newValue)
                  
                  
                  rv$dataIn = ret$dataIn
                  rv$steps.status = ret$steps.status
                  rv$steps.enabled = ret$steps.enabled
                  rv$steps.skipped = ret$steps.skipped

                }

                # Send result
                res <- Send_Result_to_Caller(rv$dataIn)
                dataOut$trigger <- res$trigger
                dataOut$value <- res$value
              }
              
            
              # Catch the returned values of the processes attached to pipeline                                                           
              observeEvent(lapply(rv$config$steps, 
                                  function(x){
                                    tmp.return[[x]]$dataOut()$trigger}), ignoreInit = TRUE, {
                                      ActionOn_Data_Trigger()
                                    })
              },
            process = {
              
              observeEvent(rv$current.pos, ignoreInit = TRUE, {
               
                ToggleState_NavBtns(rv = rv)
                # Hide all screens 
                shinyjs::hide(selector = paste0('.page_', id))
                
                #Show the current step which is identified by its name. This point is very important
                # and need that the renderUI functions of the process to be strickly well named
                shinyjs::show(rv$config$steps[rv$current.pos])
                
              })
              
              
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

              
              observeEvent(rv$proc$dataOut()$trigger, ignoreNULL = TRUE, ignoreInit = TRUE, {
                # If a value is returned, this is because the current step has been validated
                rv$steps.status[rv$current.pos] <- global$VALIDATED
                
                # Look for new skipped steps
                rv$steps.status <- Discover_Skipped_Steps(rv$steps.status)
                
                
                # If it is the first step (description step), then 
                # load the dataset in work variable 'dataIn'
                if (rv$current.pos==1)
                  rv$dataIn <- rv$temp.dataIn
                else #if it is the last step of the process
                  if (rv$current.pos == rv$length){
                    # Update the work variable of the nav_process with the dataset returned by the process
                    # Thus, the variable rv$temp.dataIn keeps trace of the original dataset sent to
                    # this  workflow and will be used in case of reset
                    rv$dataIn <- rv$proc$dataOut()$value
                    
                    # Update the 'dataOut' reactive value to return this dataset to the caller
                    # this nav_process is only a bridge between the process and the caller
                    # For a pipeline, the output is updated each time a process has 
                    # been validated
                    res <- Send_Result_to_Caller(rv$dataIn)
                    dataOut$trigger <- res$trigger
                    dataOut$value <- res$value
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