GetCode_InitProcessServer <- function(){
  
  InitProcessServer = function(){
    
    rv$current.pos  <- 1
    
    #Call the module server of the process
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
    rv$steps.status <- setNames(rep(Magellan::global$UNDONE, rv$length), rv$config$steps)
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
    
  }
  
  code <- paste0("InitProcessServer <- ", 
                        paste0(deparse(InitProcessServer), collapse = "\n"),
                        "\n\n")
  code
}