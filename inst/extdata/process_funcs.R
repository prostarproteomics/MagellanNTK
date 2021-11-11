observeEvent(rv$current.pos, ignoreInit = TRUE, {
      if (verbose) cat(yellow(paste0(id, '::observeEvent(rv$current.pos)\n\n')))
      
      ToggleState_NavBtns()
      # Hide all screens 
      shinyjs::hide(selector = paste0('.page_', id))
      
      #Show the current step which is identified by its name. This point is very important
      # and need that the renderUI functions of the process to be strickly well named
      shinyjs::show(rv$config$steps[rv$current.pos])
      
      })


output$process_ui <- renderUI({
  
  tagList(
        fluidRow(style = 'display: flex; align-items: center;
                          justify-content: center;',
             column(width=1, shinyjs::disabled(
               actionButton(ns('prevBtn'), '<<',
                            class = PrevNextBtnClass,
                            style='font-size:80%')
             )),
             column(width=1, actionButton(ns('rstBtn'), 'Reset',
                                          class = redBtnClass,
                                          style='font-size:80%')),
             column(width=9, mod_timeline_h_ui(ns('timeline'))),
             column(width=1, shinyjs::disabled(
               actionButton(ns('nextBtn'),'>>',
                            class = PrevNextBtnClass,
                            style='font-size:80%'))
             )
    ),
    div(id = ns('Screens'),
        uiOutput(ns('SkippedInfoPanel')),
        uiOutput(ns('EncapsulateScreens_ui'))
    )
      )
    
})


observeEvent(id, {
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
      Discover_Skipped_Steps()
      
      # If it is the first step (description step), then xxxx
      if (rv$current.pos==1)
        rv$dataIn <- rv$temp.dataIn
       else #if it is the last step of the process
      if (rv$current.pos == rv$length){
        #Update the work variable of the nav_process with the dataset returned by the process
        rv$dataIn <- rv$proc$dataOut()$value
        
        #Update the 'dataOut' reactive value to return this dataset to the caller
        # this nav_process is only a bridge between the process and the caller
        Send_Result_to_Caller()
        
        # dataOut$trigger <- rv$proc$dataOut()$trigger
        # dataOut$value <- rv$proc$dataOut()$value
      }

    })



observeEvent(req(!is.null(rv$position)), ignoreInit = TRUE, {
  pos <- strsplit(rv$position, '_')[[1]][1]
  if (pos == 'last')
    rv$current.pos <- rv$length
  else if (is.numeric(pos))
    rv$current.pos <- rv$position
})

