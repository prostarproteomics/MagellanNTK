#' @title xxx
#' 
#' @description 
#' This module contains the configuration informations for the corresponding pipeline.
#' It is called by the nav_pipeline module of the package Magellan
#' 
#' @noRd
#' 
#' @export
#' 
mod_nav_process_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    
    fluidRow(style="display: flex;
    align-items: center;
             justify-content: center;",
             column(width=1, shinyjs::disabled(
               actionButton(ns("prevBtn"), "<<",
                            class = PrevNextBtnClass,
                            style='font-size:80%')
             )),
             column(width=1, actionButton(ns("rstBtn"), "Reset",
                                          class = redBtnClass,
                                          style='font-size:80%')),
             column(width=9, mod_timeline_h_ui(ns('timeline'))),
             column(width=1, shinyjs::disabled(
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
}


#' @title xxx
#' 
#' @description xxx
#' 
#' @param id xxx
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
#' @noRd
#' 
#' @examples
#' \donttest{
#' library(shiny)
#' library(shinyBS)
#' ui <- fluidPage(
#'   mod_nav_process_ui('Protein_Description')
#' )
#' server <- function(input, output){
#'   mod_nav_process_server(id = 'Protein_Description',
#'                          dataIn = reactive({QFeatures::feat1})
#'   )
#' }
#' shinyApp(ui, server)
#' }
#' 
mod_nav_process_server <- function(id,
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
    
    nav.mode <- "process"
    
    source(system.file("extdata", 'commonFuncs.R', package="Magellan"), local=TRUE)$value
    
    
    
    
    # This text is showed in the modal when the user click on the 'Reset' button.
    modal_txt <- "This action will reset this process. The input dataset will be the output of the last previous
                      validated process and all further datasets will be removed"
    
    
    
    
    ##### Initialization of the module. ####
    # The first action is to instantiate the module process which contains the 
    # configuration and UI of the process. Then, it instantiates the local 
    # (reactive) variables of the nav_process module. Finally, launches the 
    # timeline module server.
    observeEvent(id, {
      #browser()
      # Launch of the module process server
      cat(yellow(paste0("Launching ", paste0('mod_', id, '_server\n\n'))))
      #browser()
      
      
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
      rv.process$steps.status <- setNames(rep(global$UNDONE, rv.process$length), rv.process$config$steps)
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
      
      current_step_skipped <- rv.process$steps.status[rv.process$current.pos] == global$SKIPPED
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
      rv.process$steps.status[rv.process$current.pos] <- global$VALIDATED
      
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
    
    
    # This function changes the state (enabled, disabled) of the steps in the process
    # The parameter 'cond' is the new state
    # The parameter 'range' corresponds to the range of steps to update
    # ToggleState_Screens = function(cond, range){
    #   if(verbose) cat(paste0('::ToggleState_Steps() from - ', id, "\n\n"))
    #   #browser()
    #   if (isTRUE(is.enabled()))
    #     lapply(range, function(x){
    #       cond <- cond && !(rv.process$steps.status[x] == global$SKIPPED)
    #       
    #       #Send to TL the enabled/disabled tags
    #       rv.process$steps.enabled[x] <- cond
    #     })
    #   
    #   
    #   # Update the state enabled/disabled of the navigation buttons
    #   ToggleState_NavBtns()
    # }
    
    
    
    
    
    
    
    
    # Catches a new value of the cursor position
    observeEvent(req(!is.null(rv.process$position)), ignoreInit = TRUE, {
      pos <- strsplit(rv.process$position, '_')[[1]][1]
      if (pos == 'last')
        rv.process$current.pos <- rv.process$length
      else if (is.numeric(pos))
        rv.process$current.pos <- rv.process$position
    })
    
    # # Default actions on reset pipeline or process.
    # # 
    # LocalReset = function(){
    #   if(verbose) cat(paste0('LocalReset() from - ', id, "\n\n"))
    #   #ResetScreens()
    #   rv.process$dataIn <- NULL
    #   rv.process$current.pos <- 1
    #  rv.process$steps.status <- setNames(rep(global$UNDONE, rv.process$length), rv.process$config$steps)
    #   Send_Result_to_Caller()
    # }
    
    
    
    
    # Catches a new position to show/hide the correct screen. This function
    # also manages the enabling/disabling of the `Prev` and `Next` buttons
    # w.r.t predefined rules (each of these buttons are disabled if there is
    # no more steps in their direction)
    observeEvent(rv.process$current.pos, ignoreInit = TRUE, {
      if (verbose) cat(yellow(paste0(id, '::observeEvent(rv.process$current.pos)\n\n')))
      
      ToggleState_NavBtns()
      # Hide all screens 
      shinyjs::hide(selector = paste0(".page_", id))
      
      #Show the current step which is identified by its name. This point is very important
      # and need that the renderUI functions of the process to be strickly well named
      shinyjs::show(rv.process$config$steps[rv.process$current.pos])
      
      })

    
    # Default actions on reset pipeline or process.
    # 
    LocalReset = function(){
      if(verbose) cat(yellow(paste0(id, '::LocalReset()\n\n')))
      #browser()
      rv.process$dataIn <- NULL
      #rv.process$temp.dataIn <- NULL
      
      # The cursor is set to the first step
      rv.process$current.pos <- 1
      
      # The status of the steps are reinitialized to the default configuration of the process
      rv.process$steps.status <- setNames(rep(global$UNDONE, rv.process$length), 
                                    rv.process$config$steps)
      
      # Return the NULL value as dataset
      Send_Result_to_Caller()
      #dataOut <- reactive({Send_Result_to_Caller(rv.process$dataIn)})
    }
    
    
    
    # Show/hide an information panel if the process is entirely skipped
    # This functions can be used for both nav_process and nav_pipeline modules
    output$SkippedInfoPanel <- renderUI({
      #if (verbose) cat(paste0(class(self)[1], '::output$SkippedInfoPanel from - ', self$id, "\n\n"))
      
      current_step_skipped <- rv.process$steps.status[rv.process$current.pos] == global$SKIPPED
      req(current_step_skipped)
      process_entirely_skipped <- isTRUE(sum(rv.process$steps.status) == global$SKIPPED * rv.process$length)
      
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
         steps.enabled = reactive({rv.process$steps.enabled})
         #steps.status = reactive({rv.process$steps.status})
    )
    
    
  }
  )
}