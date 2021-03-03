btn_style <- "display:inline-block; vertical-align: middle; padding: 7px"

source(file.path('.', 'mod_timeline_h.R'), local=TRUE)$value

btn_style <- "display:inline-block; vertical-align: middle; padding: 7px"
verbose <- F
redBtnClass <- "btn-danger"
PrevNextBtnClass <- "btn-info"
btn_success_color <- "btn-success"
optionsBtnClass <- "info"






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
             column(width=1, actionButton(ns("nextBtn"),">>",
                                          class = PrevNextBtnClass,
                                          style='font-size:80%'))
    ),
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
    
  )
}


#' @title xxx
#' 
#' @description 
#' This module contains the configuration informations for the corresponding pipeline.
#' It is called by the nav_pipeline module of the package Magellan
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
#' @param reset It is a remote command to reset the module. A boolen that 
#' indicates is the pipeline has been reseted by a program of higher level
#' Basically, it is the program which has called this module
#' 
#' @param status xxx
#' 
#' @return A list of four items:
#' * dataOut xxx
#' * steps.enabled xxxxx
#' * status xxxx
#' * reset xxxx
#' 
#' @author Samuel Wieczorek
#' 
#' @export
#' 
mod_nav_process_server <- function(id,
                                   dataIn = reactive({NULL}),
                                   dataOut = NULL,
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
    
    source(file.path('.', 'commonFuncs.R'), local=TRUE)$value
    
    AddItemToDataset <- function(dataset, name){
      addAssay(dataset, 
               dataset[[length(dataset)]], 
               name=name)
    }
    
    # child.dataOut <- reactiveValues(
    #   trigger = NULL,
    #   value = NULL,
    #   name = NULL
    # )
    
    rv.process <- reactiveValues(
      #' @field proc contains the return value of the called process 
      proc = NULL,
      #' @field status A booelan vector which contains the status (validated,
      #' skipped or undone) of the steps
      status = NULL,
      #' @field dataIn A dataset
      dataIn = NULL,
      #' @field temp.dataIn This variable is used to serves as a tampon between 
      #' the input of the module and the functions. 
      temp.dataIn = NULL,
      #' @field steps.enabled xxx
      steps.enabled = NULL,
      #' @field current.pos Stores the current cursor position in the timeline
      current.pos = 1
    )
    
    verbose <- T
    #' @field modal_txt xxx
    modal_txt <- "This action will reset this process. The input dataset will be the output of the last previous
                      validated process and all further datasets will be removed"
    
    #' #' @description
    #' #' Catch a change in the output variable `status` of the module that has been
    #' #' called. It is used to know when a step has been validated in the corresponding module
    #' observeEvent(rv.process$proc$status(), {
    #'   #browser()
    #'   # If step 1 has been validated, then initialize dataIn
    #'   if (rv.process$status[1]==0 && rv.process$proc$status()[1]==1)
    #'     rv.process$dataIn <- rv.process$temp.dataIn
    #'   
    #'   rv.process$status <- rv.process$proc$status() 
    #'   })
    
    
    
    #' @description
    #' Catch the dataset sent by the process module and instantiate the rv$dataOut variable
    #' which is the return value of the module.
    #' This function is only used to communicate between the process module and and the caller
    observeEvent(dataOut$trigger, ignoreNULL = TRUE, ignoreInit = TRUE, {
      print('end')
     browser()
    if (is.null(dataOut$value))
      rv.process$status[1:rv.process$length] <- global$UNDONE
    else {
      rv.process$status[rv.process$current.pos] <- global$VALIDATED
      if (rv.process$current.pos == rv.process$length){
        rv.process$dataIn <- dataOut$value
      }
    }
     
      Discover_Skipped_Steps()
      
      
      dataOut$trigger <- dataOut$trigger
      dataOut$value <- dataOut$value
      dataOut$name <- dataOut$id
      if (rv.process$current.pos==1)
        rv.process$dataIn <- rv.process$temp.dataIn
      
      
      
     # browser()
    })
    
    
    #' @description 
    #' Initialization of the module.
    #' The first action is to instantiate the module process which contains the 
    #' configuration and UI of the process. Then, it instantiates the local 
    #' (reactive) variables of the nav_process module. Finally, launches the 
    #' timeline module server.
    observeEvent(id, {
      #browser()
      # Launch of the module process server
      rv.process$proc <- do.call(paste0('mod_', id, '_server'),
                                 list(id = id,
                                      dataIn = reactive({rv.process$temp.dataIn}),
                                      dataOut = dataOut,
                                      steps.enabled = reactive({rv.process$steps.enabled}),
                                      remoteReset = reactive({input$rstBtn + remoteReset()}))
      )
      
      # Instantiate the local variables
      rv.process$config <- rv.process$proc$config()
      check <- CheckConfig(rv.process$config)
      if (!check$passed)
        stop(paste0("Errors in 'rv.process$config'", paste0(check$msg, collapse=' ')))
      
      
      
      rv.process$length <- length(rv.process$config$steps)
      rv.process$current.pos  <- 1
      
      rv.process$parent <- unlist(strsplit(id, split='_'))[1]
      
      
      rv.process$config$mandatory <- setNames(rv.process$config$mandatory, rv.process$config$steps)
      rv.process$status = setNames(rep(global$UNDONE, rv.process$length), rv.process$config$steps)
      rv.process$currentStepName <- reactive({rv.process$config$steps[rv.process$current.pos]})
      rv.process$steps.enabled <- setNames(rep(FALSE, rv.process$length), rv.process$config$steps)
      
      
      # Launch the timeline server
      mod_timeline_h_server(id = 'timeline',
                            config =  rv.process$config,
                            status = reactive({rv.process$status}),
                            position = reactive({rv.process$current.pos}),
                            enabled = reactive({rv.process$steps.enabled})
      )
    }, priority=1000) 
    

    
    #' @description
    #' xxx
    #'
    #' @param cond A number
    #' @param range A number
    #' 
    #' @return Nothing.
    #' 
    ToggleState_Screens = function(cond, range){
      if(verbose) cat(paste0('::ToggleState_Steps() from - ', id, '\n\n'))
      #browser()
      if (is.enabled())
        lapply(range, function(x){
          cond <- cond && !(rv.process$status[x] == global$SKIPPED)
          
          #Send to TL the enabled/disabled tags
          rv.process$steps.enabled[x] <- cond
        })
    }
    
    
    #' @description 
    #' This function is updated each time the status vector is changed. It is 
    #' used to decide which steps must be enabled or disabled w.r.t the new
    #' status vector value. 
    #' The behaviour is the following:
    #' * All the steps before the last validated one are disabled
    #' * all the steps before a undone mandatory step are enable and the ones
    #' after this mandatory step are disabled
    #' * xxx 
    #' 
    Update_State_Screens = function(){
      if(verbose) cat(paste0('::', 'Update_State_Screens() from - ', id, '\n\n'))
      
      if (isTRUE(is.skipped())){
        ToggleState_Screens(cond = FALSE, range = 1:rv.process$length)
      } else {
        ind.max <- GetMaxValidated_AllSteps()
        if (ind.max > 0)
          ToggleState_Screens(cond = FALSE, range = 1:ind.max)
        
        if (ind.max < rv.process$length){
          # Enable all steps after the current one but the ones
          # after the first mandatory not validated
          firstM <- GetFirstMandatoryNotValidated((ind.max+1):rv.process$length)
          if (is.null(firstM)){
            ToggleState_Screens(cond = TRUE, range = (1 + ind.max):(rv.process$length))
          } else {
            ToggleState_Screens(cond = TRUE, range = (1 + ind.max):(ind.max + firstM))
            if (ind.max + firstM < rv.process$length)
              ToggleState_Screens(cond = FALSE, range = (ind.max + firstM + 1):rv.process$length)
          }
        }
      }
      # browser()
    }
    
    
    #' @description
    #' Catch a new value on the dataIn variable, sent by the caller. This value 
    #' may be NULL or a dataset.
    #' The first action is to store the dataset in the temporary variable 
    #' temp.dataIn. Then, two behaviours:
    #' * the variable is NULL. xxxx
    #' * the variable contains a dataset. xxx
    #
    observeEvent(dataIn(), ignoreNULL = F, ignoreInit = F,{
      if (verbose) cat(paste0('::observeEvent(dataIn()) from --- ', id, '\n\n'))
      #browser()
      
      Change_Current_Pos(1)
      rv.process$temp.dataIn <- dataIn()
      #ActionOn_New_DataIn() # Used by class pipeline
      
      if(is.null(dataIn())){
        print('Process : dataIn() NULL')
        ToggleState_Screens(FALSE, 1:rv.process$length)
        ToggleState_Screens(TRUE, 1)
        rv.process$original.length <- 0
      } else { # A new dataset has been loaded
        print('Process : dataIn() not NULL')
        rv.process$original.length <- length(dataIn())
        Update_State_Screens()
        ToggleState_Screens(TRUE, 1)
      }
      #browser()
    })
    

    #' @description
    #' Catches a new value of the cursor position
    observeEvent(req(!is.null(rv.process$position)), ignoreInit = T, {
      pos <- strsplit(rv.process$position, '_')[[1]][1]
      if (pos == 'last')
        rv.process$current.pos <- rv.process$length
      else if (is.numeric(pos))
        rv.process$current.pos <- rv.process$position
    })
    
    #' #' @description
    #' #' Default actions on reset pipeline or process.
    #' #' 
    #' LocalReset = function(){
    #'   if(verbose) cat(paste0('LocalReset() from - ', id, '\n\n'))
    #'   #ResetScreens()
    #'   rv.process$dataIn <- NULL
    #'   rv.process$current.pos <- 1
    #'   rv.process$status <- setNames(rep(global$UNDONE, rv.process$length), rv.process$config$steps)
    #'   Send_Result_to_Caller()
    #' }
   
    #' @description
    #' Catches a new position to show/hide the correct screen. This function
    #' also manages the enabling/disabling of the `Prev` and `Next` buttons
    #' w.r.t predefined rules (each of these buttons are disabled if there is
    #' no more steps in their direction)
    observeEvent(rv.process$current.pos, ignoreInit = F,{
      if (verbose) cat(paste0('::observe(rv$current.pos) from - ', id, '\n\n'))
      
      shinyjs::toggleState(id = "prevBtn", condition = rv.process$current.pos > 1)
      shinyjs::toggleState(id = "nextBtn", condition = rv.process$current.pos < rv.process$length)
      shinyjs::hide(selector = paste0(".page_", id))
      shinyjs::show(rv.process$config$steps[rv.process$current.pos])
    })
    
    #' @description
    #' Default actions on reset pipeline or process.
    #' 
    LocalReset = function(){
      if(verbose) cat(paste0('LocalReset() from - ', id, '\n\n'))
      #browser()
      rv.process$dataIn <- NULL
      #rv.process$temp.dataIn <- NULL
      rv.process$current.pos <- 1
      rv.process$status <- setNames(rep(global$UNDONE, rv.process$length), rv.process$config$steps)
      #Send_Result_to_Caller()
      dataOut$trigger <- as.numeric(Sys.time())
      dataOut$value <- NULL
      dataOut$name <- id
    }
    
    #' @description
    #' Show/hide an information panel if the process is entirely skipped
    #' This functions can be used for both nav_process and nav_pipeline modules
    output$SkippedInfoPanel <- renderUI({
      #if (verbose) cat(paste0(class(self)[1], '::output$SkippedInfoPanel from - ', self$id, '\n\n'))
      
      current_step_skipped <- rv.process$status[rv.process$current.pos] == global$SKIPPED
      req(current_step_skipped)
      process_entirely_skipped <- isTRUE(sum(rv.process$status) == global$SKIPPED * rv.process$length)
      
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
    
    
    #' @descripion
    #' This function uses the UI definition to:
    #' * initialize the UI (only the first screen is shown),
    #' * encapsulate the UI in a div (used to hide all screens at a time before
    #' showing the one corresponding to the current position)
    output$EncapsulateScreens <- renderUI({
      #browser()
      tagList(
        lapply(1:length(rv.process$config$ll.UI), function(i) {
          if (i==1)
            div(id = ns(rv.process$config$steps[i]),
                class = paste0("page_", id),
                rv.process$config$ll.UI[[i]]
            )
          else
            shinyjs::hidden(
              div(id =  ns(rv.process$config$steps[i]),
                  class = paste0("page_", id),
                  rv.process$config$ll.UI[[i]]
              )
            )
        }
        )
      )
      
      
    })
    
    
    
    output$show_Debug_Infos <- renderUI({
      tagList(
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
      if (verbose) cat(paste0('::output$show_dataIn from - ', id, '\n\n'))
      req(dataIn())
      tagList(
        # h4('show dataIn()'),
        lapply(names(dataIn()), function(x){tags$p(x)})
      )
    })
    
    output$show_rv_dataIn <- renderUI({
      if (verbose) cat(paste0('::output$show_rv_dataIn from - ', id, '\n\n'))
      req(rv.process$dataIn)
      tagList(
        # h4('show dataIn()'),
        lapply(names(rv.process$dataIn), function(x){tags$p(x)})
      )
    })
    
    output$show_rv_dataOut <- renderUI({
      if (verbose) cat(paste0('::output$show_rv_dataOut from - ', id, '\n\n'))
      tagList(
        #h4('show dataOut$value'),
        lapply(names(dataOut$value), function(x){tags$p(x)})
      )
    })
    
    
    output$show_status <- renderUI({
      tagList(lapply(1:rv.process$length, 
                     function(x){
                       color <- if(rv.process$steps.enabled[x]) 'black' else 'lightgrey'
                       if (x == rv.process$current.pos)
                         tags$p(style = paste0('color: ', color, ';'),
                                tags$b(paste0('---> ', rv.process$config$steps[x], ' - ', GetStringStatus(rv.process$status[[x]])), ' <---'))
                       else 
                         tags$p(style = paste0('color: ', color, ';'),
                                paste0(rv.process$config$steps[x], ' - ', GetStringStatus(rv.process$status[[x]])))
                     }))
    })
    
    output$show_tag_enabled <- renderUI({
      tagList(
        p(paste0('steps.enabled = ', paste0(as.numeric(rv.process$steps.enabled), collapse=' '))),
        p(paste0('enabled() = ', as.numeric(is.enabled())))
      )
    })
    
  #  observeEvent(dataOut$trigger, { browser()})
    list(steps.enabled = reactive({rv.process$steps.enabled})
         #status = reactive({rv.process$status})
    )
    
    
  }
  )
}