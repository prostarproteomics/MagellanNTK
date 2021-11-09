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
  
  nav.mode <- "process"
  
  ###-------------------------------------------------------------###
  ###                                                             ###
  ### ------------------- MODULE SERVER --------------------------###
  ###                                                             ###
  ###-------------------------------------------------------------###
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
    verbose <- FALSE
    
    
    # Reactive values that will be used to output the current dataset when 
    # the last step is validated
    dataOut <- reactiveValues(
      trigger = NULL,
      value = NULL
    )
    
    
    #These reactive values are specific to this instance of mod_nav_process_server
    rv <- reactiveValues(
      #' @field proc contains the return value of the process module that has been called 
      proc = NULL,
      
      #' @field status A booelan vector which contains the status (validated,
      #' skipped or undone) of the steps
      steps.status = NULL,
      
      #' @field dataIn Contains the dataset passed by argument to the module server
      dataIn = NULL,
      
      #' @field temp.dataIn This variable is used to serves as a tampon between 
      #' the input of the module and the functions. 
      temp.dataIn = NULL,
      
      #' @field steps.enabled Contains the value of the parameter 'is.enabled'
      steps.enabled = NULL,
      
      #' @field current.pos Stores the current cursor position in the timeline and 
      #' indicates which of the process' steps is active
      current.pos = 1,
      
      length = NULL,
      config = NULL
    )
    
    
    
    
    
    
    eval(str2expression(GetCode_observeEvent_dataIn()))
    eval(str2expression(GetCode_Send_Result_to_Caller()))
    eval(str2expression(GetCode_Update_State_Screens()))
    eval(str2expression(GetCode_EncapsulateScreens()))
    eval(str2expression(GetCode_GetStringStatus()))
    eval(str2expression(GetCode_GetMaxValidated_AllSteps()))
    eval(str2expression(GetCode_GetMaxValidated_BeforePos()))
    eval(str2expression(GetCode_GetFirstMandatoryNotValidated()))
    eval(str2expression(GetCode_Change_Current_Pos()))
    eval(str2expression(GetCode_Set_All_Skipped()))
    eval(str2expression(GetCode_Unskip_All_Steps()))
    eval(str2expression(GetCode_Discover_Skipped_Steps()))
    eval(str2expression(GetCode_dataModal()))
    eval(str2expression(GetCode_ToggleState_ResetBtn()))
    eval(str2expression(GetCode_NavPage()))
    eval(str2expression(GetCode_observeEvent_stepsStatus()))
    eval(str2expression(GetCode_observeEvent_isSkipped()))
    eval(str2expression(GetCode_observeEvent_rstBtn()))
    eval(str2expression(GetCode_observeEvent_remoteReset()))
    eval(str2expression(GetCode_observeEvent_modal_ok()))
    eval(str2expression(GetCode_LocalReset()))
    eval(str2expression(GetCode_ToggleState_Screens()))
    eval(str2expression(GetCode_ToggleState_NavBtns()))
    eval(str2expression(GetCode_InitProcessServer()))
    eval(str2expression(GetCode_observeEvent_isEnabled()))
    eval(str2expression(GetCode_SkippedInfoPanel_UI()))
    
    
    
    # This text is showed in the modal when the user click on the 'Reset' button.
    modal_txt <- "This action will reset this process. The input dataset will be the output of the last previous
                      validated process and all further datasets will be removed"
    
    
    
    
    
    
    
    observeEvent(input$closeModal, {removeModal() })
    
    observeEvent(input$prevBtn, ignoreInit = TRUE, {NavPage(-1)})
    observeEvent(input$nextBtn, ignoreInit = TRUE, {NavPage(1)})
    
    
    
    # This function uses the UI definition to:
    # * initialize the UI (only the first screen is shown),
    # * encapsulate the UI in a div (used to hide all screens at a time before
    # showing the one corresponding to the current position)
    output$EncapsulateScreens_ui <- renderUI({
      EncapsulateScreens()
    })
    

    
    
    # Catch the dataset returned by the process module. The event is observed by a change in the 'trigger' value
    # and instantiate the rv$dataOut variable
    # which is the return value of the module.
    # This function is only used to communicate between the process module and and the caller
    observeEvent(rv$proc$dataOut()$trigger, ignoreNULL = TRUE, ignoreInit = TRUE, {
      
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
    

    # Catches a new value of the cursor position
    observeEvent(req(!is.null(rv$position)), ignoreInit = TRUE, {
      pos <- strsplit(rv$position, '_')[[1]][1]
      if (pos == 'last')
        rv$current.pos <- rv$length
      else if (is.numeric(pos))
        rv$current.pos <- rv$position
    })
 
    
    # Catches a new position to show/hide the correct screen. This function
    # also manages the enabling/disabling of the `Prev` and `Next` buttons
    # w.r.t predefined rules (each of these buttons are disabled if there is
    # no more steps in their direction)
    observeEvent(rv$current.pos, ignoreInit = TRUE, {
      if (verbose) cat(yellow(paste0(id, '::observeEvent(rv$current.pos)\n\n')))
      
      ToggleState_NavBtns()
      # Hide all screens 
      shinyjs::hide(selector = paste0(".page_", id))
      
      #Show the current step which is identified by its name. This point is very important
      # and need that the renderUI functions of the process to be strickly well named
      shinyjs::show(rv$config$steps[rv$current.pos])
      
      })

    
    
    
    
    
    
    # The following functions are only there for dev and debugging reasons
    # They will not be part of the final code
    
    output$show_Debug_Infos <- renderUI({
      tagList(
        h3(paste0('module process "', id, '"')),
        uiOutput(ns('show_tag_enabled')),
        fluidRow(
          column(width=2,
                 tags$b(h4(style = 'color: blue;', paste0("dataIn() ", rv$config$type))),
                 uiOutput(ns('show_dataIn'))),
          column(width=2,
                 tags$b(h4(style = 'color: blue;', paste0("rv$dataIn ", rv$config$type))),
                 uiOutput(ns('show_rv_dataIn'))),
          column(width=2,
                 tags$b(h4(style = 'color: blue;', paste0("dataOut$value ", rv$config$type))),
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
    #   req(rv$dataIn)
    #   tagList(
    #     # h4('show dataIn()'),
    #     lapply(names(rv$dataIn), function(x){tags$p(x)})
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
      tagList(lapply(seq_len(rv$length), 
                     function(x){
                       color <- if(rv$steps.enabled[x]) 'black' else 'lightgrey'
                       if (x == rv$current.pos)
                         tags$p(style = paste0('color: ', color, ';'),
                                tags$b(paste0('---> ', rv$config$steps[x], ' - ', GetStringStatus(rv$steps.status[[x]])), ' <---'))
                       else 
                         tags$p(style = paste0('color: ', color, ';'),
                                paste0(rv$config$steps[x], ' - ', GetStringStatus(rv$steps.status[[x]])))
                     }))
    })
    
    output$show_tag_enabled <- renderUI({
      tagList(
        p(paste0('steps.enabled = ', paste0(as.numeric(rv$steps.enabled), collapse=' '))),
        p(paste0('enabled() = ', as.numeric(is.enabled())))
      )
    })

    
    
    output$show_rv_dataIn <- renderUI({
     # if (verbose) cat(paste0('::output$show_rv_dataIn from - ', id, "\n\n"))
      req(rv$dataIn)
      tagList(
        # h4('show dataIn()'),
        lapply(names(rv$dataIn), function(x){tags$p(x)})
      )
    })

    
    # The return value of the nav_process module server
    # The item 'dataOut' has been updated by the module process and it is returned to the
    # function that has called this nav_process module (it can be a module, a Shiny app or another nav module
    # for example, nav_pipeline)
    #  observeEvent(dataOut$trigger, { browser()})
    list(dataOut = reactive({dataOut}),
         steps.enabled = reactive({rv$steps.enabled})
         #steps.status = reactive({rv$steps.status})
    )
    
    
  }
  )
}