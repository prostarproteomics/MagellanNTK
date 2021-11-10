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
#' @param nav.mode A `character(1)` indicating the type of workflow. It can be
#' either 'process' (for a simple workflow) or 'pipeline' (for a composed 
#' workflow). Default is NULL: a value is necessary.
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
#'   nav.mode = 'process',
#'   dataIn = reactive({feat1})
#'   )
#' }
#' shinyApp(ui, server)
#' }
#' 
#' @importFrom stats setNames
#' 
mod_nav_server <- function(id,
                           nav.mode = NULL,
                           dataIn = reactive({NULL}),
                           is.enabled = reactive({TRUE}),
                           remoteReset = reactive({FALSE}),
                           is.skipped = reactive({FALSE})
                           ){
  
  verbose <- FALSE
  
  
  
  ###-------------------------------------------------------------###
  ###                                                             ###
  ### ------------------- MODULE SERVER --------------------------###
  ###                                                             ###
  ###-------------------------------------------------------------###
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Declare reactiveValues common to both process and pipeline
    eval(str2expression(GetCode_Declare_reactiveValues()))
    
    
    if (is.null(nav.mode) || !(nav.mode %in% c('process', 'pipeline')))
        stop("'nav.mode' must be either 'process' or 'pipeline'.")
    
    switch (nav.mode,
            pipeline = {
              # Declare reactiveValues specific to pipeline
              eval(str2expression(GetCode_Declare_Pipeline_reactiveValues()))
              
              eval(str2expression(GetCode_Update_Data2send_Vector()))
              eval(str2expression(GetCode_PrepareData2Send()))
                  
              # Catch the dataset returned by the process module. The event is 
              # observed by a change in the 'trigger' value
              # and instantiate the rv$dataOut variable
              # which is the return value of the module.
              # This function is only used to communicate between the process module 
              # and and the caller
              eval(str2expression(GetCode_ActionOn_Data_Trigger()))
                  
                  
              eval(str2expression(GetCode_ActionOn_NewPosition()))
              
              eval(str2expression(GetCode_observeEvent_currentPos_pipeline()))
                  
              # Catch the returned values of the process 
              eval(str2expression(GetCode_observeEvent_returnValuesOfProcesses()))
                  
              eval(str2expression(GetCode_ResetChildren()))
                  
              },
            process = {
              # Catch the dataset returned by the process module. The event is 
              # observed by a change in the 'trigger' value
              # and instantiate the rv$dataOut variable
              # which is the return value of the module.
              # This function is only used to communicate between the process module 
              # and and the caller
              eval(str2expression(GetCode_observeEvent_dataOut_trigger()))
                  
                  
              # Catches a new value of the cursor position
              eval(str2expression(GetCode_observeEvent_rv_position()))
                  
              # Catches a new position to show/hide the correct screen. This function
              # also manages the enabling/disabling of the `Prev` and `Next` buttons
              # w.r.t predefined rules (each of these buttons are disabled if there is
              # no more steps in their direction)
              eval(str2expression(GetCode_observeEvent_currentPos_process()))
              }
        )

    
    
    # Launch the renderUI function for the user interface of the module
    # Apparently, the renderUI() cannot be stored in the expression
    output$nav_mod_ui <- renderUI({
      FUN <- paste0('GetCode_', nav.mode, '_ui')
      eval(str2expression(do.call(FUN, list())))
    })
    
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
    eval(str2expression(GetCode_NavPage_Managment()))
    eval(str2expression(GetCode_observeEvent_stepsStatus()))
    eval(str2expression(GetCode_observeEvent_isEnabled()))
    eval(str2expression(GetCode_observeEvent_isSkipped()))
    eval(str2expression(GetCode_observeEvent_rstBtn()))
    eval(str2expression(GetCode_observeEvent_remoteReset()))
    eval(str2expression(GetCode_observeEvent_modal_ok()))
    eval(str2expression(GetCode_LocalReset()))
    eval(str2expression(GetCode_ToggleState_Screens()))
    eval(str2expression(GetCode_ToggleState_NavBtns()))
    
    # Initialisation of the nav module
    FUN <- paste0('GetCode_Init_', nav.mode, '_Server')
    eval(str2expression(do.call(FUN, list())))
    
    # Get the name of the current (active) step
    eval(str2expression(GetCode_CurrentStepName()))
    
    # Show/hide an information panel if the process is entirely skipped
    # This functions can be used for both nav_process and nav_pipeline modules
    eval(str2expression(GetCode_SkippedInfoPanel_UI()))
     
    # This function uses the UI definition to:
    # * initialize the UI (only the first screen is shown),
    # * encapsulate the UI in a div (used to hide all screens at a time before
    # showing the one corresponding to the current position)
    eval(str2expression(GetCode_EncapsulateScreens()))
    
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
    #  observeEvent(dataOut$trigger, { browser()})
    list(dataOut = reactive({dataOut}),
         steps.enabled = reactive({rv$steps.enabled}),
         status = reactive({rv$steps.status})
    )
    
    
  })
  
}