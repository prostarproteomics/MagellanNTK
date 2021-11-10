

#' @title xxx
#' 
#' @description 
#' xxxxxx
#' 
#' @noRd
#' 
#' @export
#'
mod_nav_pipeline_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    uiOutput(ns('nav_pipeline_ui')),
    wellPanel(title = 'foo',
              uiOutput(ns('show_Debug_Infos'))
    )
    
  )
}



#' @title xxx
#' 
#' @description 
#' xxxxxx
#' 
#' @param id xxx
#' 
#' @param dataIn xxx
#' @param is.enabled xxx
#' @param remoteReset xxx
#' @param is.skipped xxx
#' 
#' @export
#' 
#' @return xxx
#' 
#' @examples 
#' \donttest{
#' library(shiny)
#' library(shinyBS)
#' library(crayon)
#' ui <- fluidPage(
#'   mod_nav_pipeline_ui('Protein')
#' )
#' server <- function(input, output){
#' mod_nav_pipeline_server(id = 'Protein',
#'                           dataIn = reactive({QFeatures::feat1})
#'   )
#' }
#' shinyApp(ui, server)
#' }
#' 
mod_nav_pipeline_server <- function(id,
                                   dataIn = reactive({NULL}),
                                   is.enabled = reactive({TRUE}),
                                   remoteReset = reactive({FALSE}),
                                   is.skipped = reactive({FALSE})
){
  
  nav.mode <- "pipeline"
  
  ###-------------------------------------------------------------###
  ###                                                             ###
  ### ------------------- MODULE SERVER --------------------------###
  ###                                                             ###
  ###-------------------------------------------------------------###
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
    switch (nav.mode,
            pipeline = {
              # Launch the renderUI function for the user interface of the module
              eval(str2expression(GetCode_Pipeline_ui()))
              
              eval(str2expression(GetCode_Update_Data2send_Vector()))
              eval(str2expression(GetCode_PrepareData2Send()))
              eval(str2expression(GetCode_ActionOn_Data_Trigger()))
              eval(str2expression(GetCode_ActionOn_NewPosition()))
              eval(str2expression(GetCode_observeEvent_currentPos_pipeline()))
            },
            process = {
              # Launch the renderUI function for the user interface of the module
              eval(str2expression(GetCode_Process_ui()))
              eval(str2expression(GetCode_observeEvent_dataOut_trigger()))
              eval(str2expression(GetCode_observeEvent_currentPos_process()))
              
            }
    )
    
    
    
    
    

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
    

    
    
    verbose <- FALSE
    # Specific to pipeline module
    # Used to store the return values (lists) of child processes
    tmp.return <- reactiveValues()
    
    # Used to xxx
    rv.child <- reactiveValues(
      # A vector of boolean where each element indicates if the corresponding
      # child if enable or disable
      enabled = NULL,
      
      # xxxx
      reset = NULL,
      
      # A vector of integers where each element denotes the current position 
      # of the corresponding element.
      position = NULL,
      
      # xxxx
      data2send = NULL
    )
    
    # Used to xxx
    rv <- reactiveValues(
      proc = NULL,
      
      steps.status = NULL,
      
      dataIn = NULL,
      
      temp.dataIn = NULL,
      
      # A vector of boolean where each element indicates whether 
      # the corresponding process is enabled or disabled
      steps.enabled = NULL,
      
      # A vector of boolean where each element indicates whether 
      # the corresponding process is skipped or not
      steps.skipped = NULL,
      
      # A vector of integers that indicates if each step must be reseted
      # This is an information sent to the child processes. Each time a child 
      # process must be reseted, the corresponding element is incremented
      # in order to modify its value. Thus, it can be catched by Shiny observers
      resetChildren = NULL
    )
    
    #' @field modal_txt xxx
    modal_txt <- "This action will reset this pipeline. The input dataset will be the output of the last previous
                      validated process and all further datasets will be removed"
    
    
    
    
    

    
    eval(str2expression(GetCode_Send_Result_to_Caller()))
    eval(str2expression(GetCode_observeEvent_dataIn()))
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
    eval(str2expression(GetCode_InitPipelineServer()))
    
    # Show/hide an information panel if the process is entirely skipped
    # This functions can be used for both nav_process and nav_pipeline modules
    eval(str2expression(GetCode_SkippedInfoPanel_UI()))
    
    
    
    
    ################################################################
    #
    #
    ############################################################"
    
    output$EncapsulateScreens_ui <- renderUI({
      EncapsulateScreens()
    })
  

    
   
    
    CurrentStepName <- reactive({
      cat(yellow(paste0('::GetCurrentStepName() from - ', id, '\n')))
      rv$config$steps[rv$current.pos]
    })
    
    
    
    # Catch the returned values of the process                                                           
    observeEvent(lapply(rv$config$steps, 
                        function(x){
                          tmp.return[[x]]$dataOut()$trigger}), ignoreInit = TRUE, {
                            if(verbose) cat(paste0('observeEvent(trigger) from - ', id, "\n\n"))
                            #browser()
                            ActionOn_Data_Trigger()
                          })
    

    
    
    
    
    eval(str2expression(GetCode_ActionOn_Data_Trigger()))
    eval(str2expression(GetCode_ResetChildren()))
    

    
    output$show_Debug_Infos <- renderUI({
      tagList(
        h3(paste0('module pipeline "', id, '"')),
        uiOutput(ns('show_tag_enabled')),
        fluidRow(
          column(width=2,
                 tags$b(h4(style = 'color: blue;', paste0("Global input of ", rv$config$type))),
                 uiOutput(ns('show_dataIn'))),
          column(width=2,
                 tags$b(h4(style = 'color: blue;', paste0("Temp input of ", rv$config$type))),
                 uiOutput(ns('show_rv_dataIn'))),
          column(width=2,
                 tags$b(h4(style = 'color: blue;', paste0("Output of ", rv$config$type))),
                 uiOutput(ns('show_rv_dataOut'))),
          column(width=4,
                 tags$b(h4(style = 'color: blue;', "status")),
                 uiOutput(ns('show_status')))
        )
      )
    })
    
    ###########---------------------------#################
    output$show_dataIn <- renderUI({
     req(dataIn())
      tagList(
        lapply(names(dataIn()), function(x){tags$p(x)})
      )
    })
    
    output$show_rv_dataIn <- renderUI({
      req(rv$dataIn)
      tagList(
        lapply(names(rv$dataIn), function(x){tags$p(x)})
      )
    })
    
    output$show_rv_dataOut <- renderUI({
     tagList(
        lapply(names(dataOut$value), function(x){tags$p(x)})
      )
    })
    
    
    output$show_status <- renderUI({
      tagList(
        lapply(seq_len(rv$length), 
                     function(x){
                       color <- if(rv$steps.enabled[x]) 'black' else 'lightgrey'
                       if (x == rv$current.pos)
                         tags$p(style = paste0('color: ', color, ';'),
                                tags$b(paste0('---> ', rv$config$steps[x], ' - ', GetStringStatus(rv$steps.status[[x]])), ' <---'))
                       else 
                         tags$p(style = paste0('color: ', color, ';'),
                                paste0(rv$config$steps[x], ' - ', GetStringStatus(rv$steps.status[[x]])))
                     })
        )
    })
    
    output$show_tag_enabled <- renderUI({
      tagList(
        p(paste0('steps.enabled = ', paste0(as.numeric(rv$steps.enabled), collapse=' '))),
        p(paste0('enabled() = ', as.numeric(is.enabled())))
      )
    })
    
    
    list(dataOut = reactive({dataOut}),
         steps.enabled = reactive({rv$steps.enabled}),
         status = reactive({rv$steps.status})
         )
    
  }
  )
}