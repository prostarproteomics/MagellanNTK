btn_style <- "display:inline-block; vertical-align: middle; padding: 7px"

mod_def_Protein_Normalization_ui <- function(id){
  ns <- NS(id)
   # mod_navigation_process_ui(ns('nav_pipe_prot_norm')),
    tagList(
      shinyjs::useShinyjs(),
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
                   mod_timeline_h_ui(ns('timeline'))
               ),
        div(style = btn_style,
                   actionButton(ns("nextBtn"),">>",
                                class = PrevNextBtnClass,
                                style='padding:4px; font-size:80%')
        )
      ),
      # fluidRow(
      #   column(width=1, shinyjs::disabled(
      #     actionButton(ns("prevBtn"), "<<",
      #                  class = PrevNextBtnClass,
      #                  style='padding:4px; font-size:80%')
      #   )),
      #   column(width=1, actionButton(ns("rstBtn"), "Reset",
      #                                class = redBtnClass,
      #                                style='padding:4px; font-size:80%')),
      #   column(width=9, mod_timeline_h_ui(ns('timeline'))),
      #   column(width=1, actionButton(ns("nextBtn"),">>",
      #                                class = PrevNextBtnClass,
      #                                style='padding:4px; font-size:80%'))
      # ),
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



mod_def_Protein_Normalization_server <- function(id,
                               dataIn = NULL,
                               tag.enabled = reactive({TRUE}),
                               reset = reactive({FALSE}),
                               position = reactive({NULL}),
                               skipped = reactive({NULL})
                               ){
  
  
  ###-------------------------------------------------------------###
  ###                                                             ###
  ### ------------------- MODULE SERVER --------------------------###
  ###                                                             ###
  ###-------------------------------------------------------------###
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    #' @field modal_txt xxx
    modal_txt <- "This action will reset this process. The input dataset will be the output of the last previous
                      validated process and all further datasets will be removed"
    
    #' @field global xxxx
    global <- list(
      VALIDATED = 1,
      UNDONE = 0,
      SKIPPED = -1
    )
    
    
    
    config <- reactiveValues(
      name = 'Protein_Normalization',
      steps = c('Description', 'Step1', 'Step2', 'Step3'),
      mandatory = c(T, F, T, T),
      ll.UI = list( uiOutput(ns("Description")),
                    uiOutput(ns("Step1")),
                    uiOutput(ns("Step2")),
                    uiOutput(ns("Step3"))
      )
    )
    
    
    output$EncapsulateScreens <- renderUI({
      tagList(
        lapply(1:length(config$ll.UI), function(i) {
          if (i==1)
            div(id = ns(config$steps[i]),
                class = paste0("page_", id),
                config$ll.UI[[i]]
            )
          else
            shinyjs::hidden(
              div(id =  ns(config$steps[i]),
                  class = paste0("page_", id),
                  config$ll.UI[[i]]
              )
            )
        }
        )
      )
         
      
    })
    
    
    
   

   # mod_navigation_process_server(id = 'nav_pipe_prot_norm', 
   #                               config = config,
   #                               status = reactive({rv.process$status}),
   #                               tag.enabled = reactive({TRUE}),
   #                               reset = reactive({FALSE}),
   #                               position = reactive({NULL}),
   #                               skipped = reactive({NULL})
   # )
   

# Define default selected values for widgets
widgets.default.values <- list(
  select1 =1,
  select2 = NULL,
  select3 = 1,
  select2_1 = 1,
  select2_2 = 1
)

rv.widgets <- reactiveValues()
# Set widgets selected values to their default
rv.widgets$select1 <- widgets.default.values$select1
rv.widgets$select2 <- widgets.default.values$select2
rv.widgets$select3 <- widgets.default.values$select3
rv.widgets$select2_1 <- widgets.default.values$select2_1
rv.widgets$select2_2 <- widgets.default.values$select2_2


rv.process <- reactiveValues(
  parent = NULL,
  
  status = NULL,
  dataIn = NULL,
  temp.dataIn = NULL,
  current.pos = 1,
  tl.tags.enabled = NULL,
  test = NULL,
  length = NULL,
  config = NULL
)


#' @field dataOut xxx
dataOut <- reactiveValues(
  trigger = 0,
  value = NULL
)

# Check if the rv.process$config is correct
#'
#' @param conf A list containing the rv.process$configuration of the current object.
#' See xxx
#' 
CheckConfig = function(conf){
  if(verbose) cat(paste0('::Checkrv.process$config() from - ', id, '\n\n'))
  passed <- T
  msg <- ""
  if (!is.list(conf)){
    passed <- F
    msg <- c(msg, "'rv.process$config' is not a list")
  }
  if (length(conf)!=3){
    passed <- F
    msg <- c(msg, "The length of 'rv.process$config' is not equal to 4")
  }
  names.conf <- c("name", "steps", "mandatory")
  if (!all(sapply(names.conf, function(x){x %in% names(conf)}))){
    passed <- F
    msg <- c(msg, "The names of elements in 'rv.process$config' must be the following: 'name', 'steps', 'mandatory'")
  }
  if (length(conf$steps) != length(conf$mandatory)){
    passed <- F
    msg <- c(msg, "The length of 'steps' and 'mandatory' must be equal.")
  }
  
  passed <- T
  list(passed=passed,
       msg = msg)
}


observeEvent(id, {
  rv.widgets <- reactiveValues()
  #source(file.path('../../../R', paste0('def_', id, '.R')), local=TRUE)$value
  # browser()
  
  #setNames(lapply(self$config$steps, function(x){
  # eval(parse(text = paste0('def_', id, '(session, input, output)')))
  #}),
  # self$config$steps)
  
  
  rv.process$config <- config
  rv.process$length <- length(config$steps)
  rv.process$current.pos  <- 1
  
  # config$ll.UI[[1]] <- div(id = ns(rv.process$config$steps[1]),  
  #                         config$ll.UI[[1]])
  # for (i in 2:rv.process$length){
  #   config$ll.UI[[i]] <- shinyjs::hidden(
  #     div(id = ns(rv.process$config$steps[i]),  
  #         config$ll.UI[[i]]))
  # }
  
  
  
  rv.process$parent <- unlist(strsplit(id, split='_'))[1]
  rv.process$config <- config
  check <- CheckConfig(rv.process$config)
  if (!check$passed)
    stop(paste0("Errors in 'rv.process$config'", paste0(check$msg, collapse=' ')))
  #else
  # rv.process$config <- rv.process$config
  #browser()
  rv.process$config$mandatory <- setNames(rv.process$config$mandatory, rv.process$config$steps)
  rv.process$status = setNames(rep(global$UNDONE, length(rv.process$config$steps)), rv.process$config$steps)
  rv.process$currentStepName <- reactive({rv.process$config$steps[rv.process$current.pos]})
  rv.process$tl.tags.enabled <- setNames(rep(FALSE, length(rv.process$config$steps)), rv.process$config$steps)
  #browser()
}, priority=1000) 



#' @description
#' xxxx
#'
Send_Result_to_Caller = function(){
  if(verbose) cat(paste0('::Send_Result_to_Caller() from - ', id, '\n\n'))
  dataOut$trigger <- Timestamp()
  dataOut$value <- rv.process$dataIn
}

#' @description 
#' xxx
#' 
InitializeDataIn = function(){ 
  if(verbose) cat(paste0('InitializeDataIn() from - ', id, '\n\n'))
  rv.process$dataIn <- rv.process$temp.dataIn
}


#' @description
#' Validate a given position. To be used by xxx
#' 
#' @return Nothing.
#' 
ValidateCurrentPos <- function(){
  #browser()
  rv.process$status[rv.process$current.pos] <- global$VALIDATED
  # Either the process has been validated, one can prepare data to be sent to caller
  # Or the module has been reseted
  if (rv.process$current.pos == length(config$steps))
    Send_Result_to_Caller()
}



#' @description
#' Gives the name of the status corresponding to the code (integer).
#'
#' @param name A number
#' 
GetStringStatus = function(name){
  if (name==global$VALIDATED) "Validated"
  else if (name==global$UNDONE) "Undone"
  else if (name==global$SKIPPED) 'Skipped'
}

#' @description 
#' Returns the date and time in timestamp UNIX format.
#' 
Timestamp = function(){ 
  if(verbose) cat(paste0('::Timestamp() from - ', id, '\n\n'))
  as.numeric(Sys.time())
}




#' @description 
#' xxx
#' 
Update_State_Screens = function(){
  if(verbose) cat(paste0('::', 'Update_State_Screens() from - ', id, '\n\n'))
  
  ind.max <- GetMaxValidated_AllSteps()
  #browser()
  if (ind.max > 0) 
    ToggleState_Screens(cond = FALSE, range = 1:ind.max)
  
  
  if (ind.max < length(config$steps)){
    # Enable all steps after the current one but the ones
    # after the first mandatory not validated
    firstM <- GetFirstMandatoryNotValidated((ind.max+1):length(config$steps))
    if (is.null(firstM)){
      ToggleState_Screens(cond = TRUE, range = (1 + ind.max):(length(config$steps)))
    } else {
      ToggleState_Screens(cond = TRUE, range = (1 + ind.max):(ind.max + firstM))
      if (ind.max + firstM < length(config$steps))
        ToggleState_Screens(cond = FALSE, range = (ind.max + firstM + 1):length(config$steps))
    }
  }
  # browser()
}



#' @description 
#' xxx
#' 
GetMaxValidated_AllSteps = function(){
  if(verbose) cat(paste0( '::', 'GetMaxValidated_AllSteps() from - ', id, '\n\n'))
  val <- 0
  ind <- grep(global$VALIDATED, rv.process$status)
  if (length(ind) > 0) 
    val <-max(ind)
  val
}



#' @description 
#' xxx
#'
#' @param range xxx
#' 
GetFirstMandatoryNotValidated = function(range){
  if(verbose) cat(paste0('::', 'GetFirstMandatoryNotValidated() from - ', id, '\n\n'))
  first <- NULL
  first <- unlist((lapply(range, 
                          function(x){config$mandatory[x] && !rv.process$status[x]})))
  if (sum(first) > 0)
    min(which(first == TRUE))
  else
    NULL
}


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
  if (tag.enabled())
    lapply(range, function(x){
      cond <- cond && !(rv.process$status[x] == global$SKIPPED)
      #shinyjs::toggleState(config$steps[x], condition = cond  )
      
      #Send to TL the enabled/disabled tags
      rv.process$tl.tags.enabled[x] <- cond
    })
}

#' @description 
#' xxx
#' 
#' @param i xxx
#' 
Change_Current_Pos = function(i){ rv.process$current.pos <- i}


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
  # shinyjs::toggleState('Screens', TRUE)
  
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
Initialize_Status_Process = function(){
  if(verbose) cat(paste0('::', 'Initialize_Status_Process() from - ', id, '\n\n'))
  rv.process$status <- setNames(rep(global$UNDONE, length(config$steps)), config$steps)
}

#' @description
#' et to skipped all steps of the current object
#' 
#' @return Nothing.
#' 
Set_All_Skipped = function(){
  if(verbose) cat(paste0('::', 'Set_All_Skipped() from - ', id, '\n\n'))
  rv.process$status <- setNames(rep(global$SKIPPED, length(config$steps)), config$steps)
}

#' @description
#' et to skipped all steps of the current object
#' 
#' @return Nothing.
#' 
Discover_Skipped_Steps = function(){
  if(verbose) cat(paste0('::Discover_Skipped_Status() from - ', id, '\n\n'))
  for (i in 1:length(config$steps)){
    max.val <- GetMaxValidated_AllSteps()
    if (rv.process$status[i] != global$VALIDATED && max.val > i)
      rv.process$status[i] <- global$SKIPPED
  }
}

#' @description
#' et to skipped all steps of the current object
#' 
#' @return Nothing.
#' 
Set_All_Reset = function(){
  if(verbose) cat(paste0('::', 'Set_All_Reset() from - ', id, '\n\n'))
  
  BasicReset()
}




observeEvent(tag.enabled(), ignoreNULL = FALSE, ignoreInit = TRUE, {
  # browser()
  if (!isTRUE(tag.enabled()))
    rv.process$tl.tags.enabled <- setNames(rep(FALSE, length(config$steps)), config$steps)
})

mod_timeline_h_server(id = 'timeline',
                      config =  config,
                      status = reactive({rv.process$status}),
                      position = reactive({rv.process$current.pos}),
                      enabled = reactive({rv.process$tl.tags.enabled})
)


observeEvent(req(!is.null(position())), ignoreInit = T, {
  pos <- strsplit(position(), '_')[[1]][1]
  if (pos == 'last')
    rv.process$current.pos <- length(config$steps)
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
  Initialize_Status_Process()
  Send_Result_to_Caller()
}



#' @description 
#' Returns the date and time in timestamp UNIX format.
#' 
Timestamp = function(){ 
  if(verbose) cat(paste0('::Timestamp() from - ', id, '\n\n'))
  as.numeric(Sys.time())
}






#' @description 
#' Return the UI for a modal dialog with data selection input. If 'failed' is
#' TRUE, then display a message that the previous value was invalid.
#' 
dataModal = function() {
  
  tags$div(id="modal1", 
           modalDialog(
             span(modal_txt),
             footer = tagList(
               actionButton(ns("closeModal"), "Cancel", class='btn-info'),
               actionButton(ns("modal_ok"), "OK")
             )
           )
  )
}





##
## Common functions
##




#' @description 
#' xxx
#' 
#' @param cond xxx
#' 
ToggleState_ResetBtn = function(cond){
  if(verbose) cat(paste0( '::', 'ToggleState_ResetBtn(', cond, ')) from - ', id, '\n\n'))
  
  shinyjs::toggleState('rstBtn', condition = cond)
}

output$SkippedInfoPanel <- renderUI({
  #if (verbose) cat(paste0(class(self)[1], '::output$SkippedInfoPanel from - ', self$id, '\n\n'))
  
  current_step_skipped <- rv.process$status[rv.process$current.pos] == global$SKIPPED
  entire_process_skipped <- isTRUE(sum(rv.process$status) == global$SKIPPED * rv.process$length)
  req(current_step_skipped)
  
  
  if (entire_process_skipped){
    # This case appears when the process has been skipped from the
    # pipleine. Thus, it is not necessary to show the info box because
    # it is shown below the timeline of the pipeline
  } else {
    txt <- paste0("This ", config$type, " is skipped so it has been disabled.")
    wellPanel(
      style = "background-color: #7CC9F0; opacity: 0.72; padding: 0px; align: center; vertical-align: center;",
      height = 100,
      width=300,
      align="center",
      p(style = "color: black;", paste0('Info: ',txt))
    )
  }
})





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
  shinyjs::toggleState(id = "nextBtn", condition = rv.process$current.pos < length(config$steps))
  shinyjs::hide(selector = paste0(".page_", id))
  shinyjs::show(config$steps[rv.process$current.pos])
  
  #ActionOn_NewPosition()
  
})

#' @description
#' Change current position.
#' 
#' @param direction xxx
#'
NavPage = function(direction) {
  newval <- rv.process$current.pos + direction 
  newval <- max(1, newval)
  newval <- min(newval, length(config$steps))
  rv.process$current.pos <- newval
}

observeEvent(input$prevBtn, ignoreInit = TRUE, {NavPage(-1)})
observeEvent(input$nextBtn, ignoreInit = TRUE, {NavPage(1)})





# Catch new status event

observeEvent(rv.process$status, ignoreInit = T, {
  if (verbose) cat(paste0('::observe((rv$status) from - ', id, '\n\n'))
  
  Discover_Skipped_Steps()
  # https://github.com/daattali/shinyjs/issues/166
  # https://github.com/daattali/shinyjs/issues/25
  Update_State_Screens()
  
  #shinyjs::delay(1000, Update_State_Screens())
})

observeEvent(input$rstBtn, {
  if (verbose) cat(paste0('::observeEvent(input$rstBtn) from - ', id, '\n\n'))
  showModal(dataModal())
})

observeEvent(input$closeModal, {removeModal() })


observeEvent(req(input$modal_ok > 0), ignoreInit=F, {
  if (verbose) cat(paste0('::observeEvent(req(c(input$modal_ok))) from - ', id, '\n\n'))
  rv.process$local.reset <- input$rstBtn
  Set_All_Reset()
  removeModal()
})

observeEvent(req(reset()), ignoreInit=F, ignoreNULL=T, {
  if (verbose) cat(paste0('::observeEvent(req(c(input$modal_ok))) from - ', id, '\n\n'))
  Set_All_Reset()
})

output$SkippedInfoPanel <- renderUI({
  if (verbose) cat(paste0('::output$SkippedInfoPanel from - ', id, '\n\n'))
  #browser()
  
  current_step_skipped <- rv.process$status[rv.process$current.pos] == global$SKIPPED
  entire_process_skipped <- isTRUE(sum(rv.process$status) == global$SKIPPED * length(config$steps))
  req(current_step_skipped)
  
  
  if (entire_process_skipped){
    # This case appears when the process has been skipped from the
    # pipleine. Thus, it is not necessary to show the info box because
    # it is shown below the timeline of the pipeline
  } else {
    txt <- paste0("This ", config$type, " is skipped so it has been disabled.")
    wellPanel(
      style = "background-color: #7CC9F0; opacity: 0.72; padding: 0px; align: center; vertical-align: center;",
      height = 100,
      width=300,
      align="center",
      p(style = "color: black;", paste0('Info: ',txt))
    )
  }
})


###-----------------------------------------------------------------------------------------------------




### ----------------------------------------------------------------------------------------------------

###### ------------------- Code for Description (step 0) -------------------------    #####
output$Description <- renderUI({
  rv.process$tl.tags.enabled
  #browser()
  wellPanel(
    tagList(
      includeMarkdown( system.file("app/md", paste0(config$name, ".md"), package="Magellan")),
      uiOutput(ns('datasetDescription')),
      if (isTRUE(rv.process$tl.tags.enabled['Description']))
        actionButton(ns('btn_validate_Description'), 
                     paste0('Start ', config$name),
                     class = btn_success_color)
      else
      shinyjs::disabled(
        actionButton(ns('btn_validate_Description'), 
                   paste0('Start ', config$name),
                   class = btn_success_color)
    )
    )
  )
 # browser()
})

observeEvent(input$btn_validate_Description, ignoreInit = T, ignoreNULL=T, {
  InitializeDataIn()
  ValidateCurrentPos()
})


###### ------------------- Code for step 1 -------------------------    #####

observeEvent(input$btn_validate_Step1, ignoreInit = T, {
  # Add your stuff code here
  ValidateCurrentPos()
})


observeEvent(input$select1,{rv.widgets$select1 <- input$select1})
observeEvent(input$select2,{rv.widgets$select2 <- input$select2})
observeEvent(input$select3,{rv.widgets$select3 <- input$select3})
observeEvent(input$select2,{rv.widgets$select2_1 <- input$select2_1})
observeEvent(input$select3,{rv.widgets$select2_2 <- input$select2_2})




output$test1 <-renderUI({
  #rv.process$tl.tags.enabled
  rv.widgets$select1
    if (rv.process$tl.tags.enabled['Step1'])
    selectInput(ns('select1'), 'Select 1 in renderUI',
                choices = 1:4,
                selected = rv.widgets$select1,
                width = '150px')
  else
    shinyjs::disabled(
      selectInput(ns('select1'), 'Select 1 in renderUI',
                  choices = 1:4,
                  selected = rv.widgets$select1,
                  width = '150px')
      )
})



output$test2 <-renderUI({

  rv.process$tl.tags.enabled
  if (rv.process$tl.tags.enabled['Step1'])
    selectInput(ns('select2'), 'Select 2 in renderUI', 
                choices = 1:3,
                selected = rv.widgets$select2,
                width = '150px')
  else
    shinyjs::disabled(
      selectInput(ns('select2'), 'Select 2 in renderUI', 
                  choices = 1:4,
                  selected = rv.widgets$select2,
                  width = '150px')
    )
})




# ------------------------ STEP 1 : UI ------------------------------------
output$Step1 <- renderUI({
  #rv.process$tl.tags.enabled
  name <- 'Step1'
  wellPanel(id = ns('toto'),
    actionButton(ns('btn1'), 'Btn 1'),
    tagList(
      div(id=ns('Step1a'),
          div(style="display:inline-block; vertical-align: middle;padding-right: 20px;",
              uiOutput(ns('test1'))
          ),
          div(style="display:inline-block; vertical-align: middle;padding-right: 20px;",
              uiOutput(ns('test2'))
          ),
          div(style="display:inline-block; vertical-align: middle; padding-right: 40px;",
              if (rv.process$tl.tags.enabled['Step1'])
                selectInput(ns('select3'), 'Select step 3', 
                          choices = 1:3, 
                          selected = rv.widgets$select3,
                          width = '150px')
              else
                shinyjs::disabled(
                  selectInput(ns('select3'), 'Select step 3', 
                              choices = 1:5, 
                              selected = rv.widgets$select3,
                              width = '150px')
                )
          ),
          div(style="display:inline-block; vertical-align: middle;padding-right: 20px;",
              if (rv.process$tl.tags.enabled['Step1'])
                actionButton(ns(paste0('btn_validate_', name)), 
                             'Perform',
                             class = btn_success_color)
              else
                shinyjs::disabled(
                  actionButton(ns(paste0('btn_validate_', name)),
                               'Perform',
                               class = btn_success_color)
                  )
           )
      )
    )
  )
})

#------------- Code for step 2 ---------------

observeEvent(input$btn_validate_Step2, ignoreInit = T, {
  # Add your stuff code here
  ValidateCurrentPos()
})

output$select2_1_UI <-renderUI({
  rv.process$tl.tags.enabled
  if (rv.process$tl.tags.enabled['Step2'])
      selectInput(ns('select2_1'), 'Select 2_1 in renderUI', 
              choices = 1:3, 
              width = '150px')
  else
    shinyjs::disabled(
      selectInput(ns('select2_1'), 'Select 2_1 in renderUI', 
                  choices = 1:3, 
                  width = '150px')
    )
})

output$Step2 <- renderUI({
  rv.process$tl.tags.enabled
  name <- 'Step2'
  wellPanel(
    tagList(
      div(id=ns(name),
          div(style="display:inline-block; vertical-align: middle;padding-right: 20px;",
              uiOutput(ns('select2_1_UI'))
          ),
          div(style="display:inline-block; vertical-align: middle; padding-right: 40px;",
              if (rv.process$tl.tags.enabled['Step2'])
                selectInput(ns('select2_2'), 'Select 2_2', 
                          choices = 1, 
                          width = '150px')
              else
                shinyjs::disabled(
                  selectInput(ns('select2_2'),
                              'Select 2_2', 
                              choices = 1, 
                              width = '150px')
                  )
          ),
          div(style="display:inline-block; vertical-align: middle;padding-right: 20px;",
              if (rv.process$tl.tags.enabled['Step2'])
                actionButton(ns(paste0('btn_validate_', name)), 
                             'Perform',
                             class = btn_success_color)
              else
                shinyjs::disabled(
                actionButton(ns(paste0('btn_validate_', name)), 
                             'Perform',
                             class = btn_success_color)
                )
          )
      )
    )
  )
})


#------------- Code for step 3 ---------------

output$Step3 <- renderUI({
  rv.process$tl.tags.enabled
  tagList(
    h3('Step 3'),
    if (rv.process$tl.tags.enabled['Step3'])
      actionButton(ns('btn_validate_Step3'), 
                   'Perform',
                   class = btn_success_color)
    else
      shinyjs::disabled(
        actionButton(ns('btn_validate_Step3'), 
                     'Perform',
                     class = btn_success_color)
        )
  )
})


observeEvent(input$btn_validate_Step3, ignoreInit = T, {
  # Add your stuff code here
  rv.process$dataIn <- AddItemToDataset(rv.process$dataIn, rv.process$config$name)
  ValidateCurrentPos()
})

output$show_Debug_Infos <- renderUI({
  tagList(
    uiOutput(ns('show_tag_enabled')),
    fluidRow(
      column(width=2,
             tags$b(h4(style = 'color: blue;', paste0("Global input of ", config$type))),
             uiOutput(ns('show_dataIn'))),
      column(width=2,
             tags$b(h4(style = 'color: blue;', paste0("Temp input of ", config$type))),
             uiOutput(ns('show_rv_dataIn'))),
      column(width=2,
             tags$b(h4(style = 'color: blue;', paste0("Output of ", config$type))),
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
  tagList(lapply(1:length(config$steps), 
                 function(x){
                   color <- if(rv.process$tl.tags.enabled[x]) 'black' else 'lightgrey'
                   if (x == rv.process$current.pos)
                     tags$p(style = paste0('color: ', color, ';'),
                            tags$b(paste0('---> ', config$steps[x], ' - ', GetStringStatus(rv.process$status[[x]])), ' <---'))
                   else 
                     tags$p(style = paste0('color: ', color, ';'),
                            paste0(config$steps[x], ' - ', GetStringStatus(rv.process$status[[x]])))
                 }))
})

output$show_tag_enabled <- renderUI({
  tagList(
    p(paste0('tl.tags.enabled = ', paste0(as.numeric(rv.process$tl.tags.enabled), collapse=' '))),
    p(paste0('enabled() = ', as.numeric(tag.enabled())))
  )
})




reactive({dataOut})


  }
)
}