redBtnClass <- "btn-danger"
PrevNextBtnClass <- "btn-info"
btn_success_color <- "btn-success"
optionsBtnClass <- "info"

btn_style <- "display:inline-block; vertical-align: middle; padding: 7px"

verbose <- F

#' process UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_navigation_process_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    # div(style = "padding: 0px",
    #   div(style = btn_style,
    #              shinyjs::disabled(
    #                actionButton(ns("prevBtn"), "<<",
    #                             class = PrevNextBtnClass,
    #                             style='padding:4px; font-size:80%')
    #                ),
    #              actionButton(ns("rstBtn"), "Reset",
    #                           class = redBtnClass,
    #                           style='padding:4px; font-size:80%')
    #          ),
    #   div(style = btn_style,
    #              mod_timeline_h_ui(ns('timeline'))
    #          ),
    #   div(style = btn_style,
    #              actionButton(ns("nextBtn"),">>",
    #                           class = PrevNextBtnClass,
    #                           style='padding:4px; font-size:80%')
    #   )
    # ),
  fluidRow(
    column(width=1, shinyjs::disabled(
      actionButton(ns("prevBtn"), "<<",
                   class = PrevNextBtnClass,
                   style='padding:4px; font-size:80%')
    )),
    column(width=1, actionButton(ns("rstBtn"), "Reset",
                                 class = redBtnClass,
                                 style='padding:4px; font-size:80%')),
    column(width=9, mod_timeline_h_ui(ns('timeline'))),
    column(width=1, actionButton(ns("nextBtn"),">>",
                                 class = PrevNextBtnClass,
                                 style='padding:4px; font-size:80%'))
  ),
    div(id = ns('Screens'),
        uiOutput(ns('SkippedInfoPanel')),
        uiOutput(ns('EncapsulateScreens'))
    )

  )
}
    
#' process Server Function
#'
#' @noRd 
mod_navigation_process_server <- function(id,
                               config = NULL,
                               status = NULL,
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
    
    rv.widgets <- "<reactiveValues>"
    widgets.default.values <- list()
    
    rv.nav <- reactiveValues(
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
    
    
    ## Initialization of the timeline
    #observeEvent(req(config),{
    #  rv.nav$length <- length(config$stepsNames)
    #  rv.nav$current.pos  <- 1
      
    #  config$ll.UI[[1]] <- div(id = ns(paste0("screen", 1)),  config$ll.UI[[1]])
    #  for (i in 2:rv.nav$length){
    #    config$ll.UI[[i]] <- shinyjs::hidden(
    #      div(id = ns(config$steps[i]),  
    #          config$ll.UI[[i]]))
    #  }
      
    #})
    
    
    observeEvent(id, {
     # browser()
      rv.nav$config <- config
      rv.nav$length <- length(config$steps)
      rv.nav$current.pos  <- 1
      
      rv.nav$parent <- unlist(strsplit(id, split='_'))[1]
      
      rv.nav$status = status()
      rv.nav$currentStepName <- reactive({rv.nav$config$steps[rv.nav$current.pos]})
      rv.nav$tl.tags.enabled <- setNames(rep(FALSE, length(rv.nav$config$steps)), rv.nav$config$steps)
    }, priority=1000) 
    
    
    
    
     output$EncapsulateScreens <- renderUI({
   # browser()

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

      # config$ll.UI[[1]] <- div(id = ns(config$steps[1]),
      #                                                 config$ll.UI[[1]])
      #                           for (i in 2:rv.nav$length){
      #                             config$ll.UI[[i]] <- shinyjs::hidden(
      #                               div(id = ns(config$steps[i]),
      #                                   config$ll.UI[[i]]))
      #                           }
      # config$ll.UI                  
                               
     })
    
     
    
    
    observeEvent(tag.enabled(), ignoreNULL = FALSE, ignoreInit = TRUE, {
     # browser()
      if (!isTRUE(tag.enabled()))
        rv.nav$tl.tags.enabled <- setNames(rep(FALSE, length(config$steps)), config$steps)
    })
    
    mod_timeline_h_server(id = 'timeline',
                        config =  config,
                        status = reactive({status()}),
                        position = reactive({rv.nav$current.pos}),
                        enabled = reactive({rv.nav$tl.tags.enabled})
                        )
    
    
    observeEvent(req(!is.null(position())), ignoreInit = T, {
      pos <- strsplit(position(), '_')[[1]][1]
      if (pos == 'last')
        rv.nav$current.pos <- length(config$steps)
      else if (is.numeric(pos))
        rv.nav$current.pos <- position()
    })
    
    #' @description
    #' Default actions on reset pipeline or process.
    #' 
    BasicReset = function(){
      if(verbose) cat(paste0('BasicReset() from - ', id, '\n\n'))
      ResetScreens()
      rv.nav$dataIn <- NULL
      rv.nav$current.pos <- 1
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
    
    current_step_skipped <- rv.nav$status[rv.nav$current.pos] == global$SKIPPED
    entire_process_skipped <- isTRUE(sum(rv.nav$status) == global$SKIPPED * rv.nav$length)
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
 observeEvent(rv.nav$current.pos, ignoreInit = F,{
    if (verbose) cat(paste0('::observe(rv$current.pos) from - ', id, '\n\n'))
   
   shinyjs::toggleState(id = "prevBtn", condition = rv.nav$current.pos > 1)
   shinyjs::toggleState(id = "nextBtn", condition = rv.nav$current.pos < length(config$steps))
   shinyjs::hide(selector = paste0(".page_", id))
   shinyjs::show(config$steps[rv.nav$current.pos])
   
   #ActionOn_NewPosition()
   
 })
 
 #' @description
 #' Change current position.
 #' 
 #' @param direction xxx
 #'
 NavPage = function(direction) {
   newval <- rv.nav$current.pos + direction 
   newval <- max(1, newval)
   newval <- min(newval, length(config$steps))
   rv.nav$current.pos <- newval
 }
 
 observeEvent(input$prevBtn, ignoreInit = TRUE, {NavPage(-1)})
 observeEvent(input$nextBtn, ignoreInit = TRUE, {NavPage(1)})
 
 
 

 
 # Catch new status event
 
 observeEvent(rv.nav$status, ignoreInit = T, {
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
   rv.nav$local.reset <- input$rstBtn
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
   
   current_step_skipped <- rv.nav$status[rv.nav$current.pos] == global$SKIPPED
   entire_process_skipped <- isTRUE(sum(rv.nav$status) == global$SKIPPED * length(config$steps))
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

         
  }

  )
}
    
## To be copied in the UI
# mod_process_ui("process_ui_1")
    
## To be copied in the server
# callModule(mod_process_server, "process_ui_1")
 
