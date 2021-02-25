btn_style <- "display:inline-block; vertical-align: middle; padding: 7px"

source(file.path('.', 'mod_timeline_h.R'), local=TRUE)$value

verbose <- F
redBtnClass <- "btn-danger"
PrevNextBtnClass <- "btn-info"
btn_success_color <- "btn-success"
optionsBtnClass <- "info"

btn_style <- "display:inline-block; vertical-align: middle; padding: 7px"

mod_nav_process_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    # div(style = "vertical-align: middle; padding: 10px; display: flex;",
    #     div(style = "vertical-align: middle; ",
    #         shinyjs::disabled(
    #           actionButton(ns("prevBtn"), "<<",
    #                        class = PrevNextBtnClass,
    #                        style='padding:4px; font-size:80%')
    #         )),
    #     div(style = "vertical-align: middle; ",
    #         actionButton(ns("rstBtn"), "Reset",
    #                      class = redBtnClass,
    #                      style='padding:4px; font-size:80%')
    #     ),
    #     div(style = "vertical-align: middle; ",
    #         mod_timeline_h_ui(ns('timeline'))
    #     ),
    #     div(style = "vertical-align: middle; ",
    #         actionButton(ns("nextBtn"),">>",
    #                      class = PrevNextBtnClass,
    #                      style='padding:4px; font-size:80%')
    # 
    # )),
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



mod_nav_process_server <- function(id,
                               config = NULL,
                               status = reactive({NULL}),
                               rv.widgets = reactive({NULL}),
                               dataIn = reactive({NULL}),
                               is.enabled = reactive({TRUE}),
                               reset = reactive({FALSE})
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
    
    
    
    #' @field modal_txt xxx
    modal_txt <- "This action will reset this process. The input dataset will be the output of the last previous
                      validated process and all further datasets will be removed"
    
    observeEvent(status(), {
      rv.process$status <- status() 
      })
    
    observeEvent(id, {
      
      rv.process$config <- config()
      rv.process$length <- length(rv.process$config$steps)
      rv.process$current.pos  <- 1
      
      rv.process$parent <- unlist(strsplit(id, split='_'))[1]
      
      check <- CheckConfig(rv.process$config)
      if (!check$passed)
        stop(paste0("Errors in 'rv.process$config'", paste0(check$msg, collapse=' ')))
      rv.process$config$mandatory <- setNames(rv.process$config$mandatory, rv.process$config$steps)
      rv.process$status = setNames(rep(global$UNDONE, rv.process$length), rv.process$config$steps)
      rv.process$currentStepName <- reactive({rv.process$config$steps[rv.process$current.pos]})
      rv.process$steps.enabled <- setNames(rep(FALSE, rv.process$length), rv.process$config$steps)
      
      mod_timeline_h_server(id = 'timeline',
                            config =  rv.process$config,
                            status = reactive({rv.process$status}),
                            position = reactive({rv.process$current.pos}),
                            enabled = reactive({rv.process$steps.enabled})
      )
    }, priority=1000) 
    

    
    #
    # Catch a new dataset sent by the caller
    #
    observeEvent(dataIn(), ignoreNULL = F, ignoreInit = F,{
      if (verbose) cat(paste0('::observeEvent(dataIn()) from --- ', id, '\n\n'))
      #browser()
      
      Change_Current_Pos(1)
      rv.process$dataIn <- dataIn()
      #ActionOn_New_DataIn() # Used by class pipeline
      
      if(is.null(dataIn())){
        print('Process : dataIn() NULL')
        ToggleState_Screens(FALSE, 1:rv.process$length)
        ToggleState_Screens(TRUE, 1)
        rv.process$original.length <- 0
      } else { # A new dataset has been loaded
        print('Process : dataIn() not NULL')
        rv.process$original.length <- length(dataIn())
        #browser()
        Update_State_Screens()
        ToggleState_Screens(TRUE, 1)
      }
    })
    

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
    #' BasicReset = function(){
    #'   if(verbose) cat(paste0('BasicReset() from - ', id, '\n\n'))
    #'   #ResetScreens()
    #'   rv.process$dataIn <- NULL
    #'   rv.process$current.pos <- 1
    #'   rv.process$status <- setNames(rep(global$UNDONE, rv.process$length), rv.process$config$steps)
    #'   Send_Result_to_Caller()
    #' }
    
    
    
    
    
    ##
    ## Common functions
    ##
    
    
    
    
    
    
    #-------------------------------------------------------
    observeEvent(rv.process$current.pos, ignoreInit = F,{
      if (verbose) cat(paste0('::observe(rv$current.pos) from - ', id, '\n\n'))
      
      shinyjs::toggleState(id = "prevBtn", condition = rv.process$current.pos > 1)
      shinyjs::toggleState(id = "nextBtn", condition = rv.process$current.pos < rv.process$length)
      shinyjs::hide(selector = paste0(".page_", id))
      shinyjs::show(rv.process$config$steps[rv.process$current.pos])
      
      #ActionOn_Newrv.process$position
      
    })
    
    
    
    observeEvent(req(reset()), ignoreInit=F, ignoreNULL=T, {
      #browser()
      if (verbose) cat(paste0('::observeEvent(req(c(input$modal_ok))) from - ', id, '\n\n'))
      BasicReset()
    })
    
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
                 tags$b(h4(style = 'color: blue;', paste0("Global input of ", rv.process$config$type))),
                 uiOutput(ns('show_dataIn'))),
          column(width=2,
                 tags$b(h4(style = 'color: blue;', paste0("Temp input of ", rv.process$config$type))),
                 uiOutput(ns('show_rv_dataIn'))),
          column(width=2,
                 tags$b(h4(style = 'color: blue;', paste0("Output of ", rv.process$config$type))),
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
    
    
    list(dataOut = reactive({dataOut}),
         steps.enabled = reactive({rv.process$steps.enabled}),
         status = reactive({rv.process$status}),
         reset = reactive({rv.process$local.reset})
    )
    
    
  }
  )
}