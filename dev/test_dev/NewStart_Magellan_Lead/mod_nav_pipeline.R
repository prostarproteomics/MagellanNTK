btn_style <- "display:inline-block; vertical-align: middle; padding: 7px"

source(file.path('.', 'mod_timeline_v.R'), local=TRUE)$value
source(file.path('.', 'mod_nav_process.R'), local=FALSE)$value
source(file.path('.', 'mod_Protein.R'), local=FALSE)$value


verbose <- F
redBtnClass <- "btn-danger"
PrevNextBtnClass <- "btn-info"
btn_success_color <- "btn-success"
optionsBtnClass <- "info"

btn_style <- "display:inline-block; vertical-align: middle; padding: 7px"



#' @title xxx
#'
#' @description
#' Adds one or more items to the dataset. This function is specific of the
#' type of dataset.
#'
#' @importFrom QFeatures addAssay
#'
#' @return
#' The dataset minus some items
#'
#' @export
#'
Add_Item_to_Dataset <- function(dataset, name){
  QFeatures::addAssay(dataset,
                      dataset[[length(dataset)]],
                      name=name)
}

#' @title xxx
#'
#' @description
#' Removes one or more items from the dataset. This function is specific of the
#' type of dataset.
#'
#' @return
#' The dataset minus some items
#'
#' @export
#'
Keep_Items_from_Dataset <- function(dataset, range){
  dataset[ , , range]
}




mod_nav_pipeline_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    fluidRow(
      column(width=2, 
             wellPanel(
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
                       actionButton(ns("nextBtn"),">>",
                                    class = PrevNextBtnClass,
                                    style='padding:4px; font-size:80%')
                   ),
                   mod_timeline_v_ui(ns('timelinev'))
               )
             )),
      column(width=10,
             style=" padding-left: 20px;",
             wellPanel(
               div(id = ns('Screens'),
                   uiOutput(ns('SkippedInfoPanel')),
                   uiOutput(ns('EncapsulateScreens'))
                   
               ),
               wellPanel(title = 'foo',
                         tagList(
                           h3('module pipeline'),
                           uiOutput(ns('show_Debug_Infos'))
                         )
               )
             ))
      
    )
  )
}


#' @export
#' 
mod_nav_pipeline_server <- function(id,
                                   #config = NULL,
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
    
    source(file.path('.', 'commonFuncs.R'), local=TRUE)$value

    verbose <- F
    # Specific to pipeline module
    tmp.return <- reactiveValues()
    
    rv.child <- reactiveValues(
      enabled = NULL,
      reset = NULL,
      position = NULL
    )
    
    rv.process <- reactiveValues(
      proc = NULL,
      status = NULL,
      dataIn = NULL,
      temp.dataIn = NULL,
      steps.enabled = NULL,
      steps.skipped = NULL,
      reset = NULL
    )
    
    #' @field modal_txt xxx
    modal_txt <- "This action will reset this pipeline. The input dataset will be the output of the last previous
                      validated process and all further datasets will be removed"
    
    observeEvent(rv.process$proc$status(), {
      #browser()
      # If step 1 has been validated, then initialize dataIn
      if (rv.process$status[1]==0 && rv.process$proc$status()[1]==1)
        rv.process$dataIn <- rv.process$temp.dataIn
      
      rv.process$status <- rv.process$proc$status() 
    })
    
    # 
    # BuildScreens <- reactive({
    #   #browser()
    #   #for (x in rv.process$config$steps)
    #   #  source(file.path('.', paste0('mod_', paste0(id, '_', x), '.R')), local=TRUE)
    # 
    # 
    #   setNames(lapply(rv.process$config$steps,
    #                   function(x){
    #                     source(file.path('.', paste0('mod_', paste0(id, '_', x), '.R')), local=TRUE)
    #                     #mod_nav_process_ui(ns(paste0(id, '_', x)))
    #                     h3(paste0(id, '_', x))
    #                   }),
    #            paste0('screen_', rv.process$config$steps)
    #   )
    # })
    # 
    
    
    observeEvent(id, {
      
      # Load code for pipeline if it is in a subdirectory of R directory
      #for (x in rv.process$config$steps)
      #  source(file.path('.', paste0('mod_', paste0(id, '_', x), '.R')), local=TRUE)
      rv.process$proc <- do.call(paste0('mod_', id, '_server'),
                                 list(id = id,
                                      dataIn = reactive({rv.process$temp.dataIn}),
                                      steps.enabled = reactive({rv.process$steps.enabled}),
                                      remoteReset = reactive({FALSE}),
                                      status = reactive({rv.process$status})
                                 )
      )
      
      
      
      
      
      rv.process$config <- rv.process$proc$config()
      rv.process$config$ll.UI <- setNames(lapply(rv.process$config$steps,
                         function(x){
                           source(file.path('.', paste0('mod_', paste0(id, '_', x), '.R')), local=TRUE)
                           mod_nav_process_ui(ns(paste0(id, '_', x)))
                         }),
                  paste0('screen_', rv.process$config$steps)
         )
      rv.process$length <- length(rv.process$config$steps)
      rv.process$current.pos  <- 1
      
      rv.process$parent <- unlist(strsplit(id, split='_'))[1]
      
      check <- CheckConfig(rv.process$config)
      if (!check$passed)
        stop(paste0("Errors in 'rv.process$config'", paste0(check$msg, collapse=' ')))
      rv.process$config$mandatory <- setNames(rv.process$config$mandatory, rv.process$config$steps)
      rv.process$status = setNames(rep(global$UNDONE, rv.process$length), rv.process$config$steps)
      rv.process$currentStepName <- reactive({rv.process$config$steps[rv.process$current.pos]})
      
      rv.process$steps.enabled <- setNames(rep(FALSE, length(rv.process$config$steps)), rv.process$config$steps)
      rv.process$steps.skipped <- setNames(rep(FALSE, length(rv.process$config$steps)), rv.process$config$steps)
      rv.process$reset <- setNames(rep(0, length(rv.process$config$steps)), rv.process$config$steps)
      
      
      #browser()
      # rv.process$config$ll.UI <- setNames(lapply(rv.process$config$steps,
      #                                        function(x){
      #                                          source(file.path('.', paste0('mod_', paste0(id, '_', x), '.R')), local=TRUE)
      #                                          mod_nav_process_ui(ns(paste0(id, '_', x)))
      #                                          }),
      #                                 paste0('screen_', rv.process$config$steps)
      # )

      lapply(rv.process$config$steps, function(x){
        tmp.return[[x]] <- mod_nav_process_server(id = paste0(id, '_', x) ,
                                                  dataIn = reactive({ rv.child$data2send[[x]] }),
                                                  is.enabled = reactive({isTRUE(rv.process$steps.enabled[x])}),
                                                  remoteReset = reactive({rv.process$reset[x]}),
                                                  is.skipped = reactive({isTRUE(rv.process$steps.skipped[x])})
                                                  )
      })
      
      
      #browser()
      mod_timeline_v_server(id = 'timelinev',
                            config =  rv.process$config,
                            status = reactive({rv.process$status}),
                            position = reactive({rv.process$current.pos}),
                            enabled = reactive({rv.process$steps.enabled})
      )
      
    }, priority=1000) 
    

    
    
    output$EncapsulateScreens <- renderUI({
     # browser()
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
      
      ResetChildren()
      
      Send_Result_to_Caller()
    }
    
    
    CurrentStepName <- reactive({
      cat(paste0('::GetCurrentStepName() from - ', id, '\n'))
      rv.process$config$steps[rv.process$current.pos]
    })
    
    
    
    # Catch the returned values of the process                                                           
    observeEvent(lapply(rv.process$config$steps, 
                        function(x){
                          tmp.return[[x]]$dataOut()$trigger}), ignoreInit=T,{
                            if(verbose) cat(paste0('observeEvent(trigger) from - ', id, '\n\n'))
                            #browser()
                            ActionOn_Data_Trigger()
                          })
    
    
    
    
    #' @description 
    #' xxx
    #' 
    Update_State_Screens = function(){
      if(verbose) cat(paste0('::', 'Update_State_Screens() from - ', id, '\n\n'))
      
      ind.max <- GetMaxValidated_AllSteps()
      #browser()
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
      # browser()
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
      if (is.enabled())
        lapply(range, function(x){
          cond <- cond && !(rv.process$status[x] == global$SKIPPED)
           rv.process$steps.enabled[x] <- cond
        })
    }
    
    
    #
    # Catch a new dataset sent by the caller
    #
    observeEvent(dataIn(), ignoreNULL = F, ignoreInit = F,{
      if (verbose) cat(paste0('::observeEvent(dataIn()) from --- ', id, '\n\n'))
      #browser()
      
      Change_Current_Pos(1)
      rv.process$temp.dataIn <- dataIn()
      if (is.null(rv.process$dataIn))
          PrepareData2Send() # Used by class pipeline
      
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
    
    #' @description
    #' This function calls the server part of each module composing the pipeline
    #'
    #' @return Nothing
    #'
    PrepareData2Send = function(){
      if(verbose) cat(paste0('PrepareData2Send() from - ', id, '\n\n'))
      # browser()
      # Returns NULL to all modules except the one pointed by the current position
      # Initialization of the pipeline : one send dataIn() to the
      # first module
      #browser()
      
      update <- function(name){
        data <- NULL
        if (name == CurrentStepName()){
          # One treat the dataset for the current position
          #ind.last.validated <- self$GetMaxValidated_BeforeCurrentPos()
          #name.last.validated <- names(rv.process$dataIn)[length(rv.process$dataIn)]
          #ind.last.validated <- which(names(rv.process$dataIn)== name.last.validated)
          ind.last.validated <- GetMaxValidated_BeforeCurrentPos()
          
          if (is.null(ind.last.validated)){
            data <- rv.process$temp.dataIn
          } else {
            data <- Keep_Items_from_Dataset(dataset = rv.process$dataIn, 
                                            range = 1:(ind.last.validated + rv.process$original.length -1)
            )
            #data <- self$rv$dataIn[ , , 1:ind.last.validated]
          }
        }
        return(data)
      }
      
      browser() 
      rv.child$data2send <- setNames(
        lapply(rv.process$config$steps, function(x){NULL}),
        rv.process$config$steps)
      
      if (is.null(rv.process$dataIn)){ # Init of core engine
        rv.child$data2send[[1]] <- rv.process$temp.dataIn
        # Disable the steps which receive NULL data
        lapply(1:rv.process$length, function(x){
          rv.process$steps.enabled[x] <- x==1
        })
      } else
        rv.child$data2send <- setNames(
          lapply(rv.process$config$steps, function(x){update(x)}),
          rv.process$config$steps)
      
     
      
      
      cat(blue("<----------------- data2 send ------------------> \n"))
      print(rv.child$data2send)
      cat(blue("<------------------------------------------------> \n"))
    }
    
    
    #' @description
    #' Catch the return value of a module and update the list of isDone modules
    #' This list is updated with the names of datasets present in the rv$tmp
    #' variable. One set to TRUE all the elements in isDone which have a corresponding
    #' element in names(rv$tmp).
    #' One cannot simply set to TRUE the last element of rv$tmp because it will does
    #' not work in case of a reseted module (it is not in the names(rv$tmp) list
    #' anymore)
    #' If a value (not NULL) is received, then it corresponds to the module
    #' pointed by the current position
    #' This function also updates the list isDone
    #' This function updates the current dataset (self$rv$dataIn)
    #'
    #' @return Nothing
    #'
    ActionOn_Data_Trigger <- function(){
      if(verbose) cat(paste0('::', 'ActionOn_Data_Trigger from - ', id, '\n\n'))
      #browser()
      processHasChanged <- newValue <- NULL
      return.trigger.values <- setNames(lapply(rv.process$config$steps, function(x){tmp.return[[x]]$dataOut()$trigger}),
                                        rv.process$config$steps)
      return.values <- setNames(lapply(rv.process$config$steps, function(x){tmp.return[[x]]$dataOut()$value}),
                                rv.process$config$steps)
      triggerValues <- unlist(return.trigger.values)
      
      browser()
      # if (sum(triggerValues)==0){ # Init of core engine
      #   rv.process$dataIn <- rv.process$temp.dataIn
      # } else
        if (is.null(unlist(return.values))) { # The entire pipeline has been reseted
        print('The entire pipeline has been reseted')
          rv.process$dataIn <- NULL
          rv.process$status[1:rv.process$length] <- global$UNDONE
          
        #PrepareData2Send()
      } else {
        processHasChanged <- rv.process$config$steps[which(max(triggerValues)==triggerValues)]
        ind.processHasChanged <- which(rv.process$config$steps==processHasChanged)
        newValue <- tmp.return[[processHasChanged]]$dataOut()$value
        
        
        # A process has been reseted
        if (is.null(newValue)){
          rv.process$status[ind.processHasChanged:rv.process$length] <- global$UNDONE
          rv.process$steps.enabled[ind.processHasChanged:rv.process$length] <- FALSE
          rv.process$steps.skipped[ind.processHasChanged:rv.process$length] <- FALSE
          
          #browser()
          # Reset all further steps also
          if (ind.processHasChanged < rv.process$length)
            rv.process$reset[(1+ind.processHasChanged):rv.process$length] <- TRUE
          #rv.process$reset[1:(ind.processHasChanged-1)] <- FALSE

          # Reset all further steps also
          #rv.child$reset[ind.processHasChanged:length(rv.process$config$steps)] <- TRUE
          #rv.child$reset[1:(ind.processHasChanged-1)] <- FALSE
          
          # browser()
          # One take the last validated step (before the one corresponding to processHasChanges
          # but it is straightforward because we just updates self$rv$status
          ind.last.validated <- NULL
          validated.steps <- which(rv.process$status == global$VALIDATED)
          if (length(validated.steps) !=0)
            ind.last.validated <- max(validated.steps)
          
          #There is no validated step (the first step has been reseted)
          if(is.null(ind.last.validated) || ind.last.validated == 1)
            rv.process$dataIn <- rv.process$temp.dataIn
          else{
            name.last.validated <- rv.process$config$steps[ind.last.validated]
            dataIn.ind.last.validated <- which(names(rv.process$dataIn) == name.last.validated)
            #self$rv$dataIn <- self$rv$dataIn[ , , 1:dataIn.ind.last.validated]
            rv.process$dataIn <- Keep_Items_from_Dataset(dataset = rv.process$dataIn, 
                                                         range = 1:dataIn.ind.last.validated)
            
          }
          Update_State_Screens()
          # In this case, one force the update of the input dataset
          #PrepareData2Send()
        } else {
          # browser()
          # A process has been validated
          rv.process$status[processHasChanged] <- global$VALIDATED
          if (ind.processHasChanged < rv.process$length)
            rv.process$status[(1 + ind.processHasChanged):rv.process$length] <- global$UNDONE
          
          Discover_Skipped_Steps()
          rv.process$dataIn <- newValue
        }
        
      }
      #PrepareData2Send()
       
      Send_Result_to_Caller()
    }
    
    
    #' @description
    #' et to skipped all steps of the current object
    #' 
    #' @return Nothing.
    #' 
    ResetChildren = function(){
      if(verbose) cat(paste0('::', 'Set_All_Reset() from - ', id, '\n\n'))
      #browser()
      lapply(rv.process$config$steps, function(x){
        rv.process$reset[x] <- 1 + rv.process$reset[x]
      })

    }
    

    
    #' @description
    #' xxx
    #'
    #' @return Nothing
    #'
    ActionOn_NewPosition = function(){
      if(verbose) cat(paste0('::ActionOn_NewPosition() from - ', id, '\n\n'))
      
      print("--- action on New position ---")
      # Send dataset to child process only if the current position is enabled
      #if(rv.process$steps.enabled[rv.process$current.pos])
        PrepareData2Send()
      #browser()
      # If the current step is validated, set the child current position to the last step
      if (rv.process$status[rv.process$current.pos] == global$VALIDATED)
        rv.child$position[rv.process$current.pos] <- paste0('last_', Timestamp())
    }
    
    
   
    
    #-------------------------------------------------------
    observeEvent(rv.process$current.pos, ignoreInit = T,{
      if (verbose) cat(paste0('::observe(rv$current.pos) from - ', id, '\n\n'))
      
      shinyjs::toggleState(id = "prevBtn", condition = rv.process$current.pos > 1)
      shinyjs::toggleState(id = "nextBtn", condition = rv.process$current.pos < rv.process$length)
      shinyjs::hide(selector = paste0(".page_", id))
      shinyjs::show(rv.process$config$steps[rv.process$current.pos])
      
      #Specific to pipeline code
      ActionOn_NewPosition()
      
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
      tagList(
        lapply(1:rv.process$length, 
                     function(x){
                       color <- if(rv.process$steps.enabled[x]) 'black' else 'lightgrey'
                       if (x == rv.process$current.pos)
                         tags$p(style = paste0('color: ', color, ';'),
                                tags$b(paste0('---> ', rv.process$config$steps[x], ' - ', GetStringStatus(rv.process$status[[x]])), ' <---'))
                       else 
                         tags$p(style = paste0('color: ', color, ';'),
                                paste0(rv.process$config$steps[x], ' - ', GetStringStatus(rv.process$status[[x]])))
                     })
        )
    })
    
    output$show_tag_enabled <- renderUI({
      tagList(
        p(paste0('steps.enabled = ', paste0(as.numeric(rv.process$steps.enabled), collapse=' '))),
        p(paste0('enabled() = ', as.numeric(is.enabled())))
      )
    })
    
    
    list(dataOut = reactive({dataOut}),
         steps.enabled = reactive({rv.process$steps.enabled}),
         status = reactive({rv.process$status}))
    
  }
  )
}