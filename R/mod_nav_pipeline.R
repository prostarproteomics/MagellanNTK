
#' @title xxx
#'
#' @description
#' Removes one or more items from the dataset. This function is specific of the
#' type of dataset.
#' 
#' 
#' @param dataset xxx
#' 
#' @param range xxx
#'
#' @return
#' The dataset minus some items
#'
#' @export
#'
#'@return xxx
#'
#' @examples 
#' utils::data(Exp1_R25_pept, package='DAPARdata2')
#' obj <- Keep_Items_from_Dataset(Exp1_R25_pept, range = seq_len(2))
#' 
Keep_Items_from_Dataset <- function(dataset, range){
  dataset[ , , range]
}


#' @title xxx
#' 
#' @description 
#' xxxxxx
#' 
#' @noRd
#'
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
                   uiOutput(ns('EncapsulateScreens_ui'))
                   
               ),
               wellPanel(title = 'foo',
                         uiOutput(ns('show_Debug_Infos'))
               )
             ))
      
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
#' library(shiny)
#' library(shinyBS)
#' library(crayon)
#' library(MSPipelines)
#' ui <- fluidPage(
#'   mod_nav_pipeline_ui('Protein')
#' )
#' server <- function(input, output){
#'   utils::data(Exp1_R25_prot, package='DAPARdata2')
#' mod_nav_pipeline_server(id = 'Protein',
#'                           dataIn = reactive({Exp1_R25_prot})
#'   )
#' }
#' shinyApp(ui, server)
#' 
mod_nav_pipeline_server <- function(id,
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
    
    nav.mode <- "pipeline"
    
    source(system.file("extdata", 'commonFuncs.R', package="Magellan"), local=TRUE)$value
    
    
    
    #
    #
    ##############################################################
    
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
    rv.process <- reactiveValues(
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
    

    # Catch any event on the 'id' parameter. As this parameter change only
    # when the server is created, this function can be view as the initialization
    # of the server
    observeEvent(id, {
      # The package containing the code for processes is supposed to be
      # already launched. Just check if the server module is ok
      # if (!exists('mod_Protein_server', where='package:DaparToolshed', mode='function')){
      #   warning("This pipeline is not available in DaparToolshed")
      #   return(NULL)
      # }
      
      source(file.path("example_modules", 'mod_PipelineA.R'), local=TRUE)$value
      
      
      # Call the server module of the pipeline which name is the parameter 'id'
      # This will give access to its config
      rv.process$proc <- do.call(paste0('mod_', id, '_server'),
                                 list(id = id,
                                      dataIn = reactive({rv.process$temp.dataIn}),
                                      steps.enabled = reactive({rv.process$steps.enabled}),
                                      remoteReset = reactive({FALSE}),
                                      steps.status = reactive({rv.process$steps.status})
                                      )
                                 )
      
      
      
      
     
      # Update the reactive value config with the config of the pipeline
      rv.process$config <- rv.process$proc$config()
      
      # TODO Write the CheckPipelineConfig function
      # Check if the config variable is correct
      # check <- CheckPipelineConfig(rv.process$config)
      # if (!check$passed)
      #   stop(paste0("Errors in 'rv.process$config'", paste0(check$msg, collapse=' ')))
      # 
      
      
      rv.process$length <- length(rv.process$config$steps)
      rv.process$current.pos <- 1
      
      # Get the name of the parent of the process
      # The id variable is composed of two ids separate by '_'. The first id correspond to the parent
      # and the second correspond to the child in the process hierarchy
      rv.process$parent.name <- unlist(strsplit(id, split='_'))[1]
      rv.process$child.name <- unlist(strsplit(id, split='_'))[2]
      
      
      rv.process$config$mandatory <- setNames(rv.process$config$mandatory, rv.process$config$steps)
      rv.process$steps.status = setNames(rep(global$UNDONE, rv.process$length), rv.process$config$steps)
      rv.process$currentStepName <- reactive({rv.process$config$steps[rv.process$current.pos]})
      
      rv.process$steps.enabled <- setNames(rep(FALSE, length(rv.process$config$steps)), rv.process$config$steps)
      rv.process$steps.skipped <- setNames(rep(FALSE, length(rv.process$config$steps)), rv.process$config$steps)
      rv.process$resetChildren <- setNames(rep(0, length(rv.process$config$steps)), rv.process$config$steps)
      
      rv.child$data2send <- setNames(lapply(as.list(rv.process$config$steps), function(x) NULL), 
                                     nm = rv.process$config$steps)
      
      # Launch the ui for each step of the pipeline
      # This function could be stored in the source file of the pipeline
      # but the strategy is to insert minimum extra code in the files for
      # pipelines and processes. This is useful when other devs will
      # develop other pipelines and processes. Tus, it will be easier.
      rv.process$config$ll.UI <- setNames(lapply(rv.process$config$steps,
                         function(x){
                            mod_nav_process_ui(ns(paste0(id, '_', x)))
                         }),
                  paste0(rv.process$config$steps)
         )
      
      
      
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
                                                  remoteReset = reactive({rv.process$resetChildren[x]}),
                                                  is.skipped = reactive({isTRUE(rv.process$steps.skipped[x])})
                                                  )
      })
      
      mod_timeline_v_server(id = 'timelinev',
                            config =  rv.process$config,
                            status = reactive({rv.process$steps.status}),
                            position = reactive({rv.process$current.pos}),
                            enabled = reactive({rv.process$steps.enabled})
      )
      
    }, priority=1000) 
    

    
    
    ################################################################
    #
    #
    ############################################################"
    
    output$EncapsulateScreens_ui <- renderUI({
      EncapsulateScreens()
    })
  
    
    output$SkippedInfoPanel <- renderUI({
      #if (verbose) cat(paste0(class(self)[1], '::output$SkippedInfoPanel from - ', self$id, "\n\n"))
      
      current_step_skipped <- rv.process$steps.status[rv.process$current.pos] == global$SKIPPED
      #entire_process_skipped <- isTRUE(sum(rv.process$steps.status) == global$SKIPPED * rv.process$length)
      req(current_step_skipped)
      
      
      #if (entire_process_skipped){
      # This case appears when the process has been skipped from the
      # pipleine. Thus, it is not necessary to show the info box because
      # it is shown below the timeline of the pipeline
      #} else {
      txt <- paste0("This ", rv.process$config$type, " is skipped so it has been disabled.")
      wellPanel(
        style = "background-color: #7CC9F0; opacity: 0.72; padding: 0px; align: center; vertical-align: center;",
        height = 100,
        width=300,
        align="center",
        p(style = "color: black;", paste0('Info: ',txt))
      )
      #}
    })
    
    
    
    # Catch any event in the status of xxx
    # observeEvent(req(rv.process$proc$status()), {
    #   #browser()
    #   # If step 1 has been validated, then initialize rv.process$dataIn
    #   if (rv.process$steps.status[1]==0 && rv.process$proc$status()[1]==1)
    #     rv.process$dataIn <- rv.process$temp.dataIn
    #   
    #   rv.process$status <- rv.process$proc$steps.status() 
    # })
    
    
    
   
    
    
    
    # # Default actions on reset pipeline or process.
    # # 
    # LocalReset = function(){
    #   if(verbose) cat(paste0('LocalReset() from - ', id, "\n\n"))
    #   rv.process$dataIn <- NULL
    #   #rv.process$temp.dataIn <- NULL
    #   rv.process$current.pos <- 1
    #   
    #   rv.process$steps.status <- setNames(rep(global$UNDONE, rv.process$length), 
    #                                 nm = rv.process$config$steps)
    #   
    #   ResetChildren()
    #   
    #   Send_Result_to_Caller()
    # }
    
    
    CurrentStepName <- reactive({
      cat(yellow(paste0('::GetCurrentStepName() from - ', id, '\n')))
      rv.process$config$steps[rv.process$current.pos]
    })
    
    
    
    # Catch the returned values of the process                                                           
    observeEvent(lapply(rv.process$config$steps, 
                        function(x){
                          tmp.return[[x]]$dataOut()$trigger}), ignoreInit=TRUE, {
                            if(verbose) cat(paste0('observeEvent(trigger) from - ', id, "\n\n"))
                            #browser()
                            ActionOn_Data_Trigger()
                          })
    
    
    
    
    #' @description 
    #' xxx
    #' 
    # Update_State_Screens = function(){
    #   if(verbose) cat(yellow(paste0('::', 'Update_State_Screens() from - ', id, "\n\n")))
    #   #browser()
    #   ind.max <- GetMaxValidated_AllSteps()
    #   #browser()
    #   
    #   # All steps before the last validated step (itself included) are disabled
    #   # To modify one of these steps, one need to reset it
    #   if (ind.max > 0) 
    #     ToggleState_Screens(cond = FALSE, range = seq_len(ind.max))
    #   
    #   
    #   if (ind.max < rv.process$length){
    #     # Enable all steps after the current one but the ones
    #     # after the first mandatory not validated
    #     firstM <- GetFirstMandatoryNotValidated((ind.max+1):rv.process$length)
    #     if (is.null(firstM)){
    #       ToggleState_Screens(cond = TRUE, range = (1 + ind.max):(rv.process$length))
    #     } else {
    #       ToggleState_Screens(cond = TRUE, range = (1 + ind.max):(ind.max + firstM))
    #       if (ind.max + firstM < rv.process$length)
    #         ToggleState_Screens(cond = FALSE, range = (ind.max + firstM + 1):rv.process$length)
    #     }
    #   }
    #   # browser()
    # }
    
 
    # ToggleState_Screens = function(cond, range){
    #   if(verbose) cat(paste0('::ToggleState_Steps() from - ', id, "\n\n"))
    #   #browser()
    #   if (isTRUE(is.enabled()))
    #     lapply(range, function(x){
    #       cond <- cond && !(rv.process$steps.status[x] == global$SKIPPED)
    #        rv.process$steps.enabled[x] <- cond
    #     })
    # }
    
    
    #
    # Catch a new dataset sent by the caller
    #
    #observeEvent(dataIn(), ignoreNULL = FALSE, ignoreInit = FALSE,{
    # observe({
    #   if (verbose) cat(paste0(id, '::observe(dataIn())\n\n'))
    #   #browser()
    #   isolate({
    #   Change_Current_Pos(1)
    #   rv.process$temp.dataIn <- dataIn()
    #   
    #   if (is.null(rv.process$dataIn))
    #       PrepareData2Send() # Used by class pipeline
    #   
    #   if(is.null(dataIn())){
    #     print('Process : dataIn() NULL')
    #     ToggleState_Screens(FALSE, seq_len(rv.process$length))
    #     ToggleState_Screens(TRUE, 1)
    #     rv.process$original.length <- 0
    #   } else { # A new dataset has been loaded
    #     print('Process : dataIn() not NULL')
    #     rv.process$original.length <- length(dataIn())
    #     #browser()
    #     Update_State_Screens()
    #     ToggleState_Screens(TRUE, 1)
    #   }
    # })
    # })
    

    # @description
    # This function calls the server part of each module composing the pipeline
    #
    # @return Nothing
    #
    PrepareData2Send = function(){
      if(verbose) cat(paste0(id, '::PrepareData2Send()\n\n'))
       #browser()
      # Returns NULL to all modules except the one pointed by the current position
      # Initialization of the pipeline : one send dataIn() to the
      # first module

      # The dataset to send is contained in the variable 'rv.process$dataIn'

      #' @param name The name of the process to update
      Update_Data2send_Vector <- function(){
        # One only update the current position because the vector has been entirely
        # initialized to NULL so the other processes are already ready to be sent
       ind.last.validated <- GetMaxValidated_BeforePos()
       if (is.null(ind.last.validated))
            data <- rv.process$temp.dataIn
       else
            data <- Keep_Items_from_Dataset(dataset = rv.process$dataIn,
                                            range = seq_len(ind.last.validated + rv.process$original.length -1)
                                            )
      return(data)
      }

      # Initialize vector to all NULL values
      rv.child$data2send <- setNames(
        lapply(rv.process$config$steps, function(x){NULL}),
        rv.process$config$steps)

      if (is.null(rv.process$dataIn)){ # Init of core engine

        # Only the first process will receive the data
        rv.child$data2send[[1]] <- rv.process$temp.dataIn

        # The other processes are by default disabled.
        # If they have to be enabled, they will be by another function later
        lapply(seq_len(rv.process$length), function(x){
          rv.process$steps.enabled[x] <- x==1
        })

      } else
        rv.child$data2send[[CurrentStepName()]] <- Update_Data2send_Vector()

      cat(blue("<----------------- Data sent to children ------------------> \n"))
      print(rv.child$data2send)
      cat(blue("<----------------------------------------------------> \n"))
    }
    
    # @description
    # Catch the return value of a module and update the list of isDone modules
    # This list is updated with the names of datasets present in the rv$tmp
    # variable. One set to TRUE all the elements in isDone which have a corresponding
    # element in names(rv$tmp).
    # One cannot simply set to TRUE the last element of rv$tmp because it will does
    # not work in case of a reseted module (it is not in the names(rv$tmp) list
    # anymore)
    # If a value (not NULL) is received, then it corresponds to the module
    # pointed by the current position
    # This function also updates the list isDone
    #This function updates the current dataset (self$rv$dataIn)
    #
    # @return Nothing
    #
    ActionOn_Data_Trigger <- function(){
      if(verbose) cat(yellow(paste0(id, '::ActionOn_Data_Trigger()\n\n')))
      #browser()
      processHasChanged <- newValue <- NULL
      return.trigger.values <- setNames(lapply(rv.process$config$steps, function(x){tmp.return[[x]]$dataOut()$trigger}),
                                        rv.process$config$steps)
      
      # Replace NULL values by NA
      return.trigger.values[vapply(return.trigger.values, is.null)] <- NA
      triggerValues <- unlist(return.trigger.values)
      
      
      return.values <- setNames(lapply(rv.process$config$steps, function(x){tmp.return[[x]]$dataOut()$value}),
                                rv.process$config$steps)
       
      cat(blue('--------------- Data received from children --------------------\n'))
      print(return.values)
      cat(blue('-------------------------------------------------------\n'))
      #browser()
      # if (sum(triggerValues)==0){ # Init of core engine
      #   rv.process$dataIn <- rv.process$temp.dataIn
      # } else
      if (is.null(unlist(return.values))) { # The entire pipeline has been reseted
        print('The entire pipeline has been reseted')
          rv.process$dataIn <- NULL
          rv.process$steps.status[seq_len(rv.process$length)] <- global$UNDONE
      } else {
        processHasChanged <- rv.process$config$steps[which(max(triggerValues, na.rm = TRUE)==triggerValues)]
        ind.processHasChanged <- which(rv.process$config$steps==processHasChanged)
        newValue <- tmp.return[[processHasChanged]]$dataOut()$value
        
        if (is.null(newValue)){
          #browser()
          # A process has been reseted
          rv.process$steps.status[ind.processHasChanged:rv.process$length] <- global$UNDONE
          rv.process$steps.enabled[ind.processHasChanged:rv.process$length] <- FALSE
          rv.process$steps.enabled[ind.processHasChanged] <- TRUE
          rv.process$steps.skipped[ind.processHasChanged:rv.process$length] <- FALSE
          
          #browser()
          # Reset all further steps also
          #ResetChildren(range = ind.processHasChanged:rv.process$length)
          #if (ind.processHasChanged < rv.process$length)
          #  rv.process$resetChildren[(1+ind.processHasChanged):rv.process$length] <- TRUE
          #rv.process$resetChildren[seq_len(ind.processHasChanged-1)] <- FALSE

          # Reset all further steps also
          #rv.child$reset[ind.processHasChanged:length(rv.process$config$steps)] <- TRUE
          #ResetChildren(range = ind.processHasChanged:rv.process$length)
          #rv.child$reset[seq_len(ind.processHasChanged-1)] <- FALSE
          
          # browser()
          # One take the last validated step (before the one corresponding to processHasChanges
          # but it is straightforward because we just updates self$rv$status
          ind.last.validated <- NULL
          validated.steps <- which(rv.process$steps.status == global$VALIDATED)
          if (length(validated.steps) !=0)
            ind.last.validated <- max(validated.steps)
          
          #There is no validated step (the first step has been reseted)
          if(is.null(ind.last.validated) || ind.last.validated == 1)
            rv.process$dataIn <- rv.process$temp.dataIn
          else{
            name.last.validated <- rv.process$config$steps[ind.last.validated]
            dataIn.ind.last.validated <- which(names(rv.process$dataIn) == name.last.validated)
            #self$rv$dataIn <- self$rv$dataIn[ , , seq_len(dataIn.ind.last.validated)]
            rv.process$dataIn <- Keep_Items_from_Dataset(dataset = rv.process$dataIn, 
                                                         range = seq_len(dataIn.ind.last.validated))
            
          }
          #Update_State_Screens()
          # In this case, one force the update of the input dataset
          #PrepareData2Send()
        } else {
          # browser()
          # A process has been validated
          rv.process$steps.status[processHasChanged] <- global$VALIDATED
          if (ind.processHasChanged < rv.process$length)
            rv.process$steps.status[(1 + ind.processHasChanged):rv.process$length] <- global$UNDONE
          
          Discover_Skipped_Steps()
          rv.process$dataIn <- newValue
        }
        
      }
      
     # PrepareData2Send()
      Send_Result_to_Caller()
    }
    
    
    # @description
    # et to skipped all steps of the current object
    # 
    # @return Nothing.
    # 
    ResetChildren = function(range){
      if(verbose) cat(paste0(id, '::ResetChildren()\n\n'))
      #browser()
      # lapply(rv.process$config$steps, function(x){
      #   rv.process$resetChildren[x] <- 1 + rv.process$resetChildren[x]
      # })
      rv.process$resetChildren[range] <- 1 + rv.process$resetChildren[range]
      

    }
    

    
    # @description
    # xxx
    #
    # @return Nothing
    #
    ActionOn_NewPosition = function(){
      if(verbose) cat(paste0(id, '::ActionOn_NewPosition()\n\n'))
      
      print("--- action on New position ---")
      # Send dataset to child process only if the current position is enabled
      #if(rv.process$steps.enabled[rv.process$current.pos])
        PrepareData2Send()
      #browser()
      # If the current step is validated, set the child current position to the last step
      if (rv.process$steps.status[rv.process$current.pos] == global$VALIDATED)
        rv.child$position[rv.process$current.pos] <- paste0('last_', Timestamp())
    }
    
    
   
    
    #-------------------------------------------------------
    observeEvent(rv.process$current.pos, ignoreInit = TRUE, {
      if (verbose) cat(paste0(id, '::observeEvent(rv$current.pos)\n\n'))
      
      shinyjs::toggleState(id = "prevBtn", condition = rv.process$current.pos > 1)
      shinyjs::toggleState(id = "nextBtn", condition = rv.process$current.pos < rv.process$length)
      shinyjs::hide(selector = paste0(".page_", id))
      shinyjs::show(rv.process$config$steps[rv.process$current.pos])
      
      #Specific to pipeline code
      ActionOn_NewPosition()
      
    })

    output$SkippedInfoPanel <- renderUI({
      #if (verbose) cat(paste0(class(self)[1], '::output$SkippedInfoPanel from - ', self$id, "\n\n"))
      
      current_step_skipped <- rv.process$steps.status[rv.process$current.pos] == global$SKIPPED
      entire_process_skipped <- isTRUE(sum(rv.process$steps.status) == global$SKIPPED * rv.process$length)
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
        h3(paste0('module pipeline "', id, '"')),
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
     # if (verbose) cat(paste0('::output$show_dataIn from - ', id, "\n\n"))
      req(dataIn())
      tagList(
        # h4('show dataIn()'),
        lapply(names(dataIn()), function(x){tags$p(x)})
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
    
    output$show_rv_dataOut <- renderUI({
     # if (verbose) cat(paste0('::output$show_rv_dataOut from - ', id, "\n\n"))
      tagList(
        #h4('show dataOut$value'),
        lapply(names(dataOut$value), function(x){tags$p(x)})
      )
    })
    
    
    output$show_status <- renderUI({
      tagList(
        lapply(seq_len(rv.process$length), 
                     function(x){
                       color <- if(rv.process$steps.enabled[x]) 'black' else 'lightgrey'
                       if (x == rv.process$current.pos)
                         tags$p(style = paste0('color: ', color, ';'),
                                tags$b(paste0('---> ', rv.process$config$steps[x], ' - ', GetStringStatus(rv.process$steps.status[[x]])), ' <---'))
                       else 
                         tags$p(style = paste0('color: ', color, ';'),
                                paste0(rv.process$config$steps[x], ' - ', GetStringStatus(rv.process$steps.status[[x]])))
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
         status = reactive({rv.process$steps.status}))
    
  }
  )
}