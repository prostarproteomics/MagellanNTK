#' @title The server() function of the module `mod_nav`
#'
#' @description The module navigation can be launched via a Shiny app. 
#' This is the core module of Magellan
#'
#' 
#' @name mod_nav
#' 
#' 
#' @author Samuel Wieczorek
#' 
#' 
#' @example inst/examples/example_mod_single_Process.R
#' @example inst/examples/example_mod_Pipeline.R
#'
NULL





#' @param id A `character(1)` which defines the id of the module. 
#' It is the same as for the server() function.
#'
#' @rdname mod_nav
#'
#' @export
#'
mod_nav_ui <- function(id) {
    ns <- NS(id)
    tagList(
        shinyjs::useShinyjs(),
        uiOutput(ns("nav_mod_ui")),
        uiOutput(ns("debug_infos_ui"))
    )
}









#' @param id A `character(1)` which defines the id of the module. It is the same
#' as for the ui() function.
#'
#' @param dataIn The dataset
#'
#' @param is.enabled A `boolean`. This variable is a remote command to specify
#' if the corresponding module is enabled/disabled in the calling module of
#' upper level.
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
#' @param tl.layout A vector of character ('h' for horizontal, 'v' for vertical)
#' where each item correspond to the orientation of the timeline for a given
#' level of navigation module.
#'
#' @param verbose xxx
#'
#' @return A list of four items:
#' * dataOut A dataset of the same class of the parameter dataIn
#' * steps.enabled A vector of `boolean` of the same length than config@steps
#' * status A vector of `integer(1)` of the same length than the config@steps
#'   vector
#' * reset xxxx
#'
#' @export
#'
#' @rdname mod_nav
#' @importFrom stats setNames
#' 
mod_nav_server <- function(id,
    dataIn = reactive({NULL}),
    is.enabled = reactive({TRUE}),
    remoteReset = reactive({FALSE}),
    is.skipped = reactive({FALSE}),
    tl.layout = NULL,
    verbose = FALSE) {

    options(shiny.fullstacktrace = verbose)


    ### -------------------------------------------------------------###
    ###                                                             ###
    ### ------------------- MODULE SERVER --------------------------###
    ###                                                             ###
    ### -------------------------------------------------------------###
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        # Reactive values that will be used to output the current dataset when
        # the last step is validated
        dataOut <- reactiveValues(
            trigger = NULL,
            value = NULL
        )

        rv <- reactiveValues(
            # Contains the return value of the process module that has been called
            proc = NULL,

            tl.layout = NULL,

            # @field status A boolean vector which contains the status 
            # (validated, skipped or undone) of the steps
            steps.status = NULL,

            # @field dataIn Contains the dataset passed by argument to the 
            # module server
            dataIn = NULL,

            # @field temp.dataIn This variable is used to serves as a tampon 
            # between the input of the module and the functions.
            temp.dataIn = NULL,

            # @field steps.enabled Contains the value of the parameter 
            # 'is.enabled'
            steps.enabled = NULL,

            # A vector of boolean where each element indicates whether
            # the corresponding process is skipped or not
            # ONLY USED WITH PIPELINE
            steps.skipped = NULL,

            # A vector of integers that indicates if each step must be reseted
            # This is an information sent to the child processes. Each time a 
            # child process must be reseted, the corresponding element is 
            # incremented in order to modify its value. Thus, it can be 
            # catched by Shiny observers
            # ---ONLY USED WITH PIPELINE---
            resetChildren = NULL,

            # @field current.pos Stores the current cursor position in the 
            # timeline and indicates which of the process' steps is active
            current.pos = 1,
            
            
            length = NULL,
            
            
            config = NULL,

            # A vector of boolean where each element indicates if the 
            # corresponding child if enable or disable
            child.enabled = NULL,

            # xxxx
            child.reset = NULL,

            # A vector of integers where each element denotes the current 
            # position of the corresponding element.
            child.position = NULL,

            # xxxx
            child.data2send = NULL
        )



        # Catch any event on the 'id' parameter. As this parameter is static 
        # and is attached to the server, this function can be view as the 
        # initialization of the server module. This code is generic to both 
        # process and pipeline modules
        observeEvent(id,
            {
                # The functions of the module server (and ui) are supposed to 
                # be already loaded. Check if it is the case. If not, show a 
                # message and abort
                if (!Found_Mod_Funcs(id)) {
                    return(NULL)
                }

                # When the server starts, the default position is 1
                # Not necessary ?
                rv$current.pos <- 1


                # Call the server module of the process/pipeline which name is 
                # the parameter 'id'. 
                # The name of the server function is prefixed by 'mod_' and 
                # suffixed by '_server'. This will give access to its config
                rv$proc <- do.call(
                    paste0("mod_", id, "_server"),
                    list(
                        id = id,
                        dataIn = reactive({rv$temp.dataIn}),
                        steps.enabled = reactive({rv$steps.enabled}),
                        remoteReset = reactive({input$rstBtn + remoteReset()}),
                        steps.status = reactive({rv$steps.status}),
                        current.pos = reactive({rv$current.pos})
                        )
                    )

                # Update the reactive value config with the config of the 
                # pipeline
                rv$config <- rv$proc$config()

                rv$length <- length(rv$config@steps)

                rv$config@mandatory <- setNames(rv$config@mandatory,
                    nm = names(rv$config@steps)
                )
                rv$steps.status <- setNames(rep(global$UNDONE, rv$length),
                    nm = names(rv$config@steps)
                )

                rv$steps.enabled <- setNames(rep(FALSE, rv$length),
                    nm = names(rv$config@steps)
                )
                rv$steps.skipped <- setNames(rep(FALSE, rv$length),
                    nm = names(rv$config@steps)
                )
                rv$resetChildren <- setNames(rep(0, rv$length),
                    nm = names(rv$config@steps)
                )

                rv$child.data2send <- setNames(lapply(
                    as.list(names(rv$config@steps)),
                    function(x) NULL
                ),
                nm = names(rv$config@steps)
                )

                rv$currentStepName <- reactive({
                    names(rv$config@steps)[rv$current.pos]
                })
                
                rv$tl.layout <- tl.layout

                if (is.null(rv$tl.layout)) {
                    rv$tl.layout <- switch(rv$config@mode,
                        process = c("h"),
                        pipeline = c("h", "h")
                    )
                }

                # Launch the server timeline for this process/pipeline
                do.call(
                    paste0("mod_timeline_", rv$tl.layout[1], "_server"),
                    list(
                        id = paste0("timeline", rv$tl.layout[1]),
                        config = rv$config,
                        status = reactive({rv$steps.status}),
                        enabled = reactive({rv$steps.enabled}),
                        position = reactive({rv$current.pos})
                        )
                    )

                # Launch the UI of the timeline
                output$show_TL <- renderUI({
                    do.call(
                        paste0("mod_timeline_", rv$tl.layout[1], "_ui"),
                        list(ns(paste0("timeline", rv$tl.layout[1])))
                    )
                })
            },
            priority = 1000
        )


        # Specific to pipeline module
        # Used to store the return values (lists) of child processes
        tmp.return <- reactiveValues()


        observeEvent(input$closeModal, {
            removeModal()
        })

        # Update the current position after a click  on the 'Previous' button
        observeEvent(input$prevBtn, ignoreInit = TRUE, {
            rv$current.pos <- NavPage(
                direction = -1,
                current.pos = rv$current.pos,
                len = rv$length
            )
        })

        # Update the current position after a click on the 'Next' button
        observeEvent(input$nextBtn, ignoreInit = TRUE, {
            rv$current.pos <- NavPage(
                direction = 1,
                current.pos = rv$current.pos,
                len = rv$length
            )
        })





        # The parameter 'is.enabled()' is updated by the caller and tells the 
        # process if it is enabled or disabled (remote action from the caller)
        # This enables/disables an entire process/pipeline
        observeEvent(is.enabled(), ignoreNULL = TRUE, ignoreInit = TRUE, {
            if (isTRUE(is.enabled())) {
                rv$steps.enabled <- Update_State_Screens(
                    is.skipped = is.skipped(),
                    is.enabled = is.enabled(),
                    rv = rv
                )
            } else {
                rv$steps.enabled <- setNames(rep(is.enabled(), rv$length),
                    nm = names(rv$config@steps)
                )
            }
        })


        # Catch new status event
        # See https://github.com/daattali/shinyjs/issues/166
        # https://github.com/daattali/shinyjs/issues/25
        observeEvent(rv$steps.status, ignoreInit = TRUE, {
            
            rv$steps.status <- Discover_Skipped_Steps(rv$steps.status)

            rv$steps.enabled <- Update_State_Screens(
                is.skipped = is.skipped(),
                is.enabled = is.enabled(),
                rv = rv
            )

            if (rv$steps.status[rv$length] == global$VALIDATED) {
                # Set current position to the last one
                rv$current.pos <- rv$length

                # If the last step is validated, it is time to send result by
                # updating the 'dataOut' reactiveValue.
                dataOut$trigger <- Timestamp()
                dataOut$value <- rv$dataIn
            }
        })


        # @description
        # The parameter is.skipped() is set by the caller and tells the process
        # if it is skipped or not (remote action from the caller)
        observeEvent(is.skipped(), 
            ignoreNULL = FALSE, 
            ignoreInit = TRUE, {
            if (isTRUE(is.skipped())) {
                rv$steps.status <- All_Skipped_tag(rv$steps.status, global$SKIPPED)
            } else {
                rv$steps.status <- All_Skipped_tag(rv$steps.status, global$UNDONE)
                rv$steps.enabled <- Update_State_Screens(
                    is.skipped = is.skipped(),
                    is.enabled = is.enabled(),
                    rv = rv
                )
            }
        })



        # Catch a click of a the button 'Ok' of a reset modal. This can be in 
        # the local module or in the module parent UI (in this case,
        # it is called a 'remoteReset')
        observeEvent(c(remoteReset(), req(input$modal_ok)),
            ignoreInit = FALSE,
            ignoreNULL = TRUE,
            {
                rv$dataIn <- NULL
                # The cursor is set to the first step
                rv$current.pos <- 1

                # The status of the steps are reinitialized to the default
                # configuration of the process
                rv$steps.status <- setNames(
                    rep(global$UNDONE, length(rv$config@steps)),
                    nm = names(rv$config@steps)
                )

                # browser()
                # If the current module is a pipeline type (node and not
                # leaf), then sent to its children the information that
                # they must reset themself
                # rv$resetChildren <- NULL
                # The reset of the children is made by incrementing
                # the values by 1. This has for effect to be detected
                # by the observeEvent function. It works like an actionButton
                # widget
                if (rv$config@mode == "pipeline") {
                    rv$resetChildren[seq_len(rv$length)] <- 1 +
                        rv$resetChildren[seq_len(rv$length)]
                }

                # browser()
                # Return the NULL value as dataset
                dataOut$trigger <- Timestamp()
                dataOut$value <- rv$dataIn

                #Finally, close the modal
                removeModal()
            }
        )


        # Catch a click on the 'Reset' button. Then, open the modal for info on
        # resetting.
        observeEvent(input$rstBtn, ignoreInit = TRUE, {
            showModal(
                dataModal(ns, rv$config@mode)
            )
        })


        # Show the info panel of a skipped module
        output$SkippedInfoPanel <- renderUI({
            Build_SkippedInfoPanel(
                steps.status = rv$steps.status,
                current.pos = rv$current.pos,
                config = rv$config
            )
        })

        # Show the debug infos is requested
        output$debug_infos_ui <- renderUI({
            req(verbose)
            mod_Debug_Infos_ui(ns("debug_infos"))
        })

        mod_Debug_Infos_server(
            id = "debug_infos",
            title = paste0("Infos from ",rv$config@mode, ": ", id),
            config = reactive({rv$config}),
            rv.dataIn = reactive({rv$dataIn}),
            dataIn = reactive({dataIn()}),
            dataOut = reactive({dataOut}),
            steps.status = reactive({rv$steps.status}),
            steps.skipped = reactive({rv$steps.skipped}),
            current.pos = reactive({rv$current.pos}),
            steps.enabled = reactive({rv$steps.enabled}),
            is.enabled = reactive({is.enabled()})
            )


        # This function uses the UI definition to:
        # 1 - initialize the UI (only the first screen is shown),
        # 2 - encapsulate the UI in a div (used to hide all screens at a time 
        # before showing the one corresponding to the current position)
        output$EncapsulateScreens_ui <- renderUI({
            Build_EncapsulateScreens_ui(
                ns = ns,
                id = id,
                config = rv$config
            )
        })


        # Launch the UI for the user interface of the module
        # Note for devs: apparently, the renderUI() cannot be stored in the 
        # function 'Build..'
        output$nav_mod_ui <- renderUI({
            # Wait until the tl.layout variable is instantiated
            req(rv$tl.layout)
            # browser()
            do.call(paste0("Build_nav_", rv$tl.layout[1], "_ui"), list(ns))
        })


        # Catch a new value on the parameter 'dataIn()' variable, sent by the
        # caller. This value may be NULL or contain a dataset.
        # The first action is to store the dataset in the temporary variable
        # temp.dataIn. Then, two behaviours:
        # 1 - if the variable is NULL. xxxx
        # 2 - if the variable contains a dataset. xxx
        observeEvent(dataIn(), 
            ignoreNULL = FALSE, 
            ignoreInit = FALSE, {
                req(rv$config)
                isolate({
                    # A new value on dataIn() means a new dataset sent to the 
                    # process
                    # browser()
                    rv$current.pos <- 1

                    # Get the new dataset in a temporary variable
                    rv$temp.dataIn <- dataIn()

                    # The mode pipeline is a node and has to send
                    # datasets to its children
                    if (rv$config@mode == "pipeline") {
                        if (is.null(rv$dataIn)) {
                            res <- PrepareData2Send(rv = rv, pos = rv$current.pos)
                            rv$child.data2send <- res$data2send
                            rv$steps.enabled <- res$steps.enabled
                            }
                        }

                    if (is.null(dataIn())) {
                        # The process has been reseted or is not concerned
                        # Disable all screens of the process
                        rv$steps.enabled <- ToggleState_Screens(
                            cond = FALSE,
                            range = seq_len(rv$length),
                            is.enabled = is.enabled,
                            rv = rv
                            )
                        } else {
                            # A new dataset has been loaded
                            # # Update the different screens in the process
                            rv$steps.enabled <- Update_State_Screens(
                                is.skipped = is.skipped(),
                                is.enabled = is.enabled(),
                                rv = rv
                            )
                            }

                    # Update the initial length of the dataset with the length
                    # of the one that has been received
                    rv$original.length <- length(dataIn())
                    # Enable the first screen
                    rv$steps.enabled <- ToggleState_Screens(
                        cond = TRUE,
                        range = 1,
                        is.enabled = is.enabled(),
                        rv = rv
                        )
                    })
                })


        observeEvent(rv$current.pos, ignoreInit = TRUE, {
            ToggleState_NavBtns(
                current.pos = rv$current.pos,
                nSteps = rv$length
            )
            shinyjs::hide(selector = paste0(".page_", id))
            shinyjs::show(names(rv$config@steps)[rv$current.pos])

            if (rv$config@mode == "pipeline") {
                # Specific to pipeline code
                res <- PrepareData2Send(rv = rv, pos = NULL)
                rv$child.data2send <- res$data2send
                rv$steps.enabled <- res$steps.enabled

                if (rv$steps.status[rv$current.pos] == global$VALIDATED) {
                    rv$child.position[rv$current.pos] <- paste0("last_", 
                        Timestamp())
                }
            }
            
        })




        # Catch the moment when the mode is defined
        # Then, launch observers and functions specific to
        # processes nor pipelines
        observeEvent(req(rv$config), {
            if (!(rv$config@mode %in% c("process", "pipeline"))) {
                warning("'mode' must be either 'process' or 'pipeline'.")
                return(NULL)
            }

            switch(rv$config@mode,
                default = {},
                pipeline = {
                    # Before continuing the initialization, check if all 
                    # modules functions (the steps contained in the slot
                    # `rv$config@steps` are found in the Global environment

                    for (i in names(rv$config@steps)) {
                        if (!Found_Mod_Funcs(i)) {
                            return(NULL)
                        }
                    }


                    rv$steps.skipped <- setNames(rep(FALSE, rv$length),
                        nm = names(rv$config@steps)
                    )
                    rv$resetChildren <- setNames(rep(0, rv$length),
                        nm = names(rv$config@steps)
                    )

                    # Launch the ui for each step of the pipeline
                    # This function could be stored in the source file of the
                    # pipeline but the strategy is to insert minimum extra 
                    # code in the files for pipelines and processes. This is 
                    # useful when other devs will develop other pipelines and 
                    # processes. Thus, it will be easier.
                    rv$config@ll.UI <- setNames(lapply(
                        names(rv$config@steps),
                        function(x) {mod_nav_ui(ns(x))}
                        ),
                    nm = paste0(names(rv$config@steps))
                    )

                    # Launch the server for each step of the pipeline
                    lapply(names(rv$config@steps), function(x) {
                        tmp.return[[x]] <- mod_nav_server(
                            id = x,
                            dataIn = reactive({rv$child.data2send[[x]]}),
                            is.enabled = reactive({isTRUE(rv$steps.enabled[x])}),
                            remoteReset = reactive({rv$resetChildren[x]}),
                            is.skipped = reactive({isTRUE(rv$steps.skipped[x])}),
                            tl.layout = rv$tl.layout[-1],verbose = verbose)
                        })





                    ActionOn_Data_Trigger <- function() {
                        processHasChanged <- newValue <- NULL

                        # Get the values returned by all children (steps) of 
                        # the module
                        values.children <- GetValuesFromChildren(
                            tmp.return = tmp.return,
                            config = rv$config
                        )
                        triggerValues <- values.children$triggers
                        return.values <- values.children$values

                        if (verbose) {
                            cat("---------- Data received from children ---\n")
                            print(return.values)
                            cat("------------------------------------------\n")
                        }

                        if (is.null(return.values)) {
                            # The entire pipeline has been reseted
                            rv$dataIn <- NULL
                            rv$steps.status[seq_len(rv$length)] <- global$UNDONE
                        } else {
                            .cd <- max(triggerValues, na.rm = TRUE) == triggerValues
                            # ind.process.has.changed <- which(.cd)
                            processHasChanged <- names(rv$config@steps)[which(.cd)]

                            # Get the new value
                            newValue <- tmp.return[[processHasChanged]]$dataOut()$value

                            ret <- ActionOn_Child_Changed(
                                temp.dataIn = rv$temp.dataIn,
                                dataIn = rv$dataIn,
                                steps.status = rv$steps.status,
                                steps = rv$config@steps,
                                steps.enabled = rv$steps.enabled,
                                steps.skipped = rv$steps.skipped,
                                processHasChanged = processHasChanged,
                                newValue = newValue
                            )


                            rv$dataIn <- ret$dataIn
                            rv$steps.status <- ret$steps.status
                            rv$steps.enabled <- ret$steps.enabled
                            rv$steps.skipped <- ret$steps.skipped
                        }

                        # Send result
                        dataOut$trigger <- Timestamp()
                        dataOut$value <- rv$dataIn
                    }


                    # Catch the returned values of the processes attached to 
                    # pipeline
                    observeEvent(lapply(
                        names(rv$config@steps),
                        function(x) {tmp.return[[x]]$dataOut()$trigger}
                        ), ignoreInit = TRUE,
                        {
                        ActionOn_Data_Trigger()
                            }
                        )
                    },
                process = {

                    # Launch the horizontal timeline server
                    # The parameter 'config' is used to xxx
                    # The parameter 'status' is used to color the bullets
                    # the parameter 'position' is used to put the cursor at the
                    # current position
                    # The parameter 'enabled' is used to modify the bullets 
                    # whether the corresponding step is enabled or disabled
                    # mod_timeline_h_server(id = 'timeline',
                    # config =  rv$config,
                    # status = reactive({rv$steps.status}),
                    # position = reactive({rv$current.pos}),
                    # enabled = reactive({rv$steps.enabled})
                    # )


                    observeEvent(rv$proc$dataOut()$trigger,
                        ignoreNULL = TRUE,
                        ignoreInit = TRUE,
                        {
                            # If a value is returned, this is because the 
                            # current step has been validated
                            rv$steps.status[rv$current.pos] <- global$VALIDATED

                            # Look for new skipped steps
                            rv$steps.status <- Discover_Skipped_Steps(rv$steps.status)


                            # If it is the first step (description step), then
                            # load the dataset in work variable 'dataIn'
                            if (rv$current.pos == 1) {
                                rv$dataIn <- rv$temp.dataIn
                            } # View intermediate datasets
                            else if (rv$current.pos > 1 && rv$current.pos < rv$length) {
                                rv$dataIn <- rv$proc$dataOut()$value
                            } 
                            # Manage the last dataset which is the real one 
                            # returned by the process
                            else if (rv$current.pos == rv$length) {
                                # Update the work variable of the nav_process 
                                # with the dataset returned by the process
                                # Thus, the variable rv$temp.dataIn keeps 
                                # trace of the original dataset sent to
                                # this  workflow and will be used in case of 
                                # reset
                                rv$dataIn <- rv$proc$dataOut()$value

                                # Update the 'dataOut' reactive value to return
                                #  this dataset to the caller. this nav_process 
                                #  is only a bridge between the process and  the
                                #  caller
                                # For a pipeline, the output is updated each 
                                # time a process has been validated
                                dataOut$trigger <- Timestamp()
                                dataOut$value <- rv$dataIn
                            }
                        }
                    )



                    observeEvent(req(!is.null(rv$position)), 
                        ignoreInit = TRUE, {
                        pos <- strsplit(rv$position, "_")[[1]][1]
                        if (pos == "last") {
                            rv$current.pos <- rv$length
                        } else if (is.numeric(pos)) {
                            rv$current.pos <- rv$position
                        }
                    })
                }
            )
        })





        # The return value of the nav_process module server
        # The item 'dataOut' has been updated by the module process and it is
        # returned to the function that has called this nav_process module (it
        # can be a module, a Shiny app or another nav module for example,
        # nav_pipeline)
        list(
            dataOut = reactive({dataOut}),
            steps.enabled = reactive({rv$steps.enabled}),
            status = reactive({rv$steps.status})
            )
        })
}
