# mod_pipeline_process_ui <- function(id){
# 	ns <- NS(id)
# }
# 
# 
# mod_pipeline_process_server <- function(id,
# 		dataIn = reactive({NULL}),
# 		steps.enabled = reactive({NULL}),
# 		remoteReset = reactive({FALSE})
# 		){
# 
# 	# This list contains the basic configuration of the process
# 	config <- list(
# 		# Name of the process
# 		name = 'process',
# 		# Name of the pipeline it belongs to
# 		parent = 'pipeline',
# 		# List of all steps of the process
# 		steps = c('Description', 'Step 1', 'Step 2', 'Save'),
# 		# A vector of boolean indicating if the steps are mandatory or not.
# 		mandatory = c(TRUE, TRUE, FALSE, TRUE)
# 		)
# 
# 	# # Define default selected values for widgets
# 	widgets.default.values <- list(
# 		# The following lines are given as example. Our advice is to use the 
# 		same nomenclature for the variables of widgets:
# 		two strings separated by '_', the first one is the name of the step
# 		 while the secondone is the nameof the widget
# 		# Step1_select1 = 1,
# 		# Step1_select2 = NULL,
# 		# Step1_select3 = 1,
# 		# Step2_select2_1 = 1,
# 	# Step2_select2_2 = 1
# 	)
# 
# 	###-------------------------------------------------------------###
# 	###                                                             ###
# 	### ------------------- MODULE SERVER --------------------------###
# 	###                                                             ###
# 	###-------------------------------------------------------------###
# 	moduleServer(id, function(input, output, session) {
# 		  ns <- session$ns
# 
# 		# Declaration of the variables that will contain the values of the widgets
# 		# To avoid confusion, the first string is the name of the step while the second is the name
# 		# of the widget
# 		rv.widgets <- reactiveValues(
# 			#Step1_select1 = widgets.default.values$Step1_select1,
# 			#Step1_select2 = widgets.default.values$Step1_select2,
# 			#Step1_select3 = widgets.default.values$Step1_select3,
# 			#Step2_select2_1 = widgets.default.values$Step2_select2_1,
# 			#Step2_select2_2 = widgets.default.values$Step2_select2_2
# 		)
# 
# 		# Reactive values during the run of the process
# 		rv <- reactiveValues(
# 			# Stores the object given in input of the process
# 			  dataIn = NULL,
# 			  # A vector of boolean indicating the status (UNDONE, SKIPPED or VALIDATED) of the steps
# 			  steps.status = NULL,
# 			  # xxx
# 			  reset = NULL,
# 			 # A vector of boolean indicating if the steps are enabled or disabled
# 			  steps.enabled = NULL
# 		)
# 
# 		# Returned value of the process
# 		# * The trigger variable is used to trigger an event that can be catched by the 
# 		#   Shiny functions observe() and observeEvent()
# 		# * The value variable contains the object to return to the instance that has called the process.
# 		dataOut <- reactiveValues(
# 			  trigger = NULL,
# 			  value = NULL
# 			)
# 
# 	# Initialization of the module
# 		observeEvent(steps.enabled(), ignoreNULL = TRUE, {
# 		if (is.null(steps.enabled()))
# 			rv$steps.enabled <- setNames(rep(FALSE, rv$length), 
# 	                                  rv$config$steps)
# 		  else
# 	    rv$steps.enabled <- steps.enabled()
# 		})
# 
# 	# Set all the widgets to their default value after the remote Reset()
# 	observeEvent(remoteReset(), {
# 	  lapply(names(rv.widgets), function(x){
# 	    rv.widgets[[x]] <- widgets.default.values[[x]]
# 	  })
# 	})
# 
# 	  ###### ------------------- Code for Description (step 0) -------------------------    #####
# 	  output$Description <- renderUI({
# 	  tagList(
# 	    includeMarkdown(paste0('md/', paste0(config$parent, '_', config$name, '.md'))),
# 	    uiOutput(ns('datasetDescription')),
# 	    uiOutput(ns('validationBtn_ui'))
# 	  )
# 	})
# 
# 
# 	observeEvent(input$btn_validate_Description, ignoreInit = TRUE, ignoreNULL = TRUE, {
# 	  rv$dataIn <- dataIn()
# 	  rv$steps.status['Description'] <- global$VALIDATED
# 	  dataOut$trigger <- Magellan::Timestamp()
# 	  dataOut$value <- rv$dataIn
# 	})
# 
# 	output$validationBtn_ui <- renderUI({
# 	  if (isTRUE(rv$steps.enabled['Description'])  )
# 	    actionButton(ns('btn_validate_Description'),
# 	                 paste0('Start ', config$name),
# 	                 class = btn_success_color)
# 	  else
# 	    shinyjs::disabled(
# 	      actionButton(ns('btn_validate_Description'),
# 	                   paste0('Start ', config$name),
# 	                   class = btn_success_color)
# 	    )
# 	})
# 
# 	# Return value of module
# 	# DO NOT MODIFY THIS PART
# 	list(config = reactive({
# 	  config$ll.UI <- setNames(lapply(config$steps,
# 	                                  function(x){
# 	                                    do.call('uiOutput', list(ns(x)))
# 	                                  }),
# 	                           paste0('screen_', config$steps)
# 	  )
# 	  config
# 	}),
# 	dataOut = reactive({dataOut})
# 	#steps.status = reactive({rv$steps.status})
# 	)
# 	}
# 	)
