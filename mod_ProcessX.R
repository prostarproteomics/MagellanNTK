
    mod_ProcessX_ui <- function(id){
    ns <- NS(id)
    }

    


  mod_ProcessX_server <- function(id,
  dataIn = reactive({NULL}),
  steps.enabled = reactive({NULL}),
  remoteReset = reactive({FALSE}),
  steps.status = reactive({NULL}),
  current.pos = reactive({1}),
  verbose = FALSE
  ){
    

    # This list contains the basic configuration of the process
    config <- Config(
    # Define the type of module
    mode = 'process',
    name = 'ProcessX',
    # List of all steps of the process
    steps = c('Description', 'Step 1', 'Step 2', 'Save'),

    # A vector of boolean indicating if the steps are mandatory or not.
    mandatory = c(TRUE, TRUE, FALSE, TRUE),

    path_to_md_dir = '.'
    )
    

    # Define default selected values for widgets
    # This is only for simple workflows
    widgets.default.values <- list()
    rv.custom.default.values <- list()
    



    ###-------------------------------------------------------------###
    ###                                                             ###
    ### ------------------- MODULE SERVER --------------------------###
    ###                                                             ###
    ###-------------------------------------------------------------###
    moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Insert necessary code which is hosted by Magellan
    # DO NOT MODIFY THIS LINE
    #eval(str2expression(Get_Code_Update_Config()))

    # Insert necessary code which is hosted by Magellan
    # DO NOT MODIFY THIS LINE
    eval(
      str2expression(
        Get_Worflow_Core_Code(
      w.names = names(widgets.default.values),
      rv.custom.names = names(rv.custom.default.values)
          )
        )
      )

    

# >>>
# >>> START ------------- Code for Description UI---------------
# >>> 


output$Description <- renderUI({
  file <- paste0(config@path_to_md_dir, '/', id, '.md')
  
  tagList(
    # In this example, the md file is found in the module_examples directory
    # but with a real app, it should be provided by the package which
    # contains the UI for the different steps of the process module.
    # system.file(xxx)
    
    if (file.exists(file))
      includeMarkdown(file)
    else
      p('No Description available'),
    
    
    # Used to show some information about the dataset which is loaded
    # This function must be provided by the package of the process module
    uiOutput(ns('datasetDescription_ui')),
    
    # Insert validation button
    uiOutput(ns('Description_btn_validate_ui'))
  )
})

output$datasetDescription_ui <- renderUI({
  # Insert your own code to vizualise some information
  # about your dataset. It will appear once the 'Start' button
  # has been clicked
  
})

output$Description_btn_validate_ui <- renderUI({
  widget <- actionButton(ns('Description_btn_validate'),
    'Start',
    class = btn_success_color)
  toggleWidget(widget, rv$steps.enabled['Description'])
})


observeEvent(input$Description_btn_validate, {
  rv$dataIn <- dataIn()
  dataOut$trigger <- Magellan::Timestamp()
  dataOut$value <- rv$dataIn
  rv$steps.status['Description'] <- global$VALIDATED
})



          output$Step1 <- renderUI({ })

            
          output$Step2 <- renderUI({ })

            
          output$Save <- renderUI({ })

            

    # Insert necessary code which is hosted by Magellan
    # DO NOT MODIFY THIS LINE
    eval(parse(text = Module_Return_Func()))
    }
    )
}

    
