
    PipeA_ProcessX_conf <- function(){
      Config(
        mode = 'process',
        fullname = 'PipeA_ProcessX',
        steps = c('Step1', 'Step2'),
        mandatory = c(TRUE, FALSE)
        )
      }
    

    PipeA_ProcessX_ui <- function(id){
    ns <- NS(id)
    }

    


  PipeA_ProcessX_server <- function(id,
    dataIn = reactive({NULL}),
    steps.enabled = reactive({NULL}),
    remoteReset = reactive({FALSE}),
    steps.status = reactive({NULL}),
    current.pos = reactive({1}),
    verbose = FALSE,
    path = path
    ){
    



    ###-------------------------------------------------------------###
    ###                                                             ###
    ### ------------------- MODULE SERVER --------------------------###
    ###                                                             ###
    ###-------------------------------------------------------------###
    moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Insert necessary code which is hosted by MagellanNTK
    # DO NOT MODIFY THIS LINE
    core.code <- Get_Worflow_Core_Code(
        name = id,
        w.names = names(widgets.default.values),
        rv.custom.names = names(rv.custom.default.values)
        )
          
    eval(str2expression(core.code))

    

    # Define default selected values for widgets
    # This is only for simple workflows
    widgets.default.values <- list()
    rv.custom.default.values <- list()
    

# >>>
# >>> START ------------- Code for Description UI---------------
# >>> 


output$Description <- renderUI({
  file <- paste0(., '/md/', id, '.md')
  
  tagList(
    # In this example, the md file is found in the extdata/module_examples directory
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
    class = GlobalSettings$btn_success_color)
  toggleWidget(widget, rv$steps.enabled['Description'])
})


observeEvent(input$Description_btn_validate, {
  rv$dataIn <- dataIn()
  dataOut$trigger <- Timestamp()
  dataOut$value <- rv$dataIn
  rv$steps.status['Description'] <- global$VALIDATED
})




# >>> START ------------- Code for step 3 UI---------------
    output$Save <- renderUI({
       tagList(
        # Insert validation button
        # This line is necessary. DO NOT MODIFY
        uiOutput(ns('Save_btn_validate_ui')),
        uiOutput(ns('dl_ui'))
      )
    })
    
    output$dl_ui <- renderUI({
      req(config@mode == 'process')
      req(rv$steps.status['Save'] == global$VALIDATED)
      dl_ui(ns('createQuickLink'))
    })
    
    output$Save_btn_validate_ui <- renderUI({
      toggleWidget(actionButton(ns('Save_btn_validate'), 'Save',
                                  class = GlobalSettings$btn_success_color),
                     rv$steps.enabled['Save']
                     )
    })
    observeEvent(input$Save_btn_validate, {
      # Do some stuff
      rv$dataIn <- Add_Datasets_to_Object(object = rv$dataIn,
                                          dataset = rnorm(1:5),
                                          name = id)
      
      # DO NOT MODIFY THE THREE FOLLOWINF LINES
      dataOut$trigger <- Timestamp()
      dataOut$value <- rv$dataIn
      rv$steps.status['Save'] <- global$VALIDATED
      dl_server('createQuickLink', 
        dataIn = reactive({rv$dataIn}))
      
    })
    # <<< END ------------- Code for step 3 UI---------------





          output$Step1 <- renderUI({ })

            
          output$Step2 <- renderUI({ })

            

    # Insert necessary code which is hosted by MagellanNTK
    # DO NOT MODIFY THIS LINE
    eval(parse(text = Module_Return_Func()))
    }
    )
}

    
