mod_Process1_ui <- function(id){
  ns <- NS(id)
}




mod_Process1_server <- function(id,
                                dataIn = reactive({NULL}),
                                steps.enabled = reactive({NULL}),
                                remoteReset = reactive({FALSE}),
                                steps.status = reactive({NULL}),
                                current.pos = reactive({1})
){
  
  
  # This list contains the basic configuration of the process
  config <- list(
    # Define the type of module
    mode = 'process',
    
    # List of all steps of the process
    steps = c('Step 1', 'Step 2', 'Save'),
    
    # A vector of boolean indicating if the steps are mandatory or not.
    mandatory = c(TRUE, FALSE, TRUE)
  )
  
  
  # Define default selected values for widgets
  # This is only for simple workflows
  widgets.default.values <- list()
  
  
  
  
  
  ###-------------------------------------------------------------###
  ###                                                             ###
  ### ------------------- MODULE SERVER --------------------------###
  ###                                                             ###
  ###-------------------------------------------------------------###
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Insert necessary code which is hosted by Magellan
    # DO NOT MODIFY THIS LINE
    eval(str2expression(Get_Code_Update_Config()))
    
    eval(str2expression(
      SimpleWorflowCoreCode(
        name = id,
        widgets = names(widgets.default.values),
        steps = config$steps)
    )
    )
    
    
    
    output$Step1 <- renderUI({ })
    
    
    output$Step2 <- renderUI({ })
    
    
    output$Save <- renderUI({ })
    
    
    
    # Insert necessary code which is hosted by Magellan
    # DO NOT MODIFY THIS LINE
    eval(parse(text = Module_Return_Func()))
  }
  )
}
