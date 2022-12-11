
    mod_ProcessToto_ui <- function(id){
    ns <- NS(id)
    }

    


  mod_ProcessToto_server <- function(id,
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

    # List of all steps of the process
    steps = c('Step 1', 'Step 2', 'Save'),

    # A vector of boolean indicating if the steps are mandatory or not.
    mandatory = c(TRUE, FALSE, TRUE),

    path_to_md_dir = ''
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

    

output$Step1 <- renderUI({ })


output$Step2 <- renderUI({ })


output$Save <- renderUI({ })



    # Insert necessary code which is hosted by Magellan
    # DO NOT MODIFY THIS LINE
    eval(parse(text = Module_Return_Func()))
    }
    )
}

    
