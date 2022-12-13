
    mod_PipelineA_ui <- function(id){
    ns <- NS(id)
    }

    


    mod_PipelineA_server <- function(id,
        dataIn = reactive({NULL}),
        steps.enabled = reactive({NULL}),
        remoteReset = reactive({FALSE}),
        steps.status = reactive({NULL}),
        current.pos = reactive({1})
        ){
    

    # This list contains the basic configuration of the process
    config <- Config(
    # Define the type of module
    mode = 'process',
    name = 'c('PipelineA')',
    # List of all steps of the process
    steps = c('Description', 'Process 1', 'Process 2', 'Process 3'),

    # A vector of boolean indicating if the steps are mandatory or not.
    mandatory = c(TRUE, TRUE, FALSE, TRUE)
    )
    



    ###-------------------------------------------------------------###
    ###                                                             ###
    ### ------------------- MODULE SERVER --------------------------###
    ###                                                             ###
    ###-------------------------------------------------------------###
    moduleServer(id, function(input, output, session) {
        ns <- session$ns


    eval(
      str2expression(
        Get_Worflow_Core_Code(
      w.names = names(widgets.default.values),
          rv.custom.names = names(rv.custom.default.values)
          
          )
        )
      )

        # Insert code for the description renderUI()
    #eval(parse(text = Get_Code_for_module_Description(config@name)),
    #    envir = .GlobalEnv)

    # Insert necessary code which is hosted by MagellanNTK
    # DO NOT MODIFY THIS LINE
    eval(parse(text = Module_Return_Func()))
    }
    )
}

    
