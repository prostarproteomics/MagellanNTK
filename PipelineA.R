
    PipelineA_conf <- function(){
      Config(
        mode = 'pipeline',
        fullname = 'PipelineA',
        steps = c('Process1', 'Process2', 'Process3'),
        mandatory = c(TRUE, FALSE, TRUE)
        )
      }
    

    PipelineA_ui <- function(id){
    ns <- NS(id)
    }

    


  PipelineA_server <- function(id,
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

    

###
###
###

#' @export
Description_conf <- function(){
Config(
    mode = 'process',
    fullname = 'Description',
    steps = c('Description'),
    mandatory = c(TRUE)
    )
}
    
#' @export
Description_ui <- function(id){
  ns <- NS(id)
}


#' @export
Description_server <- function(id,
  dataIn = reactive({NULL}),
  steps.enabled = reactive({NULL}),
  remoteReset = reactive({FALSE}),
  steps.status = reactive({NULL}),
  current.pos = reactive({1}),
  verbose = FALSE
){
  
  
  
  # Define default selected values for widgets
  # By default, this list is empty for the Description module
  # but it can be customized
  widgets.default.values <- NULL
  rv.custom.default.values <- NULL
  
  ###-------------------------------------------------------------###
  ###                                                             ###
  ### ------------------- MODULE SERVER --------------------------###
  ###                                                             ###
  ###-------------------------------------------------------------###
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Insert necessary code which is hosted by MagellanNTK
    # DO NOT MODIFY THIS LINE
    eval(
      str2expression(
        Get_Worflow_Core_Code(
        name = id,
          w.names = names(widgets.default.values),
          rv.custom.names = names(rv.custom.default.values)
          
        )
      )
    )
    
    #rv.custom <- reactiveValues()
    #rv.custom.default.values <- list()
    
    
    # Insert necessary code which is hosted by MagellanNTK
    # DO NOT MODIFY THIS LINE
    eval(parse(text = Module_Return_Func()))
    
  }
  )
}




    # Insert necessary code which is hosted by MagellanNTK
    # DO NOT MODIFY THIS LINE
    eval(parse(text = Module_Return_Func()))
    }
    )
}

    
