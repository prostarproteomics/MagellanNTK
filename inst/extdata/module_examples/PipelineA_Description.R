
###
###
###

#' @export
#' 
PipelineA_Description_conf <- function(){
  Config(
    name = 'Description',
    
    # The name of the parent module, if exists
    parent = 'PipelineA',
    
    # Define the type of module
    mode = 'process',
    # List of all steps of the process
    steps = '',
    # A vector of boolean indicating if the steps are mandatory or not.
    mandatory = ''
  )
}



#' @export
PipelineA_Description_ui <- function(id){
  ns <- NS(id)
}


#' @export
PipelineA_Description_server <- function(id,
                                         dataIn = reactive({NULL}),
                                         steps.enabled = reactive({NULL}),
                                         remoteReset = reactive({FALSE}),
                                         steps.status = reactive({NULL}),
                                         current.pos = reactive({1}),
                                         verbose = FALSE,
                                         path = NULL
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
    
    ###### ------------------- Code for Description (step 0) -------------------------    #####
    output$Description <- renderUI({
      file <- paste0(path, '/md/', name, '.md')
      tagList(
        if (file.exists(file))
          includeMarkdown(file)
        else
          p('No Description available'),
        
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
      widget <- actionButton(ns("Description_btn_validate"),
                             "Start",
                             class = GlobalSettings$btn_success_color)
      toggleWidget(widget, rv$steps.enabled['Description'])
    })
    
    
    observeEvent(input$Description_btn_validate, {
      rv$dataIn <- dataIn()
      dataOut$trigger <- Timestamp()
      dataOut$value <- rv$dataIn
      rv$steps.status['Description'] <- global$VALIDATED
    })
    
    
    # Insert necessary code which is hosted by MagellanNTK
    # DO NOT MODIFY THIS LINE
    eval(parse(text = Module_Return_Func()))
    
  }
  )
}
