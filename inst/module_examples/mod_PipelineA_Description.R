
mod_PipelineA_Description_ui <- function(id){
  ns <- NS(id)
}



mod_PipelineA_Description_server <- function(id,
                                             dataIn = reactive({NULL}),
                                             steps.enabled = reactive({NULL}),
                                             remoteReset = reactive({FALSE}),
                                             steps.status = reactive({NULL}),
                                             current.pos = reactive({1}),
                                             verbose = FALSE
                                             
){
  
  config <- list(
    mode = 'process',
    
    name = 'Description',
    
    # List of all steps of the process
    steps = c('Description'),
    # A vector of boolean indicating if the steps are mandatory or not.
    mandatory = c(TRUE),
    
    path_to_md_dir = system.file('module_examples/md/', package='Magellan')
  )
  
  # Define default selected values for widgets
  # By default, this list is empty for the Description module
  # but it can be customized
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
    eval(str2expression(Get_Worflow_Core_Code(
      w.names = names(widgets.default.values)
    )))
    
    ###### ------------------- Code for Description (step 0) -------------------------    #####
    output$Description <- renderUI({
      file <- paste0(config$path_to_md_dir, '/', id, '.md')
      tagList(
        if (file.exists(file))
          includeMarkdown(file)
        else
          p('No Description available'),
        
        uiOutput(ns('datasetDescription')),
        
        # Insert validation button
        uiOutput(ns('Description_btn_validate_ui'))
      )
    })
    
    
    output$Description_btn_validate_ui <- renderUI({
      widget <- actionButton(ns("Description_btn_validate"),
                             "Start",
                             class = btn_success_color)
      toggleWidget(rv$steps.enabled['Description'], widget)
    })
    
    
    observeEvent(input$Description_btn_validate, {
      rv$dataIn <- dataIn()
      dataOut$trigger <- Magellan::Timestamp()
      dataOut$value <- rv$dataIn
      rv$steps.status['Description'] <- global$VALIDATED
    })
    
    
    
    
    
    # Insert necessary code which is hosted by Magellan
    # DO NOT MODIFY THIS LINE
    eval(parse(text = Module_Return_Func()))
    
  }
  )
}
