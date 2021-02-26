btn_style <- "display:inline-block; vertical-align: middle; padding: 7px"

source(file.path('.', 'mod_Protein_Description.R'), local=TRUE)$value
source(file.path('.', 'mod_Protein_Normalization.R'), local=TRUE)$value



mod_Protein_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns('show_processes_screens'))
  )
}



mod_Protein_server <- function(id,
                               dataIn = reactive({NULL}),
                               steps.enabled = reactive({NULL}),
                               reset = reactive({FALSE}),
                               status = reactive({NULL})){
  
  #' @field global xxxx
  global <- list(
    VALIDATED = 1,
    UNDONE = 0,
    SKIPPED = -1
  )
  
  
  config <- reactiveValues(
    name = 'Protein',
    steps = c('Description', 'Normalization'),
    mandatory = c(T, T, F)
    
  )

  ###-------------------------------------------------------------###
  ###                                                             ###
  ### ------------------- MODULE SERVER --------------------------###
  ###                                                             ###
  ###-------------------------------------------------------------###
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    rv <- reactiveValues(
      dataIn = NULL,
      dataOut = NULL,
      status = NULL,
      reset = NULL,
      steps.enabled = NULL
    )
    
    
    output$show_processes_screens <- renderUI({
      
    })
    
    
    observeEvent(status(), { rv$status <- status()})
    # Initialization of the module
    observeEvent(steps.enabled(), ignoreNULL = TRUE, {
      if (is.null(steps.enabled()))
        rv$steps.enabled <- setNames(rep(FALSE, rv.process$length), rv.process$config$steps)
      else
        rv$steps.enabled <- steps.enabled()
    })
    
    
   
    
    
   # Return value of module
   # DO NOT MODIFY THIS PART
    list(config = reactive({
      
      config
    }),
    dataOut = reactive({rv$dataOut}),
    status = reactive({rv$status})
    )
    
  }
  )
}