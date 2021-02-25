btn_style <- "display:inline-block; vertical-align: middle; padding: 7px"

mod_Protein_ui <- function(id){
  ns <- NS(id)
  mod_nav_pipeline_ui(ns('Protein'))
}



mod_Protein_server <- function(id,
                               dataIn = NULL,
                               is.enabled = reactive({TRUE}),
                               reset = reactive({FALSE}),
                               position = reactive({NULL}),
                               skipped = reactive({NULL})
                               ){
  
  #' @field global xxxx
  global <- list(
    VALIDATED = 1,
    UNDONE = 0,
    SKIPPED = -1
  )
  
  
  config <- reactiveValues(
    name = 'Protein',
    steps = c('Description', 'Normalization'),
    mandatory = c(T, F)
    
  )
  
  
  rv.nav <- reactiveValues(
    return = TRUE,
    status = NULL,
    dataIn = NULL,
    temp.dataIn = NULL
  )
  
  ###-------------------------------------------------------------###
  ###                                                             ###
  ### ------------------- MODULE SERVER --------------------------###
  ###                                                             ###
  ###-------------------------------------------------------------###
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    rv.widgets <- reactiveValues()
    
    observeEvent(id, {
      for (x in config$steps)
      source(file.path('.', paste0('mod_', paste0(id, '_', x), '.R')), local=TRUE)
      
      # Build list of screens
      config$ll.UI <- lapply(config$steps,
                             function(x){
                               do.call(paste0('mod_', id, '_', x, '_ui'),
                                       list(ns(paste0(id, '_', x))))
                             })
      
      #browser()
      rv.nav$return <- mod_nav_pipeline_server('Protein',
                                               config = reactive({config}),
                                               status = reactive({rv.nav$status}),
                                               dataIn = reactive({rv.nav$dataIn}),
                                               is.enabled = reactive({is.enabled()})
                                               )
      
    }, priority=1000)
    
    
    observeEvent(rv.nav$return$status(), {rv.nav$status <- rv.nav$return$status()})
    observeEvent(rv.nav$return$dataOut()$trigger, {rv.nav$dataIn <- rv.nav$return$dataOut()$value})
    observeEvent(dataIn(), {rv.nav$temp.dataIn <- dataIn()})
    
    
   # Return value of module
   # DO NOT MODIFY THIS PART
    reactive({rv.nav$return$dataOut()})
    
    
  }
  )
}