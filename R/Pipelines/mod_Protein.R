btn_style <- "display:inline-block; vertical-align: middle; padding: 7px"

mod_Protein_ui <- function(id){
  ns <- NS(id)
  uiOutput(ns('ui'))
}



mod_Protein_server <- function(id,
                               dataIn = NULL,
                               tag.enabled = reactive({TRUE})){
  
  config <- reactiveValues(
    name = 'Protein',
    steps = c('Description', 'Normalization'),
    mandatory = c(T, F)
    
  )
  ###-------------------------------------------------------------###
  ###                                                             ###
  ### ------------------- MODULE SERVER --------------------------###
  ###                                                             ###
  ###-------------------------------------------------------------###
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    #source(file.path('.', 'code_for_pipeline.R'), local=TRUE)$value
    
    
    
   
    reactive({dataOut})
    
    
  }
  )
}