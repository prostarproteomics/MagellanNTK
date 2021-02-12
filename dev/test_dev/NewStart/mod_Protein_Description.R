
btn_style <- "display:inline-block; vertical-align: middle; padding: 7px"
source(file.path('.', 'mod_timeline_h.R'), local=TRUE)$value
#source(file.path('.', 'mod_timeline_v.R'), local=TRUE)$value

mod_Protein_Description_ui <- function(id){
  ns <- NS(id)
  uiOutput(ns('ui'))
}



mod_Protein_Description_server <- function(id,
                                             dataIn = NULL,
                                             tag.enabled = reactive({TRUE}),
                                             reset = reactive({FALSE}),
                                             position = reactive({NULL}),
                                             skipped = reactive({NULL})
){
  
  config <- reactiveValues(
    name = 'Protein_Description',
    steps = c('Description'),
    mandatory = c(T)
  )
  
  # Define default selected values for widgets
  widgets.default.values <- list()
  
  ###-------------------------------------------------------------###
  ###                                                             ###
  ### ------------------- MODULE SERVER --------------------------###
  ###                                                             ###
  ###-------------------------------------------------------------###
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    source(file.path('.', 'process.R'), local=TRUE)$value
    
    rv.widgets <- reactiveValues()
    
    ###-----------------------------------------------------------------------------------------------------
    
    
    
    
    ### ----------------------------------------------------------------------------------------------------
    
    ###### ------------------- Code for Description (step 0) -------------------------    #####
    output$Description <- renderUI({
      rv.process$tl.tags.enabled
      
      wellPanel(
        tagList(
          includeMarkdown( system.file("app/md", paste0(rv.process$config$name, ".md"), package="Magellan")),
          uiOutput(ns('datasetDescription')),
          if (isTRUE(rv.process$tl.tags.enabled['Description']))
            actionButton(ns('btn_validate_Description'), 
                         paste0('Start ', rv.process$config$name),
                         class = btn_success_color)
          else
            shinyjs::disabled(
              actionButton(ns('btn_validate_Description'), 
                           paste0('Start ', rv.process$config$name),
                           class = btn_success_color)
            )
        )
      )
    })
    
    observeEvent(input$btn_validate_Description, ignoreInit = T, ignoreNULL=T, {
      InitializeDataIn()
      ValidateCurrentPos()
    })
    
    reactive({dataOut})
    
    
  }
  )
}