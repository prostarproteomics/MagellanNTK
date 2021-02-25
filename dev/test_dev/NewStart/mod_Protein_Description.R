
btn_style <- "display:inline-block; vertical-align: middle; padding: 7px"
#source(file.path('.', 'mod_timeline_h.R'), local=TRUE)$value
#source(file.path('.', 'mod_timeline_v.R'), local=TRUE)$value

mod_Protein_Description_ui <- function(id){
  ns <- NS(id)
  #uiOutput(ns('ui'))
  mod_nav_process_ui(ns('tutu'))
}



mod_Protein_Description_server <- function(id,
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
    name = 'Protein_Description',
    steps = c('Description'),
    mandatory = c(T)
  )
  
  # Define default selected values for widgets
  widgets.default.values <- list()
  
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
      config$ll.UI <- setNames(lapply(config$steps,
                                      function(x){
                                        do.call('uiOutput', list(ns(x)))
                                      }),
                               paste0('screen_', config$steps)
      )
      
      rv.nav$return <- mod_nav_process_server('tutu',
                                              config = reactive({config}),
                                              status = reactive({rv.nav$status}),
                                              dataIn = reactive({rv.nav$dataIn}),
                                              is.enabled = reactive({is.enabled()})
      )
    }, priority=1000) 
    
    observeEvent(rv.nav$return$status(), {rv.nav$status <- rv.nav$return$status()})
    observeEvent(rv.nav$return$dataOut()$trigger, {rv.nav$dataIn <- rv.nav$return$dataOut()$value})
    
    observeEvent(rv.nav$return$reset(), {
      lapply(names(rv.widgets), function(x){
        rv.widgets[[x]] <- widgets.default.values[[x]]
      })
    })
    
    observeEvent(dataIn(), {rv.nav$temp.dataIn <- dataIn()})
    
    
    ### ----------------------------------------------------------------------------------------------------
    
    ###### ------------------- Code for Description (step 0) -------------------------    #####
    output$Description <- renderUI({
      rv.nav$return$steps.enabled()
      
      
        tagList(
          includeMarkdown( system.file("app/md", paste0(config$name, ".md"), package="Magellan")),
          uiOutput(ns('datasetDescription')),
          if (isTRUE(rv.nav$return$steps.enabled()['Description']))
            actionButton(ns('btn_validate_Description'), 
                         paste0('Start ', config$name),
                         class = btn_success_color)
          else
            shinyjs::disabled(
              actionButton(ns('btn_validate_Description'), 
                           paste0('Start ', config$name),
                           class = btn_success_color)
            )
        )
    })
    
    observeEvent(input$btn_validate_Description, ignoreInit = T, ignoreNULL=T, {
      rv.nav$dataIn <- rv.nav$temp.dataIn
      rv.nav$status['Description'] <- global$VALIDATED
    })
    
    reactive({rv.nav$return$dataOut()})
    
    
  }
  )
}