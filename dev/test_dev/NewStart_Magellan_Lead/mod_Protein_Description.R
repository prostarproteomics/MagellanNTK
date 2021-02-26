
btn_style <- "display:inline-block; vertical-align: middle; padding: 7px"

mod_Protein_Description_ui <- function(id){
  ns <- NS(id)
}



mod_Protein_Description_server <- function(id,
                                           dataIn = NULL,
                                           steps.enabled = reactive({NULL}),
                                           reset = reactive({FALSE}),
                                           status = reactive({NULL})
                                           ){
  
  #' @field global xxxx
  global <- list(
    VALIDATED = 1,
    UNDONE = 0,
    SKIPPED = -1
  )
  
  config <- list(
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
    
    rv.widgets <- reactiveValues()
    
    rv <- reactiveValues(
      dataIn = NULL,
      dataOut = NULL,
      status = NULL,
      reset = NULL,
      steps.enabled = NULL
    )
    
    observeEvent(status(), { rv$status <- status()})
    # Initialization of the module
    observeEvent(steps.enabled(), ignoreNULL = TRUE, {
      if (is.null(steps.enabled()))
        rv$steps.enabled <- setNames(rep(FALSE, rv.process$length), rv.process$config$steps)
      else
        rv$steps.enabled <- steps.enabled()
    })
    
    
    observeEvent(reset(), {
      lapply(names(rv.widgets), function(x){
        rv.widgets[[x]] <- widgets.default.values[[x]]
      })
    })
    
    ### ----------------------------------------------------------------------------------------------------
    
    ###### ------------------- Code for Description (step 0) -------------------------    #####
    output$Description <- renderUI({
      rv$steps.enabled
      tagList(
          includeMarkdown( system.file("app/md", paste0(config$name, ".md"), package="Magellan")),
          uiOutput(ns('datasetDescription')),
          if (isTRUE(rv$steps.enabled['Description']))
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
      print('tutu')
      rv$dataIn <- dataIn()
      rv$dataOut <- rv$dataIn
      rv$status['Description'] <- global$VALIDATED
    })
    
    list(config = reactive({
                  config$ll.UI <- setNames(lapply(config$steps,
                                      function(x){
                                        do.call('uiOutput', list(ns(x)))
                                      }),
                               paste0('screen_', config$steps)
                              )
                  config
                  }),
        dataOut = reactive({rv$dataOut}),
        status = reactive({rv$status})
    )
    
    
  }
  )
}