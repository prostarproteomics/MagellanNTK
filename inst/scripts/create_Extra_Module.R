#' @title ## To be customized ##
#' 
#' @description 
#' ## To be customized ##
#' 
#' @param id xxx
#' 
#' @name extra_module
#'
#'
#' @examplesIf interactive() 
#' data(ft_na)
#' ui <- foo_ui('query')
#' 
#' server <- function(input, output, session) {
#'   
#'   rv <- reactiveValues(
#'     res = NULL
#'   )
#'   ll.tags <- c('None' = 'None', 
#'   qMetadata.def(typeDataset(ft_na[[1]]))$node)
#'   
#'   rv$res <- foo_server('query', 
#'   reset = reactive({NULL}),
#'   is.enabled = reactive({NULL})
#'   )
#'   
#'   observeEvent(rv$res$dataOut()$value, ignoreNULL = TRUE, ignoreInit = TRUE, {
#'     print(rv$res$dataOut()$value)
#'     print(rv$res$dataOut()$widgets)
#'   })
#' 
#' shinyApp(ui=ui, server=server)
#' }
NULL



#' @export
#' @rdname extra_module
#' 
NAME_ui <- function(id){

ns <- NS(id)

# This is an example of what it is expected
# You must 
# All the widgets must be defined in the server part. Here, 
# one have only calls with uiOutput() function.

# Each line correspond to one widget and the id is composed of
# the name of the widget followed by '_ui'
tagList(
  #uiOutput(ns('widget1_ui')),
  #uiOutput(ns('widget2_ui')),
  #uiOutput(ns('widget3_ui')),
  uiOutput(ns('valid_btn_ui'))
)
}


#' @export
#' @rdname extra_module
#' 
NAME_server <- function(id,
    obj,
    remoteReset = reactive({NULL}),
    is.enabled = reactive({TRUE})
    ) {
  
  # Define default selected values for widgets
  # This is only for simple workflows
  # This list must contain one item per widget (defined in the ui() function
  # The name of each item is the same as in the ui minus the suffix '_ui')
  widgets.default.values <- list(
    #widget1 = NULL,
    # widget2 = NULL,
    # widget3 = NULL
  )
  
  
  rv.custom.default.values <- list()
  
  moduleServer(id,function(input, output, session) {
    ns <- session$ns
    
    # DO NOT MODIFY THIS FUNCTION CALL
    eval(
      str2expression(
        Get_AdditionalModule_Core_Code(
          w.names = names(widgets.default.values),
          rv.custom.names = names(rv.custom.default.values)
        )
      )
    )
    
    
    output$widget1_ui <- renderUI({
      widget <- selectInput(ns('widget1'), 'widget1', choices = 1:3)
      MagellanNTK::toggleWidget(widget, is.enabled())
    })
    
    output$widget2_ui <- renderUI({
      widget <- selectInput(ns('widget2'), 'widget2', choices = 1:3)
      MagellanNTK::toggleWidget(widget, is.enabled())
    })
    
    output$widget3_ui <- renderUI({
      widget <- selectInput(ns('widget3'), 'widget3', choices = 1:3)
      MagellanNTK::toggleWidget(widget, is.enabled())
    })
    
    
    output$valid_btn_ui <- renderUI({
      widget <- actionButton(ns('valid_btn'), 'Validate')
      MagellanNTK::toggleWidget(widget, is.enabled())
    })
    
    
    observeEvent(input$valid_btn, {
      dataOut$trigger <- as.numeric(Sys.time())
      dataOut$value <- obj()
      dataOut$widgets <- reactiveValuesToList(rv.widgets)
    })
    
    
    return(reactive({dataOut}))
  })
  
}
