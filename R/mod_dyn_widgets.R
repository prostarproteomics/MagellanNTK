#' @title Create dynamic widgets
#'
#' @description  A shiny Module to create a dynamic number of widgets.
#' 
#' @param id xxx
#' 
#' @name DynamicWidgets
#' 
NULL


#' @rdname DynamicWidgets
#' @import shiny
#' @export
dyn_widgets_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    p('Add steps to the workflow'),
    fluidRow(
      column(width = 3, align='middle', 
             textInput(ns('step'), label = '', placeholder = paste0("Enter the name"))),
      column(width = 3, align='center',
             selectInput(ns('mandatory'), label = "", choices = c(TRUE, FALSE), width = '80px')),
             column(width = 3, align='center',
                    actionButton(ns("add_button"), "Add", class = "btn-info"))
    )
  )
}

#' @rdname DynamicWidgets
#' @import shiny
#' @export
dyn_widgets_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # reactive value to "collect" company inputs
    steps <- reactiveValues(inputs = c(),
                            mandatory = c())
    
    
    dataOut <- reactiveVal(list())
    
    observeEvent(input$add_button,{
      steps$inputs[input$add_button] <- input$step
      steps$mandatory[input$add_button] <- input$mandatory
      updateTextInput(session, 'step', value = '')
      updateSelectInput(session, 'mandatory', selected = 'TRUE')
      #dataOut(list(inputs = steps$inputs,
      #             mandatory = steps$mandatory))
    })


    reactive({list(steps = steps$inputs,
                   mandatory = steps$mandatory)
      })
})
}


#' @rdname DynamicWidgets
#' @import shiny
#' @export
dyn_widgets <- function(){
  ui <- shinyUI(
    dyn_widgets_ui('test')
  )
  
  server <- function(input, output, session) {
    res <- dyn_widgets_server('test')
    
    observeEvent(res(), {
      print(res()$steps)
      print(res()$mandatory)
    })
  }
  
  app <- shinyApp(ui, server)
}


