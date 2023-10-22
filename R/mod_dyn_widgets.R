
library(shiny)

dyn_widgets_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      column(width = 4, 
             textInput(ns('step'),
                       label = 'Step ', 
                       placeholder = paste0("Enter the name"))
      ),
      column(width = 3, 
            selectInput(ns('mandatory'), label = "",
                         choices = c(TRUE, FALSE),
                         width = '80px')
    ),
    column(width = 3, actionButton(ns("add_button"), "Add"))
    ),
    uiOutput(ns("add_exp"))
  )
}


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
      dataOut(list(inputs = steps$inputs,
                   mandatory = steps$mandatory))
    })
    
    # render the widget collection
    output$add_exp <- renderUI({
      # function to create widget
      create_widget = function(i){
        p(paste0(steps$inputs[i], ' ', steps$mandatory[i]))
      }
      
      lapply(1:(input$add_button), create_widget)
    })
    

    reactive({dataOut()})
})
}



dyn_widgets <- function(){
  ui <- dyn_widgets_ui('test')
  
  server <- function(input, output, session) {
    res <- reactiveValues(dataOut = list())
    res$dataOut <- dyn_widgets_server('test')
    
    observeEvent(req(length(res$dataOut()) > 0), {
      print(res$dataOut())
    })
  }
  
  shinyApp(ui, server)
  
}

dyn_widgets()

