
library(shiny)

dyn_widgets_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("add_exp")),
    actionButton(ns("add_button"), "Add"),
    actionButton(ns('validate'), "Validate")
  )
}


dyn_widgets_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # reactive value to "collect" all widgets
    widget <- reactiveValues(collection = NULL)
    
    # reactive value to "collect" company inputs
    steps <- reactiveValues(inputs = c(),
                            mandatory = c())
    dataOut <- reactiveVal(list())
    
    observe({
      # update steps inputs
      lapply(1:input$add_button, function(i){
        out = ''
        value = paste0('input$id_', paste0('step_', i))
        if(!is.null(eval(parse(text = value)))) 
          out = eval(parse(text = value))
        steps$inputs[i] <- out
        
        out = ''
        value = paste0("input$idexp_", paste0('step_', i))
        if(!is.null(eval(parse(text = value)))) 
          out = eval(parse(text = value))
        steps$mandatory[i] <- out
      })
    })
    
    
    # click "Add" button to, 
    # 1) track companies entered 
    # 2) update the widget collection
    # 3) update click counts
    observeEvent(input$add_button,{
      # function to create widget
      create_widget = function(i){
        div(style = "display:inline-block; vertical-align: top;",
               textInput(ns(paste0("id_", paste0('step_', i))),
                         label = paste0('Step ', i), 
                         value = steps$inputs[i],
                         placeholder = paste0("Enter the ", paste0('step_', i))),
               
               selectInput(ns(paste0("idexp_", paste0('step_', i))),
                           label = "",
                           selected = steps$mandatory[i],
                           choices = c(TRUE, FALSE),
                           width = '80px')
        )
      }
      
      # update widget collection
      widget$collection <- lapply(1:input$add_button, create_widget)
      
      
      
    })
    
    # render the widget collection
    output$add_exp <- renderUI({
      widget$collection
    })
    
    
    observeEvent(input$validate, {
      dataOut(list(inputs = steps$inputs,
                   mandatory = steps$mandatory))
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

