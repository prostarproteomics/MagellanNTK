
mod_A_ui <- function(id){
  ns <- NS(id)
  uiOutput(ns('btn'))
}

mod_A_server <- function(id, dataOut){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
    output$btn <- renderUI({
      actionButton(ns(paste0('btn_A')), paste0('Button ', id))
    })
    
    
    observeEvent(input$btn_A, {
     # browser()
      dataOut$trigger <- as.numeric(Sys.time())
      dataOut$value <- input$btn_A
      dataOut$name <- paste0(id, ' - ', input$btn_A)
    })

  })
}



mod_Process_ui <- function(id){
  ns <- NS(id)
  uiOutput(ns('show'))
}

mod_Process_server <- function(id, dataOut){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
    observe({
      mod_A_server(id, dataOut = dataOut)
    })

    output$show <- renderUI({
      mod_A_ui(ns(id))
    })
    
    
    observeEvent(dataOut$trigger, {
       #browser()
      dataOut$trigger <- dataOut$trigger
      dataOut$value <- dataOut$value
      dataOut$name <- dataOut$name
    })
    
  })
}





mod_Pipeline_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns('ui'))
  )
}

mod_Pipeline_server <- function(id, dataOut){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
    observe({
      mod_Process_server(paste0(id, '_1'), dataOut = dataOut)
      mod_Process_server(paste0(id, '_2'), dataOut = dataOut)
      mod_Process_server(paste0(id, '_3'), dataOut = dataOut)
    })
    
    
    output$ui <- renderUI({
      tagList(
        mod_Process_ui(ns(paste0(id, '_1'))),
        mod_Process_ui(ns(paste0(id, '_2'))),
        mod_Process_ui(ns(paste0(id, '_3')))
      )
    })

    
    observeEvent(dataOut$trigger, {
      #browser()
      dataOut$trigger <- dataOut$trigger
      dataOut$value <- dataOut$value
      dataOut$name <- dataOut$name
    })
    
  })
}