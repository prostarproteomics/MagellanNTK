
options(shiny.fullstacktrace = T)
rv <- reactiveValues(
  toto = 1
)

source(file.path('.', 'mod_A.R'), local=FALSE)$value




mod_test_process_ui <- function(id){
  ns <- NS(id)
  tagList(
    mod_Pipeline_ui(ns('pipe')),
    uiOutput(ns('showInfos'))
  )
}


mod_test_process_server <- function(id){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
     
    dataOut <- reactiveValues(
      value = NULL,
      trigger = NULL,
      name = NULL
    )

    observe({
       mod_Pipeline_server(id = 'pipe', dataOut = dataOut)
      
       observeEvent(dataOut$trigger, {
      #   print('totototo')
      #   print(names(rv$dataOut$dataOut()$value))
         #browser()
       })
      
    }, priority=1000)
    
    
    output$showInfos <- renderUI({
      tagList(
        h3('trigger = ', dataOut$trigger),
        h3('name = ', dataOut$name),
        h3('value = ', dataOut$value)
      )
    })
    
  })
}



#----------------------------------------------------------------------
ui <- fluidPage(
  mod_test_process_ui('test_mod_process')
)


#----------------------------------------------------------------------
server <- function(input, output){
  mod_test_process_server('test_mod_process')
}


shinyApp(ui, server)
