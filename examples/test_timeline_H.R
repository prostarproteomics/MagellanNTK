if(interactive()){
  library(shiny)
library(shinyWidgets)
library(shinyjs)

options(shiny.fullstacktrace = TRUE)


config <- Config(
  fullname = 'PipelineA_test',
  mode = 'process',
  steps = c('Step 1', 'Step 2', 'Step 3'),
  mandatory = c(TRUE, FALSE, FALSE)
)



ui <- fluidPage(
  actionButton("prevpos", GlobalSettings$tl_h_prev_icon),
  actionButton("nextpos", GlobalSettings$tl_h_next_icon),
  timeline_h_ui('TLh')
  )



server <- function(input, output){
  
  rv <- reactiveValues(
    direction = 0,
    status = c(0, 1, 0, 0),
    current.pos = 1,
    tl.tags.enabled = c(1, 1, 1, 1),
    position = NULL
  )
  
  
  
  
  observeEvent(input$nextpos,{
    if (rv$current.pos != length(config@steps))
      rv$current.pos <- rv$current.pos + 1
  })
  
  observeEvent(input$prevpos,{
    if (rv$current.pos != 1)
      rv$current.pos <- rv$current.pos - 1
  })
  

  timeline_h_server(id = 'TLh',
    config = config,
    status = reactive({rv$status}),
    position = reactive({rv$current.pos}),
    enabled = reactive({rv$tl.tags.enabled})
  )
  
}


shinyApp(ui, server)
#
