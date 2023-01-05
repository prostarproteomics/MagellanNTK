if(interactive()){
  library(shiny)
  library(shinyWidgets)
  library(shinyjs)

  options(shiny.fullstacktrace = TRUE)
  

ui <- fluidPage(
  actionButton("prevpos", GlobalSettings$tl_v_prev_icon),
  actionButton("nextpos", GlobalSettings$tl_v_next_icon),
  timeline_v_ui('TLv')
)



server <- function(input, output){
  
  rv <- reactiveValues(
    direction = 0,
    status = c(0, 1, 0, 0),
    current.pos = 1,
    tl.tags.enabled = c(1, 1, 1, 1),
    position = NULL
  )
  
  
  config <- Config(
    fullname = 'PipeA_Process1',
    mode = 'process',
    steps = c('Step 1', 'Step 2'),
    mandatory = c(FALSE, TRUE)
  )
  
  
  
  timeline_v_server(id = 'TLv',
    config = config,
    status = reactive({rv$status}),
    position = reactive({rv$current.pos}),
    enabled = reactive({rv$tl.tags.enabled})
  )
  
  observeEvent(input$nextpos,{
    if (rv$current.pos != length(config@steps))
      rv$current.pos <- rv$current.pos + 1
  })
  
  observeEvent(input$prevpos,{
    if (rv$current.pos != 1)
      rv$current.pos <- rv$current.pos - 1
  })
}


shinyApp(ui, server)
}