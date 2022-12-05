library(shiny)
library(shinyWidgets)
library(shinyjs)
library(R6)
library(tibble)

options(shiny.fullstacktrace = TRUE)
btn_style <- "display:inline-block; vertical-align: middle; padding: 7px"

#------------------------ Class TimelineDraw --------------------------------------
#source(file.path('.', 'mod_timeline_h.R'), local=TRUE)$value

ui <- fluidPage(
  actionButton("prevpos", "<<"),
  actionButton("nextpos", ">>"),
  mod_timeline_v_ui('TLv')
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
    name = 'Process1',
    mode = 'process',
    steps = c('Description', 'Step 1', 'Step 2', 'Save'),
    mandatory = c(TRUE, FALSE, TRUE, TRUE),
    path_to_md_dir = system.file('module_examples/md/', package='Magellan')
  )
  
  
  
  mod_timeline_v_server(id = 'TLv',
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
