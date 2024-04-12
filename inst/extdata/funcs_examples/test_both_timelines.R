#if(interactive()){
  library(shiny)
library(shinyWidgets)
library(shinyjs)
library(MagellanNTK)

options(shiny.fullstacktrace = TRUE)


ui <- fillPage(
  fluidRow(
    column(width=2, 
           actionButton("prevpos_v", icon("arrow-up")),
           actionButton("nextpos_v", icon("arrow-down")),
           timeline_v_ui('TLv')
           ),
    column(width=10,
           style=" padding-left: 60px;",
           tagList(
             timeline_h_ui('TLh'),
             actionButton("prevpos_h", icon("arrow-left")),
             actionButton("nextpos_h", icon("arrow-right"))
             )
    )

  )
)


server <- function(input, output){
  
  rv_h <- reactiveValues(
    status = c(0, 1, 0, 0),
    current.pos = 1,
    tl.tags.enabled = c(1, 1, 0, 1),
    position = NULL
  )
  
  config_h <- config_v <- Config(
    fullname = 'Process1',
    mode = 'process',
    steps = c('Step 1', 'Step 2'),
    mandatory = c(TRUE, TRUE),
    steps.source.file = system.file('workflow/PipelineA/R/PipelineA_Process1.R', 
                                    package='MagellanNTK')
  )

  rv_v <- reactiveValues(
    status = c(0, 1, 0, 0),
    current.pos = 1,
    tl.tags.enabled = c(1, 1, 0, 1),
    position = NULL
  )
  
  observeEvent(input$nextpos_h,{
    if (rv_h$current.pos != length(config_h@steps))
      rv_h$current.pos <- rv_h$current.pos + 1
  })
  
  observeEvent(input$prevpos_h,{
    if (rv_h$current.pos != 1)
      rv_h$current.pos <- rv_h$current.pos - 1
  })
  
  observeEvent(input$nextpos_v,{
    if (rv_v$current.pos != length(config_v@steps))
      rv_v$current.pos <- rv_v$current.pos + 1
  })
  
  observeEvent(input$prevpos_v,{
    if (rv_v$current.pos != 1)
      rv_v$current.pos <- rv_v$current.pos - 1
  })
  
  
  timeline_v_server(id = 'TLv',
    config = config_v,
    status = reactive({rv_v$status}),
    position = reactive({rv_v$current.pos}),
    enabled = reactive({rv_v$tl.tags.enabled})
    )
  
  timeline_h_server(id = 'TLh',
    config = config_h,
    status = reactive({rv_h$status}),
    position = reactive({rv_h$current.pos}),
    enabled = reactive({rv_h$tl.tags.enabled})
    )

}


shinyApp(ui, server)

#}