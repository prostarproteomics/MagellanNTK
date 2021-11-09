library(shiny)
library(shinyjs)
library(crayon)


options(shiny.fullstacktrace = TRUE)

setwd('~/GitHub/Magellan/inst/scripts')

source(file.path('./module_examples', "mod_PipelineA.R"), 
       local=TRUE)$value
source(file.path('./module_examples', "mod_PipelineA_Description.R"), 
       local=TRUE)$value

for (i in 1:3)
  source(file.path('./module_examples', 
                   paste0("mod_PipelineA_Process", i, ".R")), 
         local=TRUE)$value

dirpath <- '../../R'
for (l in list.files(path = dirpath, pattern = ".R", recursive = TRUE))
  source(file.path(dirpath, l), local=TRUE)$value


#----------------------------------------------------------------------
ui <- fluidPage(
  tagList(
    uiOutput('UI'),
    
    # Just for example purpose
    wellPanel(tagList(
                h3('Caller function'),
                uiOutput('show_Debug_Infos')
              )
    )
  )
)

#----------------------------------------------------------------------
server <- function(input, output){
  
  data(feat1, package='Magellan')
  rv <- reactiveValues(
    dataIn = feat1,
    dataOut = NULL
  )
  
  observe({
    # rv$dataOut <- mod_navigation_server(id = 'PipelineA',
    #                                     nav.mode = 'pipeline',
    #                                     dataIn = reactive({rv$dataIn})
    #                                     )
    rv$dataOut <- mod_nav_pipeline_server(id = 'PipelineA',
                                          dataIn = reactive({rv$dataIn}),
                                          is.enabled = reactive({TRUE}),
                                          remoteReset = reactive({FALSE})
    )
    
    output$UI <- renderUI({
      mod_navigation_ui('PipelineA')
      mod_nav_pipeline_ui('PipelineA')
    })
  }, priority=1000)
  
  
  
  #--------------------------------------------------------------------
  
  output$show_Debug_Infos <- renderUI({
    fluidRow(
      column(width=2,
             tags$b(h4(style = 'color: blue;', "Data In")),
             uiOutput('show_rv_dataIn')
      ),
      column(width=2,
             tags$b(h4(style = 'color: blue;', "Data Out")),
             uiOutput('show_rv_dataOut')
      )
    )
  })
  
  ###########---------------------------#################
  output$show_rv_dataIn <- renderUI({
    lapply(names(rv$dataIn), function(x){tags$p(x)})
  })
  
  output$show_rv_dataOut <- renderUI({
    lapply(names(rv$dataOut$dataOut()$value), function(x){tags$p(x)})
  })
  
}



shinyApp(ui, server)

