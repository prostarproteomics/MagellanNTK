library(highcharter)
library(shinyjs)
library(DT)
library(tibble)
library(QFeatures)


options(shiny.fullstacktrace = TRUE)


dirpath <- system.file('module_examples', package='Magellan')
for (l in list.files(path = dirpath, pattern = ".R"))
  source(file.path(dirpath, l), local=TRUE)$value
#--------------------------------------------


mod_test_process_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns('UI')),
    wellPanel(title = 'foo',
              tagList(
                h3('Valler'),
                uiOutput(ns('show_Debug_Infos'))
              )
    )
  )
}


mod_test_process_server <- function(id){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    rv <- reactiveValues(
      dataIn = Build_example_dataset(),
      dataOut = NULL
    )
    
    observe({
      rv$dataOut <- mod_nav_server(id = 'PipelineA_Description',
                                           dataIn = reactive({rv$dataIn})
                                           )
    }, priority=1000)
    
    
    output$UI <- renderUI({
      mod_nav_ui(ns('PipelineA_Description'))
    })
    
    
    
    #--------------------------------------------------------------------
    
    output$show_Debug_Infos <- renderUI({
      fluidRow(
        column(width=2,
               tags$b(h4(style = 'color: blue;', "Data In")),
               uiOutput(ns('show_rv_dataIn'))),
        column(width=2,
               tags$b(h4(style = 'color: blue;', "Data Out")),
               uiOutput(ns('show_rv_dataOut')))
      )
    })
    
    ###########---------------------------#################
    output$show_rv_dataIn <- renderUI({
      req(rv$dataIn)
      tagList(
        lapply(names(rv$dataIn), function(x){tags$p(x)})
      )
    })
    
    output$show_rv_dataOut <- renderUI({
       tagList(
        lapply(names(rv$dataOut$dataOut()$value), function(x){tags$p(x)})
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


# shinyApp(ui = mod_test_process_ui('test_mod_process'),
#          server = mod_test_process_server('test_mod_process'))
