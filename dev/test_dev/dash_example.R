library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(QFeatures)
library(tibble)
library(DaparToolshed)

options(shiny.fullstacktrace = TRUE)


source(file.path('../../R', 'mod_timeline_h.R'), local=TRUE)$value
source(file.path('../../R', 'mod_timeline_v.R'), local=TRUE)$value
source(file.path('../../R', 'mod_nav_process.R'), local=TRUE)$value
source(file.path('../../R', 'mod_nav_pipeline.R'), local=TRUE)$value


ui <- dashboardPage(
  dashboardHeader(),
    dashboardSidebar(
      
      sidebarMenu(
        menuItem("Screen1", tabName = "screen1", icon = icon("th")),
        menuItem("Screen2", tabName = "screen2", icon = icon("th"))
      )
  ),
  dashboardBody(
    shinyjs::useShinyjs(),
    
    tabItems(
      # First tab content
      tabItem(tabName = "screen1",
              fluidRow(
                actionButton('send', 'Send')
              )
      ),
      
      # Second tab content
      tabItem(tabName = "screen2",
              tagList(
                shinyjs::disabled(
                selectInput('select1', 'Select 1', choices = seq_len(3))),
                uiOutput('magellan')
              )
      ),
      tabItem(tabName = "screen3",
              tagList(
                mod_nav_pipeline_ui('Protein')
              )
      )
    )
  )
)

server <- function(input, output) {
  
  obj <- NULL
  obj <- Exp1_R25_prot
  
  
  rv <- reactiveValues(
    res = NULL
  )
  
  observe({
    rv$res <- mod_nav_pipeline_server(id = 'Protein', 
                                      dataIn = reactive({obj})
    )
  })
  
  
  observeEvent(input$send, {
    shinyjs::toggleState('select1', condition = TRUE )
    shinyjs::toggleState('tutu', condition = TRUE )
    
  })
  
  output$magellan <- renderUI({
    if(input$send)
      selectInput('select2', 'Select 2', choices = seq_len(3))
    else
      shinyjs::disabled(selectInput('select2', 'Select 2', choices = seq_len(3)))
  })
}

shinyApp(ui, server)