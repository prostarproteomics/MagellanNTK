library(shiny)
library(shinydashboard)

library(MagellanNTK)
options(shiny.fullstacktrace = TRUE)

#------------------------ Class TimelineDraw --------------------------------------
source(file.path('../../../R', 'class_TimelineDraw.R'), local=TRUE)$value
source(file.path('../../../R', 'global.R'), local=TRUE)$value
source(file.path('../../../R', 'class_ScreenManager.R'), local=TRUE)$value
source(file.path('../../../R', 'class_Process.R'), local=TRUE)$value
source(file.path('.', 'Example_ProcessA.R'), local=TRUE)$value


ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    
    sidebarMenu(
      id = "tabs",
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
      )
    )
  )
)

server <- function(session, input, output) {
  
  
  observeEvent(input$send, {
    shinyjs::toggleState('select1', condition = TRUE )
    shinyjs::toggleState('tutu', condition = TRUE )
    
  })
  
  output$magellan <- renderUI({
    req(rv$pipe)
    shinyjs::disabled(rv$pipe$ui())
  })
  
  #------------------------------------------------
  rv <- reactiveValues(
    dataIn = NULL,
    res = NULL,
    pipe = NULL
  )
  rv$pipe <- Example_ProcessA$new('App')
  
  observe({
    rv$res <- rv$pipe$server(dataIn = reactive({rv$dataIn}))
  })
  
  
  observeEvent(input$send, {
    updateTabItems(session, 'tabs', 'screen2')
    shinyjs::delay(100, rv$dataIn <- data1)
    
  })
  
  output$show_pipe <- renderUI({
    req(rv$pipe)
    rv$pipe$ui()
  })
  
  
  
}

shinyApp(ui, server)