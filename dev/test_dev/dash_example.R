library(shiny)
library(shinydashboard)

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
      )
    )
  )
)

server <- function(input, output) {
  
  
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