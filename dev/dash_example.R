library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(tibble)

options(shiny.fullstacktrace = TRUE)


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
    div(id='tutu',
        absolutePanel('toto', bsmodal_ui("tbl"))
        ),
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
                selectInput('select1', 'Select 1', choices = seq_len(3))
              )
      ),
      tabItem(tabName = "screen3",
              tagList(
                h3('test')
              )
      )
    )
  )
)

server <- function(input, output) {
  
  bsmodal_server(
    id = "tbl",
    title = "test",
    uiContent = p("test")
  )
  
  
}

shinyApp(ui, server)