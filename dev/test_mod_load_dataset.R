library(shiny)
library(shinyBS)

source(file.path("../../R", "mod_Load_Dataset.R"), local=TRUE)$value


options(shiny.fullstacktrace = TRUE)

#### test modal ####
ui <- fluidPage(
 # mod_Load_Dataset_ui('exemple')
  actionButton('show', 'Show')
)


server <- function(input, output, session) {
  
  
  observeEvent(input$show, {
    data <- mod_Load_Dataset_server('exemple')
  })
  
  observeEvent(req(data()), {print(data())})
}

shinyApp(ui=ui, server=server)
