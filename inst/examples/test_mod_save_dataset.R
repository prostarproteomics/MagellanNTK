library(shiny)
library(shinyBS)
library(Magellan)


options(shiny.fullstacktrace = TRUE)

#### test modal ####
ui <- fluidPage(
  #actionButton('show', 'Show'),
  mod_Save_Dataset_ui(id = 'exemple')
)


server <- function(input, output, session) {

  observe({
    mod_Save_Dataset_server(id = 'exemple', 
                        data = reactive({feat1}))
    
  })
  
  
  
}

shinyApp(ui=ui, server=server)
