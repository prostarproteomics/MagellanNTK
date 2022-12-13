if(interactive()){
  library(shiny)
library(shinyBS)


options(shiny.fullstacktrace = TRUE)

#### test modal ####
ui <- fluidPage(
   mod_Save_Dataset_ui(id = 'example')
)


server <- function(input, output, session) {

  data(data1)
  observe({
    mod_Save_Dataset_server(id = 'example', data = reactive({data1}))
  })

}

shinyApp(ui, server)
}