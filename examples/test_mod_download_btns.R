
if (interactive()){
  library(shinyjqui)
  library(DT)
  library(shinyjs)
  library(shiny)
  options(shiny.fullstacktrace = TRUE)
  data(data_na)
  
  ui <- fluidPage(
    mod_download_btns_ui("dl")
  )
  
  server <- function(input, output, session) {
    
    mod_download_btns_server(
      id = "dl",
      df.data = reactive({data_na$array1}),
      name = reactive({'foo'}),
      extension = reactive({c('Excel', 'csv', 'RData')}),
      widget.type = reactive({c('Link', 'Button', 'Link')})
    )
  }
  
  shinyApp(ui, server)
}
