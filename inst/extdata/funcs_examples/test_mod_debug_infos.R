if (interactive()){
  library(shiny)

ui <- fluidPage(
        Debug_Infos_ui("tbl")
    )
 

server <- function(input, output) {
      data(data_na)
      
      #dataset <- NULL
      dataset <- data_na
      
      
      Debug_Infos_server(
        id = "tbl",
        title = "Infos from shiny app",
        rv.dataIn = reactive({dataset}),
        dataOut = reactive({NULL})
      )
}


  shinyApp(ui, server)
}
