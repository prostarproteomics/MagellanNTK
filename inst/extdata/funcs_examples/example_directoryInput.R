# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

ui <- shiny::shinyUI(fluidPage(
  fluidRow(
    column(1),
    column(
      width = 10,
      
      # Application title
      titlePanel("Directory Input Demo"),
      directoryInput('directory', label = 'Select directory', value = '~', width='600px'),
      tags$h5('Files'),
      dataTableOutput('files')
    ),
    column(1)
  )
))


# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
library(shiny)

server <- shinyServer(function(input, output, session) {
  
  session$onSessionEnded(function(){
    stopApp()
  })
  
  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {
      input$directory
    },
    handlerExpr = {
      if (input$directory > 0) {
        # condition prevents handler execution on initial app launch
        path = choose.dir(default = readDirectoryInput(session, 'directory', width='600px'),
                          caption="Choose a directory...")
        updateDirectoryInput(session, 'directory', value = path)
      }
    }
  )
  
  output$directory = renderText({
    readDirectoryInput(session, 'directory')
  })
  
  output$files = renderDataTable({
    files = list.files(readDirectoryInput(session, 'directory'), full.names = TRUE)
    data.frame(name = basename(files), file.info(files))
  })
  
})


if(interactive())
  app <- shinyApp(ui, server)
