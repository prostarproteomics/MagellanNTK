library("shinydashboard")
library("shiny")
library(MagellanNTK)

ui = dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody(
    workflow_ui("PipelineDemo_Process1")
  )
)

server = function(input, output, session) {
  
  path <- system.file('workflow/PipelineDemo', package = 'MagellanNTK')
  data(sub_R25)
  
  workflow_server("PipelineDemo_Process1", 
    path = path,
    dataIn = sub_R25)
}

shinyApp(ui, server)