library(shiny)
library(shinyjqui)
library(shinyBS)
library(DAPAR2)



library(highcharter)
library(DT)
library(shinyjs)
library(MSPipelines)

source(file.path("../../R", "mod_popover_for_help.R"), local = TRUE)$value
source(file.path("../../R", "global.R"), local = TRUE)$value
source(file.path("../../R", "mod_format_DT.R"), local = TRUE)$value
source(file.path("../../R", "mod_bsmodal.R"), local=TRUE)$value

#### test modal ####
ui <- fluidPage(
  mod_bsmodal_ui('exemple')
)


server <- function(input, output, session) {
 
  utils::data(Exp1_R25_prot, package='DAPARdata2')
  
  mod_bsmodal_server('exemple',
                     dataIn = reactive({Exp1_R25_prot}),
                     title = title,
                     width="75%" # en px ou % de largeur
  )
}

shinyApp(ui=ui, server=server)
