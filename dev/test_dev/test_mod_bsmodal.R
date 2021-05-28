library(shiny)
library(shinyjqui)
library(shinyBS)
library(DAPAR2)

library(shinyjs)
library(MSPipelines)

source(file.path("../../R", "mod_popover_for_help.R"), local = TRUE)$value
source(file.path("../../R", "global.R"), local = TRUE)$value
source(file.path("../../R", "mod_format_DT.R"), local = TRUE)$value
source(file.path("../../R", "mod_bsmodal.R"), local=TRUE)$value


options(shiny.fullstacktrace = TRUE)

#### test modal ####
ui <- fluidPage(
  mod_bsmodal_ui('exemple')
)


server <- function(input, output, session) {
 
  utils::data(Exp1_R25_prot, package='DAPARdata2')
  
  MSPipelines::mod_all_plots_server('plots',
                                    dataIn = reactive({Exp1_R25_prot})
  )
  mod_bsmodal_server('exemple',
                     title = 'Plots',
                     width="75%", # en px ou % de largeur
                     uiContent = MSPipelines::mod_all_plots_ui('plots')
  )
}

shinyApp(ui=ui, server=server)
