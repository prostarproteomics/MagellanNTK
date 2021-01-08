library(shiny)
library(shinyjqui)
library(shinyBS)
library(DAPAR2)

source(file.path("../../R", "mod_bsmodal.R"), local=TRUE)$value


library(highcharter)
library(DT)
library(shinyjs)
library(MSPipelines)

source(file.path("../../R", "mod_popover_for_help.R"), local = TRUE)$value
source(file.path("../../R", "global.R"), local = TRUE)$value
source(file.path("../../R", "mod_format_DT.R"), local = TRUE)$value

#### test modal ####
ui <- fluidPage(
  mod_bsmodal_ui('exemple')
)


server <- function(input, output, session) {
  
  #datasets <- utils::data(package="DAPARdata2")$results[,"Item"]
  utils::data(Exp1_R25_prot, package='DAPARdata2')
  
  
  mod_all_plots_server('exemple_plot',
                       dataIn = reactive({Exp1_R25_prot}),
                       indice = reactive({length(Exp1_R25_prot)})
                       ) 
  
  mod_UI <- mod_all_plots_ui('exemple_plot')
  title <- "Plots"
  
  # module d'affichage modal contenant ci-dessus
  mod_bsmodal_server('exemple',
                     title = title,
                     mod_UI = mod_UI,
                     width="75%" # en px ou % de largeur
  )
}

shinyApp(ui=ui, server=server)
