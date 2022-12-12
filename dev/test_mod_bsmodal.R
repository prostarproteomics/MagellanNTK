library(shiny)
library(shinyjqui)
library(shinyBS)
library(shinyjs)

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
 
    mod_bsmodal_server('exemple',
                     title = 'Plots',
                     width="75%", # en px ou % de largeur
                     uiContent = list(title = 'Title',
                                      content = 'Content')
  )
}

shinyApp(ui=ui, server=server)
