library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(QFeatures)
library(tibble)

options(shiny.fullstacktrace = T)

source(file.path('.', 'mod_Protein.R'), local=TRUE)$value

verbose <- F

redBtnClass <- "btn-danger"
PrevNextBtnClass <- "btn-info"
btn_success_color <- "btn-success"
optionsBtnClass <- "info"

btn_style <- "display:inline-block; vertical-align: middle; padding: 7px"

AddItemToDataset <- function(dataset, name){
  addAssay(dataset, 
           dataset[[length(dataset)]], 
           name=name)
}


ui <- fluidPage(
  mod_Protein_ui('Protein')
)


server <- function(input, output){
  utils::data(Exp1_R25_prot, package='DAPARdata2')
  
  obj <- NULL
  obj <- Exp1_R25_prot
  
  rv <- reactiveValues(
    res = NULL
  )
  
  observe({
    rv$res <- mod_Protein_server('Protein', 
                                     dataIn = reactive({obj}),
                                     tag.enabled = reactive({TRUE}) )
  })
  
}


shinyApp(ui, server)
