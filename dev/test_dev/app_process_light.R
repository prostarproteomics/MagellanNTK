library(shiny)
library(R6)
library(tibble)
library(MSPipelines)
options(shiny.fullstacktrace = T)

#------------------------ Class TimelineDraw --------------------------------------
source(file.path('../../R', 'class_TimelineDraw.R'), local=TRUE)$value
source(file.path('../../R', 'global.R'), local=TRUE)$value
source(file.path('../../R', 'class_ScreenManager.R'), local=TRUE)$value
source(file.path('../../R', 'class_Process.R'), local=TRUE)$value
source(file.path("../../R", "mod_popover_for_help.R"), local = TRUE)$value
source(file.path("../../R", "mod_format_DT.R"), local = TRUE)$value
source(file.path("../../R", "mod_bsmodal.R"), local=TRUE)$value

source(file.path('.', 'Example_ProcessA.R'), local=TRUE)$value
source(file.path('.', 'Example_ProcessB.R'), local=TRUE)$value
source(file.path('.', 'Example_Description.R'), local=TRUE)$value


#----------------------------------------------------------------------------

# 
# 
# 
# 
# Pipeline <- R6Class(
#   "Pipeline",
#   public = list(
#     id = NULL,
#     ns = NULL,
#     tmp.return = reactiveValues(),
#     rv = reactiveValues(dataIn = NULL),
#     child.process = list(
#       #Example_Description = NULL,
#       Example_ProcessA = NULL
#       #Example_ProcessB = NULL
#     ),
#     initialize = function(id){
#       self$id <- id
#       self$ns <- NS(id)
#       self$child.process <- setNames(lapply(names(self$child.process),
#                                             function(x){
#                                               assign(x, base::get(x))$new(self$ns(x))
#                                             }),
#                                      names(self$child.process)
#       )
#     },
#     
#     
#     Launch_Servers = function(data){
#       lapply(names(self$child.process), function(x){
#         self$tmp.return[[x]] <- self$child.process[[x]]$server(dataIn = reactive({data()}))
#       })
#     },
#     
#     ui = function() {
#       tagList(
#         lapply(names(self$child.process), function(x){
#           wellPanel(h3(x), self$child.process[[x]]$ui())
#         })
#       )
#     },
#     server = function(dataIn ) {
#       
#       self$Launch_Servers(data = reactive({dataIn()}))
#       
#       moduleServer(self$id, function(input, output, session) {
#         
#         output$show_ui <- renderUI({
#           tagList(
#             lapply(names(self$child.process), function(x){
#               wellPanel(h3(x), self$child.process[[x]]$ui())
#             })
#           )
#         })
#         
#         
#         
#       })
#     }
#   )
# )

rv <- reactiveValues()
proc <- Example_ProcessA$new('App', orientation='h')
ui = fluidPage(
  tagList(
    actionButton('send', 'Send dataset'),
    actionButton('updateStatus', 'Update status'),
    mod_bsmodal_ui('exemple'),
    proc$ui()
  )
)




server = function(input, output){
  # Get a QFeatures dataset for example
  basename(f <- msdata::quant(pattern = "cptac", full.names = TRUE))
  i <- grep("Intensity\\.", names(read.delim(f)))
  cptac <- QFeatures::readQFeatures(f, ecol = i, sep = "\t", name = "peptides", fnames = "Sequence")
  
  proc$server(dataIn = reactive({Exp1_R25_prot}))
  
  utils::data(Exp1_R25_prot, package='DAPARdata2')
  
  
  mod_all_plots_server('exemple_plot',
                       dataIn = reactive({Exp1_R25_prot})
  ) 
  title <- "Plots"
  mod_UI <- mod_all_plots_ui('exemple_plot')
  # module d'affichage modal contenant ci-dessus
  mod_bsmodal_server('exemple',
                     title = 'Plots',
                     uiContent = MSPipelines::mod_all_plots_ui('plots'),
                     width="75%" # en px ou % de largeur
  )
  
  observeEvent(input$updateStatus, {
    proc$rv$status <- c(1, 1, 0, 1)
    #proc$ToggleState_Screens(FALSE, 1:proc$length)
  })
  
  observeEvent(input$send,{
    if (input$send%%2 != 0)
      rv$dataIn <- cptac
    else
      rv$dataIn <- NULL
  })
  
}
shiny::shinyApp(ui, server)