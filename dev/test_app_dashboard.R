# library(shinydashboard)
# library(tibble)
# options(shiny.fullstacktrace = TRUE)
# 
# #------------------------ Class TimelineDraw --------------------------------------
# source(file.path('../../R', 'class_TimelineDraw.R'), local=TRUE)$value
# source(file.path('../../R', 'global.R'), local=TRUE)$value
# source(file.path('../../R', 'class_ScreenManager.R'), local=TRUE)$value
# source(file.path('../../R', 'class_Process.R'), local=TRUE)$value
# source(file.path("../../R", "mod_popover_for_help.R"), local = TRUE)$value
# source(file.path("../../R", "mod_format_DT.R"), local = TRUE)$value
# source(file.path("../../R", "mod_bsmodal.R"), local=TRUE)$value
# 
# source(file.path('.', 'Example_ProcessA.R'), local=TRUE)$value
# source(file.path('.', 'Example_ProcessB.R'), local=TRUE)$value
# source(file.path('.', 'Example_Description.R'), local=TRUE)$value
# 
# 
# rv <- reactiveValues()
# proc <- Example_ProcessA$new('App', orientation='v')
# 
# 
# 
# 
# 
# ui <- dashboardPage(
#   dashboardHeader(title = "Basic dashboard"),
#   dashboardSidebar(),
#   dashboardBody(
#     tagList(
#       actionButton('send', 'Send dataset'),
#       actionButton('updateStatus', 'Update status'),
#       mod_bsmodal_ui('exemple'),
#       # Boxes need to be put in a row (or column)
#     fluidRow(
#       box(title = "Status summary 2",
#           width = 12,
#           background = "red",
#           proc$ui()))
#     )
#   )
# )
# 
# server <- function(input, output) {
#   
#   
#   # Get a QFeatures dataset for example
#   basename(f <- msdata::quant(pattern = "cptac", full.names = TRUE))
#   i <- grep("Intensity\\.", names(read.delim(f)))
#   cptac <- QFeatures::readQFeatures(f, ecol = i, sep = "\t", name = "peptides", fnames = "Sequence")
#   
#   proc$server(dataIn = reactive({Exp1_R25_prot}))
#   
#   utils::data(Exp1_R25_prot, package='DAPARdata2')
#   
#   
#   mod_all_plots_server('exemple_plot',
#                        dataIn = reactive({Exp1_R25_prot})
#   ) 
#   title <- "Plots"
#   mod_UI <- mod_all_plots_ui('exemple_plot')
#   # module d'affichage modal contenant ci-dessus
#   mod_bsmodal_server('exemple',
#                      title = 'Plots',
#                      uiContent = DaparToolshed::mod_all_plots_ui('plots'),
#                      width="75%" # en px ou % de largeur
#   )
#   
#   observeEvent(input$updateStatus, {
#     proc$rv$status <- c(1, 1, 0, 1)
#     #proc$ToggleState_Screens(FALSE, seq_len(proc$length))
#   })
#   
#   observeEvent(input$send,{
#     if (input$send%%2 != 0)
#       rv$dataIn <- cptac
#     else
#       rv$dataIn <- NULL
#   })
#   
#   
#   
# }
# 
# shinyApp(ui, server)
# 
