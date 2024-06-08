library(shinydashboard)
library(shinyjs)



#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @importFrom shiny shinyUI tagList 
#' @import shinydashboardPlus
#' @import shinydashboard
#' @importFrom shinyjs useShinyjs extendShinyjs
#' 
#' @export
#' 
MagellanNTK_ui <- function(id){
  ns <- NS(id)
  shiny::tagList(
    #launchGA(),
    shinyjs::useShinyjs(),
    shinyjs::extendShinyjs(
      text = "shinyjs.resetProstar = function() {history.go(0)}",
      functions = c("resetProstar")),
    
    shiny::titlePanel("", windowTitle = "Prostar"),
    #hidden(div(id = 'div_mainapp_module',
    mainapp_ui(ns('mainapp_module'))
    #)
  )
}



options(shiny.maxRequestSize=300*1024^2,
  encoding = "UTF-8",
  shiny.fullstacktrace = TRUE
)
require(compiler)
enableJIT(3)


#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @importFrom  shiny shinyServer observeEvent toggle
#' @import shinyjs
#' 
#' @export
#' 
#' @noRd
MagellanNTK_server <- function(id,
  obj = reactive({NULL}),
  workflow.path = reactive({NULL}),
  workflow.name = reactive({NULL})){
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns 
    #addResourcePath(prefix = "www", directoryPath = "./www")
    addResourcePath('www', system.file('app/www', package='MagellanNTK'))
    
    #shiny::observeEvent(funcs, {
    #  for(i in names(funcs))
    #    requireNamespace(unlist(strsplit(funcs[[i]], split='::'))[1])
      
      #shinyjs::toggle('mainapp_module', condition = !is.null(funcs))
      mainapp_server('mainapp_module',
        obj = obj,
        workflow.path = reactive({workflow.path()}),
        workflow.name = reactive({workflow.name()})
        )
    })
  }
#)
#}


#' @title Main Shiny application
#' @description xxx
#' 
#' @param funcs An list containing th following items:
#' * title: xxx
#' * base_URL: xxx
#' @param verbose A `boolean(1)` 
#' 
#' @details
#' The list of customizable funcs (param `funcs`) contains the following items:
#' 
#'  These are the default values where each item points to a default fucntion
#'  implemented into MagellanNTK.
#'  
#'  The user can modify these values by two means:
#'  * setting the values in the parameter to pass to the function 
#'  `MagellanNTK()`,
#'  * inside the UI of MagellanNTK, in the settings panels
#'  
#'  
#' @export
#' @examplesIf interactive()
#' 
#' # launch without initial config
#' shiny::runApp(MagellanNTK())
#' 
#' 
#' MagellanNTK(funcs)
#' 
#' @export
#' 
MagellanNTK <- function(
    obj = NULL,
    workflow.path = NULL,
    workflow.name = NULL,
    verbose = FALSE) {
  
  source_shinyApp_files()
  
  # Set global variables to global environment
  #.GlobalEnv$funcs <- funcs
  #.GlobalEnv$workflow <- workflow
  
  #on.exit(rm(funcs, envir = .GlobalEnv))
  #on.exit(rm(workflow, envir=.GlobalEnv))
  
  #source_wf_files(workflow$path)
  #
  ui <-  MagellanNTK_ui("infos")
  
  
  server <- function(input, output, session) {
    MagellanNTK_server("infos",
      obj = reactive({obj}),
      workflow.path = reactive({workflow.path}),
      workflow.name = reactive({workflow.name})
      )
}
  
  # Launch app
  app <- shiny::shinyApp(ui, server)
  shiny::runApp(app)
  
}
