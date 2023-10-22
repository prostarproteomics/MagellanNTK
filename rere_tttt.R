
  #' @title Shiny example process module.
#'
#' @description
#' This module contains the configuration informations for the corresponding pipeline.
#' It is called by the nav_pipeline module of the package MagellanNTK
#' 
#' The name of the server and ui functions are formatted with keywords separated by '_', as follows:
#' * first string `mod`: indicates that it is a Shiny module
#' * `pipeline name` is the name of the pipeline to which the process belongs
#' * `process name` is the name of the process itself
#' 
#' This convention is important because MagellanNTK call the different
#' server and ui functions by building dynamically their name.
#' 
#' In this example, `rere_tttt_ui()` and `rere_tttt_server()` define
#' the code for the process `xxx` which is part of the pipeline called `xxx`.
#'
#' @name module_rere_tttt
#' 
#' @param id xxx
#' @param dataIn The dataset
#' @param steps.enabled A vector of boolean which has the same length of the steps
#' of the pipeline. This information is used to enable/disable the widgets. It is not
#' a communication variable between the caller and this module, thus there is no
#' corresponding output variable
#' @param remoteReset It is a remote command to reset the module. A boolean that
#' indicates is the pipeline has been reseted by a program of higher level
#' Basically, it is the program which has called this module
#' @param steps.status xxx
#' @param current.pos xxx
#' @param path xxx
#'
NULL


  

#' @export
#' 
rere_tttt_conf <- function(){
  Config(
    fullname = 'rere_tttt',
    mode = 'process',
    steps = c('dsdd'),
    mandatory = c(TRUE)
    )
}



#' @export
#' 
rere_tttt_ui <- function(id){
  ns <- NS(id)
  }



#' @export
#' 
 rere_tttt_server <- function(id,
  dataIn = reactive({NULL}),
  steps.enabled = reactive({NULL}),
  remoteReset = reactive({FALSE}),
  steps.status = reactive({NULL}),
  current.pos = reactive({1}),
  path = path
  ){
  
  # Here, you can source other .R files which contains
  # shiny modules code which will be used in this workflow.
  # This file must be stored in the same directory as all
  # the other ones
  # R.filename <- xxx
  # source(paste0(path, R.filename), local=TRUE)$value



  # Define default selected values for widgets
  # This is only for simple workflows
  widgets.default.values <- NULL
  rv.custom.default.values <- NULL
  


###-------------------------------------------------------------###
###                                                             ###
### ------------------- MODULE SERVER --------------------------###
###                                                             ###
###-------------------------------------------------------------###
moduleServer(id, function(input, output, session) {
  ns <- session$ns

  # Insert necessary code which is hosted by MagellanNTK
  # DO NOT MODIFY THIS LINE
  core.code <- Get_Workflow_Core_Code(
    mode = 'process',
    name = id,
    w.names = names(widgets.default.values),
    rv.custom.names = names(rv.custom.default.values)
    )
          
  eval(str2expression(core.code))


