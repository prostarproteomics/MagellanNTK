#' @title Create process template code
#' 
#' @description xxx
#' 
#' @param config xxx
#' 
#' @examples 
#' \dontrun{
#' conf <- list(mode = "process",
#' steps = c("Step 1", "Step 2", "Save"),
#' mandatory = c(TRUE, FALSE, TRUE)
#' )
#' createProcessTemplateForExpert(name = 'Process1', config = conf)
#' }
#' @author Samuel Wieczorek
#' 
#' @export
#' 
createProcessTemplateForExpert <- function(name = NULL,
                                           config = NULL){
  
  # Check config integrity
 # check <- CheckConfig(config)
 # if (!check$passed)
  #  stop(paste0("Errors in 'config'", paste0(check$msg, collapse=' ')))
  
  # Create file
  mod.filename <- paste0('mod_', name, '.R')
  if (file.exists(mod.filename))
    file.remove(mod.filename)
  con <- file(mod.filename, open = 'a')
  
  
  
  config$steps <- setNames(config$steps, 
                           nm = gsub('\ ', '', config$steps))
  # Write different parts of the module functions
  writeLines(get_ui_function(name))
  writeLines(get_header_server_func(name))
  writeLines(get_config_code(config))
  writeLines(get_code_for_default_value_widgets())
  
  
  # write_module_server_header(con)
  # write_declaration_rv_widgets(con)
  # 
  # write_rv_reactiveVariable(con)
  # write_dataOut_variable(con)
  # write_observeEvents(con)
  # write_code_step0(con)
  # write_return_value(con)
  
  
  
  close(con)
}


#' @noRd
#' 
get_ui_function <- function(name){
  
  code <- "
  mod_#name#_ui <- function(id){
    ns <- NS(id)
  }
  
  "

  gsub('#name#', name, code)

}

#' @noRd
#' 
get_header_server_func <- function(name){
  code <- "
  
  mod_#name#_server <- function(id,
                                dataIn = reactive({NULL}),
                                steps.enabled = reactive({NULL}),
                                remoteReset = reactive({FALSE}),
                                steps.status = reactive({NULL}),
                                current.pos = reactive({1})
                                ){
  "
  gsub('#name#', name, code)

}



#' @noRd
#' 
vec2code <- function(ls_list, is.char = FALSE) {
  
  if (is.char)
    coll <- "', '"
  else
    coll <- ", "
  
  # create string
  if(is.char)
    st_string_from_list = paste0("c('", 
                               paste0(ls_list, sep="", collapse= coll)
                               )
  else
    st_string_from_list = paste0("c(", 
                                 paste0(ls_list, sep="", collapse= coll)
    )
  
  if(is.char)
    paste0(st_string_from_list, "')")
  else
    paste0(st_string_from_list, ")")
}


#' @noRd
#' 
get_config_code <- function(config){
  
  code <- "
  # This list contains the basic configuration of the process
  config <- list(
  # Define the type of module
    mode = 'process',
    
    # List of all steps of the process
    steps = #steps#,
    
    # A vector of boolean indicating if the steps are mandatory or not.
    mandatory = #mandatory#
  )
  "
  
  code <- gsub('#steps#', vec2code(config$steps, TRUE), code)
  code <- gsub('#mandatory#', vec2code(config$mandatory, FALSE), code)
  code
}


#' @noRd
#' 
get_code_for_default_value_widgets <- function(){
  code <- "
  # Define default selected values for widgets
  # This is only for simple workflows
  widgets.default.values <- list()
  
  "
  
  code
}


#' @noRd
#' 
get_module_server_header <- function(){
  code <- "
  
  
  ###-------------------------------------------------------------###
  ###                                                             ###
  ### ------------------- MODULE SERVER --------------------------###
  ###                                                             ###
  ###-------------------------------------------------------------###
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  
    # Insert necessary code which is hosted by Magellan
    # DO NOT MODIFY THIS LINE
    eval(str2expression(Get_Code_Update_Config()))
    
    eval(str2expression(
      SimpleWorflowCoreCode(
        name = id,
        widgets = names(widgets.default.values),
        steps = config$steps)
      )
      )
      
  "
  
  code
}


get_renderUI_for_steps <- function(steps){
  
  
}