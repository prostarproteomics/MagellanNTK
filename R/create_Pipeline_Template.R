#' @title Create pipeline template code
#' 
#' @description xxx
#' 
#' @param config xxx
#' 
#' @examples 
#' \dontrun{
#' conf <- list(mode = "pipeline",
#' steps = c("Step 1", "Step 2", "Save"),
#' mandatory = c(TRUE, FALSE, TRUE)
#' )
#' createPipelineTemplateForExpert('Process1', conf)
#' }
#' @author Samuel Wieczorek
#' 
#' @export
#' 
createPipelineTemplateForExpert <- function(name = NULL,
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
                           nm = gsub(' ', '', config$steps))
  # Write different parts of the module functions
  writeLines(get_ui_function(name))
  writeLines(get_header_server_func(name))
  writeLines(get_config_code(config))
  writeLines(get_module_server_header())
  writeLines(get_renderUI_for_steps(config$steps))
  writeLines(get_output_func())
  
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
get_module_server_header <- function(){
  code <- "
  
  
  ###-------------------------------------------------------------###
  ###                                                             ###
  ### ------------------- MODULE SERVER --------------------------###
  ###                                                             ###
  ###-------------------------------------------------------------###
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  
  
   config$steps <- c(paste0(config$name, '_Description'), config$steps)
     config$steps <- setNames(config$steps,
                              nm = gsub(paste0(config$name, '_'), '', config$steps))
     config$mandatory <- c(TRUE, config$mandatory)
    
    # Insert necessary code which is hosted by Magellan
    # DO NOT MODIFY THIS LINE
    eval(parse(text = ComposedeWorflowCoreCode(
      name = id,
      steps = config$steps)
      )
      )
      
      # Insert code for the description renderUI()
    eval(parse(text = Get_Code_for_module_Description(config$name)),
         envir = .GlobalEnv)
      
  "
  
  code
}


get_renderUI_for_steps <- function(steps){
  code <- NULL
  
  for(i in names(steps)){ 
    code <- paste0(code, 
                   "
output$#step# <- renderUI({ })

")
    code <- gsub('#step#', i, code)
  }
  
  code
  
}


get_output_func <- function(){
  
  code <- "
   # Insert necessary code which is hosted by Magellan
    # DO NOT MODIFY THIS LINE
    eval(parse(text = Module_Return_Func()))
  }
  )
}

  "
code
}