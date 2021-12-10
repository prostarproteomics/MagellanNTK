#' @title Create process template code
#' 
#' @description xxx
#' 
#' @param name xxx
#' @param config xxx
#' 
#' @examples 
#' \dontrun{
#' conf.process <- list(mode = "process",
#' steps = c("Step 1", "Step 2", "Save"),
#' mandatory = c(TRUE, FALSE, TRUE),
#' path_to_md_dir = NULL
#' )
#' createModuleTemplate('Process1', conf.process)
#' 
#' conf.pipeline <- list(mode = "pipeline",
#' steps = c("Process 1", "Process 2", "Process 3"),
#' mandatory = c(TRUE, FALSE, TRUE),
#' path_to_md_dir = system.file('module_examples/md/', package='Magellan')
#' )
#' createModuleTemplate('PipelineA', conf.pipeline)
#' }
#' 
#' @author Samuel Wieczorek
#' 
#' @importFrom stringi stri_locate_all stri_locate
#' 
#' @export
#' 
createModuleTemplate <- function(name = NULL,
                                 config = NULL){
  
  # Check config integrity
   check <- CheckConfig(config)
   if (!check$passed)
    stop(paste0("Errors in 'config'", paste0(check$msg, collapse=' ')))
   
   # Create file
   mod.filename <- paste0('mod_', name, '.R')
   if (file.exists(mod.filename))
     file.remove(mod.filename)
   con <- file(mod.filename, open = 'a')
   
   
   switch(config$mode,
          process = {
            config$steps <- setNames(config$steps, 
                                     nm = gsub(' ', '', config$steps))
            # Write different parts of the module functions
            writeLines(get_process_ui_function(name))
            writeLines(get_process_header_server_func(name))
            writeLines(get_process_config_code(config))
            writeLines(get_process_code_for_default_value_widgets())
            writeLines(get_process_module_server_header())
            writeLines(get_process_renderUI_for_steps(config$steps))
            writeLines(get_process_output_func())
          },
          pipeline = {
            config$steps <- setNames(config$steps, 
                                     nm = gsub(' ', '', config$steps))
            # Write different parts of the module functions
            writeLines(get_pipeline_ui_function(name))
            writeLines(get_pipeline_header_server_func(name))
            writeLines(get_pipeline_config_code(config))
            writeLines(get_pipeline_module_server())
          }
          )
   
   
   close(con)
}





#' @noRd
#' 
get_process_ui_function <- function(name){
  
  code <- "
  mod_#name#_ui <- function(id){
    ns <- NS(id)
  }
  
  "
  
  gsub('#name#', name, code)
  
}

#' @noRd
#' 
get_process_header_server_func <- function(name){
  code <- "
  
  mod_#name#_server <- function(id,
                                dataIn = reactive({NULL}),
                                steps.enabled = reactive({NULL}),
                                remoteReset = reactive({FALSE}),
                                steps.status = reactive({NULL}),
                                current.pos = reactive({1}),
                                verbose = FALSE
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
get_process_config_code <- function(config){
  
  code <- "
  # This list contains the basic configuration of the process
  config <- list(
  # Define the type of module
    mode = 'process',
    
    # List of all steps of the process
    steps = #steps#,
    
    # A vector of boolean indicating if the steps are mandatory or not.
    mandatory = #mandatory#,
    
    path_to_md_dir = #path_to_md_dir#
  )
  "
  code <- gsub('#steps#', vec2code(config$steps, TRUE), code)
  code <- gsub('#mandatory#', vec2code(config$mandatory, FALSE), code)
  if(is.null(config$path_to_md_dir))
    config$path_to_md_dir <- 'NULL'
  code <- gsub('#path_to_md_dir#', config$path_to_md_dir, code)
  
  
  code
}


#' @noRd
#' 
get_process_code_for_default_value_widgets <- function(){
  code <- "
  # Define default selected values for widgets
  # This is only for simple workflows
  widgets.default.values <- list()
  
  "
  
  code
}


#' @noRd
#' 
get_process_module_server_header <- function(){
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


get_process_renderUI_for_steps <- function(steps){
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


get_process_output_func <- function(){
  
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





#' @noRd
#' 
get_pipeline_ui_function <- function(name){
  
  code <- "
  mod_#name#_ui <- function(id){
    ns <- NS(id)
  }
  
  "
  
  gsub('#name#', name, code)
  
}

#' @noRd
#' 
get_pipeline_header_server_func <- function(name){
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
get_pipeline_config_code <- function(config){
  
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
get_pipeline_module_server <- function(){
  code <- "
  
  
  ###-------------------------------------------------------------###
  ###                                                             ###
  ### ------------------- MODULE SERVER --------------------------###
  ###                                                             ###
  ###-------------------------------------------------------------###
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  
  
   eval(parse(text = Get_Code_Update_Config_Pipeline()))
    
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
         
         # Insert necessary code which is hosted by Magellan
    # DO NOT MODIFY THIS LINE
    eval(parse(text = Module_Return_Func()))
  }
  )
}
      
  "

code
}

