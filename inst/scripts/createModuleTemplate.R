#' @title Create template code for modules
#' @description xxx
#' 
#' @param con xxx
#' @param name xxx
#' @param config xxx
#' @param mode xxx
#' @param ll.config xxx
#' @param step.name xxx
#' @param fullname xxx
#' @param path xxx
#' 
#' @export
#' @name create_template
#'
createModuleTemplate <- function(config = NULL, 
                                 path = '.') {
  #if (!inherits(config, 'list'))
  #  stop("'config' is not a `list`. Abort.")
  
  if(is.null(path))
    stop('path is not configured.')
  
  # Return values
  value <- NULL
  
  if (!is.null(config$steps))
    names(config$steps) <- gsub(' ', '', config$steps)
 
  # Create template module file
  mod.filename <- file.path(path, "R", paste0(config$fullname, ".R"))
  value <- c('R', paste0(config$fullname, ".R"))
  if (file.exists(mod.filename)) {
    file.remove(mod.filename)
    }
  con <- file(mod.filename, open = "a")

  # Write code to file
  write_general_comment(con, config$fullname)
  write_config_func(con, config)
  write_ui_func(con, config$fullname)
  write_header_server_func(con, config$fullname)
  
  #if(config$mode == 'process')
    write_process_code_for_default_value_widgets(con)
  
  
  write_module_server_header(con, config$mode)
  
  if(config$mode == 'process'){
     write_process_renderUI_for_steps(con, config, path)
  }
  
  write_output_func(con)
  close(con)
    
  
  
  # Create description files:
  # * source code for Description step of a pipeline,
  # * md files
  #
  
  if(config$mode == 'pipeline')
    value <- c(value, Create_Pipeline_Description_source_file(config$fullname, path))

  
  # if (config$mode == 'pipeline')
  #   # In this particularly case, on add the prefix '_Description' to the name of the
  #   # process. Here, it is considered as an independant step of the pipeline and
  #   # all steps names are built on the same pattern : the name of the workflow
  #   # suffixed with the name of the step
  #   value <- c(value, Create_md_file(path, paste0(config$fullname, '_Description')))
  # else
  #   # Here, one can directly use the fullname because it refers to an internal step, 
  #   # not to a process
  #   value <- c(value, Create_md_file(path, config$fullname))
  # 
  
  # Create extra functions template
  #value <- c(value, createExtraFunctions(path))
  
  
  #value <- c(value, createExtraModule(path, extraModuleName))
  
  return(paste0(config$fullname, ".R"))
}



#' @rdname create_template
#' 
write_general_comment <- function(con, name){
  
  code <- 
  code <- gsub("#name#", name, code)
  writeLines(code, con)
  
}




# Create_md_file <- function(path, fullname){
# ###
# ### Create the Description md file
# ###
#   
#   
# desc.dir <- file.path(path, 'md')
# if (!dir.exists(desc.dir)) 
#   dir.create(desc.dir)          
# 
# md.file <- paste0(fullname, ".md")
# desc.filename <- file.path(desc.dir, md.file)
# if (file.exists(desc.filename)) {
#   file.remove(desc.filename)
# }
# con.desc <- file(desc.filename, open = "a")
# 
# 
# code <- "
#   ## Overview
#   
#   This page describes the workflow '#name#'.
#   
#   "
# writeLines(gsub("#name#", fullname, code), con.desc)
# 
# 
# close(con.desc)
# 
# return(md.file)
# 
# }


#' @rdname create_template
#'
write_ui_func <- function(con, name) {
code <- "
#' @export
#' 
#name#_ui <- function(id){
  ns <- NS(id)
  }

"
    writeLines(gsub("#name#", name, code), con)
}

#' @rdname create_template
#'
write_header_server_func <- function(con, name) {
code <- "
#' @export
#' 
 #name#_server <- function(id,
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

"
    
    code <- gsub("#name#", name, code)
    writeLines(code, con)
}





#' @rdname create_template
#'
write_config_func <- function(con, config) {
code <- "
#' @export
#' 
#fullname#_conf <- function(){
  Config(
    fullname = '#fullname#',
    mode = '#mode#',
    steps = #steps#,
    mandatory = #mandatory#
    )
}

"
    code <- gsub("#mode#", config$mode, code)
    code <- gsub("#fullname#", config$fullname, code)
    code <- gsub("#steps#", vec2code(config$steps, TRUE), code)
    code <- gsub("#mandatory#", vec2code(config$mandatory, FALSE), code)
    
    writeLines(code, con)
}


#' @rdname create_template
#'
write_process_code_for_default_value_widgets <- function(con) {
code <- "
  # Define default selected values for widgets
  # This is only for simple workflows
  widgets.default.values <- NULL
  rv.custom.default.values <- NULL
  
"

writeLines(code, con)
}


#' @rdname create_template
#'
write_module_server_header <- function(con, mode) {
code <- "
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
    mode = '#mode#',
    name = id,
    w.names = names(widgets.default.values),
    rv.custom.names = names(rv.custom.default.values)
    )
          
  eval(str2expression(core.code))

"
code <- gsub("#mode#", mode, code)
writeLines(code, con)
}

#' @rdname create_template
#' @export
write_process_renderUI_for_steps <- function(con, ll.config, path) {
    code <- NULL

    # Create a temp Config so as to automatically
    # add 'Description' and 'Save' steps if needed
  tmp.config <- Config(fullname = ll.config$fullname,
                       mode = ll.config$mode,
                       steps = ll.config$steps,
                       mandatory = ll.config$mandatory)
  #show(tmp.config)

  for (i in names(tmp.config@steps)) {
    if (i == 'Description'){
      code <- paste0(
        code,
        write_Insert_Description_Step_code_for_Process(con, path)
        )
    } else if (i == 'Save'){
      code <- paste0(code,
        write_Insert_Save_Step_code_for_Process(con)
      )
    } else {
      code <- paste0(code, write_stepUI_template(i))
      }
code <- gsub("#step#", i, code)
}

#remove tmp.config
rm(tmp.config)
writeLines(code, con)
}

  
#' @rdname create_template
#' @export
#' 
write_stepUI_template <- function(step.name){
    
code <- "
output$#step.name# <- renderUI({

wellPanel(
        # uiOutput for all widgets in this UI
        # This part is mandatory
        # The renderUI() function of each widget is managed by MagellanNTK
        # The dev only have to define a reactive() function for each
        # widget he want to insert
        # Be aware of the naming convention for ids in uiOutput()
        # For more details, please refer to the dev document.
        
        # Insert validation button
        uiOutput(ns('#step.name#_btn_validate_ui')),
        
        # Additional code
            
        )
    })


output$#step.name#_btn_validate_ui <- renderUI({
    widget <-  actionButton(ns('#step.name#_btn_validate'),
                   'Perform',
                   class = btn_success_color)
      toggleWidget(widget, rv$steps.enabled['#step.name#'] )
      
    })
    # >>> END: Definition of the widgets
    
    
    observeEvent(input$#step.name#_btn_validate, {
      # Do some stuff
      
      # Here, you to hase use a function to add an item to the
      # dataset
      # rv$dataIn <- addDatasets(
      #                object = rv$dataIn,
      #                dataset = rnorm(1:5),
      #                name = paste0('temp_',id)
      #                )
      
      # DO NOT MODIFY THE THREE FOLLOWINF LINES
      dataOut$trigger <- Timestamp()
      dataOut$value <- rv$dataIn
      rv$steps.status['#step.name#'] <- stepStatus$VALIDATED
    })
  

"

code <- gsub('#step.name#', step.name, code)
code
  }



#' @title xxx
#' @description This function inserts the necessary code for the 'Description' step
#' @rdname create_template
#' 
write_Insert_Description_Step_code_for_Process <- function(con, path){

code <- "
  # >>>
  # >>> START ------------- Code for Description UI---------------
  # >>> 

  output$Description <- renderUI({
    md.file <- paste0(id, '.md')
    file <- file.path(path, 'md', md.file)
  
    tagList(
      # In this example, the md file is found in the extdata/module_examples directory
      # but with a real app, it should be provided by the package which
      # contains the UI for the different steps of the process module.
      # system.file(xxx)
    
      if (file.exists(file))
        includeMarkdown(file)
      else
        p('No Description available'),
    
      # Used to show some information about the dataset which is loaded
      # This function must be provided by the package of the process module
      uiOutput(ns('datasetDescription_ui')),
    
      # Insert validation button
      uiOutput(ns('Description_btn_validate_ui'))
      )
    })

    output$datasetDescription_ui <- renderUI({
      # Insert your own code to vizualise some information
      # about your dataset. It will appear once the 'Start' button
      # has been clicked
  
      })

    output$Description_btn_validate_ui <- renderUI({
      widget <- actionButton(ns('Description_btn_validate'),
                  'Start',
                  class = btn_success_color)
      toggleWidget(widget, rv$steps.enabled['Description'])
      })


    observeEvent(input$Description_btn_validate, {
      rv$dataIn <- dataIn()
      dataOut$trigger <- Timestamp()
      dataOut$value <- rv$dataIn
      rv$steps.status['Description'] <- stepStatus$VALIDATED
    })


"

code <- gsub('#path#', path, code)
writeLines(code, con)
}


#' @title xxx
#' @description This function inserts the necessary code for the 'Description' step
#' @rdname create_template
#' 
write_Insert_Save_Step_code_for_Process <- function(con){
  
code <- "
  # >>> START ------------- Code for step 'Save' UI---------------
  output$Save <- renderUI({
    tagList(
      # Insert validation button
      # This line is necessary. DO NOT MODIFY
      uiOutput(ns('Save_btn_validate_ui')),
      uiOutput(ns('dl_ui'))
      )
    })
    
  output$dl_ui <- renderUI({
    req(config@mode == 'process')
    req(rv$steps.status['Save'] == stepStatus$VALIDATED)
    dl_ui(ns('createQuickLink'))
    })
    
  output$Save_btn_validate_ui <- renderUI({
    toggleWidget(actionButton(ns('Save_btn_validate'), 'Save',
                        class = btn_success_color),
                       rv$steps.enabled['Save']
                     )
    })
    
    observeEvent(input$Save_btn_validate, {
      # Do some stuff
      rv$dataIn <- addDatasets(object = rv$dataIn,
      dataset = rnorm(1:5),
                          name = id)
    
      # DO NOT MODIFY THE THREE FOLLOWINF LINES
      dataOut$trigger <- Timestamp()
      dataOut$value <- rv$dataIn
      rv$steps.status['Save'] <- stepStatus$VALIDATED
      dl_server('createQuickLink', dataIn = reactive({rv$dataIn}))
      
      })
      
    # <<< END ------------- Code for step 'Save' UI---------------

"
  
writeLines(code, con)
}



#' @title xxx
#' @description This function inserts the necessary code for the 
#' Description' step
#' @rdname create_template
#' 
Create_Pipeline_Description_source_file <- function(fullname, path){

  mod.filename <- file.path(path, paste0(fullname, "_Description.R"))
  
  if (file.exists(mod.filename)) {
    file.remove(mod.filename)
  }
  con.desc <- file(mod.filename, open = "a")
  
  
  
code <- "
###
###
###

#' @export
#'
#fullname#_Description_conf <- function(){
  Config(
    mode = 'process',
    fullname = '#fullname#_Description'
    )
}
    
#' @export
#'
#fullname#_Description_ui <- function(id){
  ns <- NS(id)
}


#' @export
#'
#fullname#_Description_server <- function(id,
  dataIn = reactive({NULL}),
  steps.enabled = reactive({NULL}),
  remoteReset = reactive({FALSE}),
  steps.status = reactive({NULL}),
  current.pos = reactive({1}),
  path = path
){
  
  
  # Define default selected values for widgets
  # By default, this list is empty for the Description module
  # but it can be customized
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
    

"
code <- gsub('#fullname#', fullname, code)
  
  writeLines(code, con.desc)
  
  write_Insert_Description_Step_code_for_Process(con.desc, path)
  write_output_func(con.desc)
  close(con.desc)
  return(paste0(fullname, "_Description.R"))
}



#' @rdname create_template
write_output_func <- function(con) {
code <- "
# Insert necessary code which is hosted by MagellanNTK
# DO NOT MODIFY THIS LINE
  eval(parse(text = Module_Return_Func()))
  }
  )
}

"

writeLines(code, con)
}


### Utility functions


#' @title Convert vector to a source code string
#' @param ls_list A vector
#' @param is.char A `bolean(1)` to indicate whether the items are
#' strings or not. In this case, they will be quoted in the result
#' 
#' @return A string
#' @rdname create_template
#'
vec2code <- function(ls_list, is.char = FALSE) {
  st_string_from_list <- 'c()'
  if (is.char)
    coll <- "', '"
  else
    coll <- ", "

  if (length(ls_list) > 0){
  # create string
  if (is.char) {
    st_string_from_list <- paste0(
      "c('",
      paste0(ls_list, sep = "", collapse = coll)
    )
  } else {
    st_string_from_list <- paste0(
      "c(",
      paste0(ls_list, sep = "", collapse = coll)
    )
  }
  
  if (is.char) {
    paste0(st_string_from_list, "')")
  } else {
    paste0(st_string_from_list, ")")
  }
  }
  
  st_string_from_list
}