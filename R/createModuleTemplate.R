#' @title Create process template code
#'
#' @description This function creates the source code of an empty module 
#' (i.e. a module without any widgets). This module contains the minimal
#' skeleton to work. The developer can then insert its own code for widgets
#' and data processing functions.
#' 
#' The 'Description' step is generic and creates a *.md file to be filled by
#' th developer.
#'
#'
#' @example examples/example_create_template.R
#'
#' @author Samuel Wieczorek
#'
#' @importFrom stringi stri_locate_all stri_locate
#'
#' @name createTemplate
#' 
#' @param ll.config A `list()` of 4 items
#' @param path xxx
#' @param name xxx
#' 
NULL


#' @return NA
#' @export
#' @rdname createTemplate
#'
createModuleTemplate <- function(config = NULL, path='.') {
  if (class(config) != 'list')
    stop("'config' is not a `list`. Abort.")
  
  config$steps <- gsub(' ', '', config$steps)
  
  # Create template module file
  mod.filename <- paste0(path, '/', config$fullname, ".R")
  if (file.exists(mod.filename)) {
    file.remove(mod.filename)
    }
  con <- file(mod.filename, open = "a")

  # Write code to file
  write_config_func(con, config)
  write_ui_func(con, config$fullname)
  write_header_server_func(con, config$fullname)
  write_module_server_header(con)
  
  if(config$mode == 'process'){
    write_process_code_for_default_value_widgets(con)
    write_process_renderUI_for_steps(con, config, path)
  }
  
  if(config$mode == 'pipeline'){
    write_Insert_Description_Step_code_for_Pipeline(con)
    }

  write_process_output_func(con)
  
  close(con)
    
    ###
    ### Create the Description file
    ###
    desc.filename <- paste0(path, '/', config$fullname, ".md")
    if (file.exists(desc.filename)) {
      file.remove(desc.filename)
    }
    con.desc <- file(desc.filename, open = "a")
    
    write_Code_for_Description_file(config$fullname, con.desc)
    close(con.desc)
    
    return(mod.filename)
}



#' @rdname createTemplate
#' 
write_Code_for_Description_file <- function(con, name){
  
  code <- "
  ## Overview
  
  This page describes the workflow '#name#'.
  
  "
  writeLines(gsub("#name#", name, code), con)
}


#' @rdname createTemplate
#'
write_ui_func <- function(con, name) {
    code <- "
    #name#_ui <- function(id){
    ns <- NS(id)
    }

    "
    writeLines(gsub("#name#", name, code), con)
}

#' @rdname createTemplate
#'
write_header_server_func <- function(con, name) {
    code <- "

  #name#_server <- function(id,
    dataIn = reactive({NULL}),
    steps.enabled = reactive({NULL}),
    remoteReset = reactive({FALSE}),
    steps.status = reactive({NULL}),
    current.pos = reactive({1}),
    verbose = FALSE,
    path = path
    ){
    "
    
    code <- gsub("#name#", name, code)
    writeLines(code, con)
}





#' @rdname createTemplate
#'
write_config_func <- function(con, config) {
    code <- "
    #fullname#_conf <- function(){
      Config(
        mode = '#mode#',
        fullname = '#fullname#',
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


#' @rdname createTemplate
#'
write_process_code_for_default_value_widgets <- function(con) {
    code <- "
    # Define default selected values for widgets
    # This is only for simple workflows
    widgets.default.values <- list()
    rv.custom.default.values <- list()
    "

    writeLines(code, con)
}


#' @rdname createTemplate
#'
write_module_server_header <- function(con) {
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
    core.code <- Get_Worflow_Core_Code(
        name = id,
        w.names = names(widgets.default.values),
        rv.custom.names = names(rv.custom.default.values)
        )
          
    eval(str2expression(core.code))

    "

    writeLines(code, con)
}

#' @rdname createTemplate
#' 
#' @param steps xxx
#' 
write_process_renderUI_for_steps <- function(con, ll.config, path) {
    code <- NULL

    tmp.config <- Config(fullname = ll.config$fullname,
                         mode = ll.config$mode,
                         steps = ll.config$steps,
                         mandatory = ll.config$mandatory)
    
    for (i in tmp.config@steps) {
      if (i == 'Description'){
        code <- paste0(
          code,
          write_Insert_Description_Step_code_for_Process(con, path)
        )
      } else if (i == 'Save'){
        code <- paste0(
          code,
          write_Insert_Save_Step_code_for_Process(con)
        )
      } else {
        code <- paste0(
            code,
            "
          output$#step# <- renderUI({ })

            ")
      }
        code <- gsub("#step#", i, code)
    }

    
    #remove tmp.config
    
    writeLines(code, con)
}


#' @title xxx
#' @description This function inserts the necessary code for the 'Description' step
#' @rdname createTemplate
#' 
write_Insert_Description_Step_code_for_Process <- function(con, path){

  
  code <- "
# >>>
# >>> START ------------- Code for Description UI---------------
# >>> 


output$Description <- renderUI({
  file <- paste0(#path#, '/md/', id, '.md')
  
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
    class = GlobalSettings$btn_success_color)
  toggleWidget(widget, rv$steps.enabled['Description'])
})


observeEvent(input$Description_btn_validate, {
  rv$dataIn <- dataIn()
  dataOut$trigger <- Timestamp()
  dataOut$value <- rv$dataIn
  rv$steps.status['Description'] <- global$VALIDATED
})


"

  code <- gsub('#path#', path, code)
writeLines(code, con)
}


#' @title xxx
#' @description This function inserts the necessary code for the 'Description' step
#' @rdname createTemplate
#' 
write_Insert_Save_Step_code_for_Process <- function(con){
  
  
  code <- "
# >>> START ------------- Code for step 3 UI---------------
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
      req(rv$steps.status['Save'] == global$VALIDATED)
      dl_ui(ns('createQuickLink'))
    })
    
    output$Save_btn_validate_ui <- renderUI({
      toggleWidget(actionButton(ns('Save_btn_validate'), 'Save',
                                  class = GlobalSettings$btn_success_color),
                     rv$steps.enabled['Save']
                     )
    })
    observeEvent(input$Save_btn_validate, {
      # Do some stuff
      rv$dataIn <- Add_Datasets_to_Object(object = rv$dataIn,
                                          dataset = rnorm(1:5),
                                          name = id)
      
      # DO NOT MODIFY THE THREE FOLLOWINF LINES
      dataOut$trigger <- Timestamp()
      dataOut$value <- rv$dataIn
      rv$steps.status['Save'] <- global$VALIDATED
      dl_server('createQuickLink', 
        dataIn = reactive({rv$dataIn}))
      
    })
    # <<< END ------------- Code for step 3 UI---------------



"
  
  writeLines(code, con)
}



#' @title xxx
#' @description This function inserts the necessary code for the 
#' Description' step
#' @rdname createTemplate
#' 
write_Insert_Description_Step_code_for_Pipeline <- function(con){
  
  
  code <- "
###
###
###

#' @export
Description_conf <- function(){
Config(
    mode = 'process',
    fullname = 'Description',
    steps = c('Description'),
    mandatory = c(TRUE)
    )
}
    
#' @export
Description_ui <- function(id){
  ns <- NS(id)
}


#' @export
Description_server <- function(id,
  dataIn = reactive({NULL}),
  steps.enabled = reactive({NULL}),
  remoteReset = reactive({FALSE}),
  steps.status = reactive({NULL}),
  current.pos = reactive({1}),
  verbose = FALSE
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
    eval(
      str2expression(
        Get_Worflow_Core_Code(
        name = id,
          w.names = names(widgets.default.values),
          rv.custom.names = names(rv.custom.default.values)
          
        )
      )
    )
    
    #rv.custom <- reactiveValues()
    #rv.custom.default.values <- list()
    
    
    # Insert necessary code which is hosted by MagellanNTK
    # DO NOT MODIFY THIS LINE
    eval(parse(text = Module_Return_Func()))
    
  }
  )
}


"
  
  writeLines(code, con)
}



#' @rdname createTemplate
write_process_output_func <- function(con) {
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





#' @rdname createTemplate
#'
write_pipeline_module_server <- function(con) {
    code <- "


    ###-------------------------------------------------------------###
    ###                                                             ###
    ### ------------------- MODULE SERVER --------------------------###
    ###                                                             ###
    ###-------------------------------------------------------------###
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

    core.code <- Get_Worflow_Core_Code(
      name = id,
      w.names = names(widgets.default.values),
      rv.custom.names = names(rv.custom.default.values)
      )
    eval(str2expression(core.code))

      
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
#' @rdname createTemplate
#'
vec2code <- function(ls_list, is.char = FALSE) {
  if (is.char) {
    coll <- "', '"
  } else {
    coll <- ", "
  }
  
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