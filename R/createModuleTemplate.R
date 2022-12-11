#' @title Create process template code
#'
#' @description This function creates the source code of an empty module 
#' (i.e. a module without any widgets). This module contains the minimal
#' skeleton to work. The developer can then insert its own code for widgets
#' and data processing functions.
#'
#' @param name A `character(0)`
#' @param config An instance of the class `Config`.
#'
#' @example inst/examples/example_create_mod_template.R
#'
#'
#' @author Samuel Wieczorek
#'
#' @importFrom stringi stri_locate_all stri_locate
#'
#' @export
#'
#' @return NA
#'
createModuleTemplate <- function(config = NULL) {

    # Check config integrity
    # check <- CheckConfig(config)
    # if (!check$passed)
    #  stop(paste0("Errors in 'config'", paste0(check$msg, collapse=' ')))


    # Create template module file
    mod.filename <- paste0("mod_", config@name, ".R")
    if (file.exists(mod.filename)) {
        file.remove(mod.filename)
      }
    con <- file(mod.filename, open = "a")


    switch(config@mode,
        process = {
            config@steps <- setNames(config@steps, nm = gsub(" ", "", config@steps))
            # Write different parts of the module functions
            writeLines(get_process_ui_function(config@name), con)
            writeLines(get_process_header_server_func(config@name), con)
            writeLines(get_process_config_code(config), con)
            writeLines(get_process_code_for_default_value_widgets(), con)
            writeLines(get_process_module_server_header(), con)
            writeLines(get_process_renderUI_for_steps(config@steps), con)
            writeLines(get_process_output_func(), con)
            },
        pipeline = {
            config@steps <- setNames(config@steps, nm = gsub(" ", "", config@steps))
            # Write different parts of the module functions
            writeLines(get_pipeline_ui_function(config@name), con)
            writeLines(get_pipeline_header_server_func(config@name), con)
            writeLines(get_pipeline_config_code(config), con)
            writeLines(get_pipeline_module_server(), con)
        }
    )


    close(con)
    
    ###
    ### Create the Description file
    ###
    desc.filename <- paste0(config@name, ".md")
    if (file.exists(desc.filename)) {
      file.remove(desc.filename)
    }
    con <- file(desc.filename, open = "a")
    writeLines(Code_for_Description_file(config@name), con)
    close(con)
}


#' @noRd
#' 
Code_for_Description_file <- function(name){
  
  code <- "
  ## Overview
  
  This page describes the workflow '#name#'.
  
  "
  code <- gsub("#name#", name, code)
code
}


#' @noRd
#'
get_process_ui_function <- function(name) {
    code <- "
    mod_#name#_ui <- function(id){
    ns <- NS(id)
    }

    "

    gsub("#name#", name, code)
}

#' @noRd
#'
get_process_header_server_func <- function(name) {
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
    gsub("#name#", name, code)
}



#' @noRd
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


#' @noRd
#'
get_process_config_code <- function(config) {
    code <- "
    # This list contains the basic configuration of the process
    config <- Config(
    # Define the type of module
    mode = 'process',
    name = '#name#',
    # List of all steps of the process
    steps = #steps#,

    # A vector of boolean indicating if the steps are mandatory or not.
    mandatory = #mandatory#,

    path_to_md_dir = '#path_to_md_dir#'
    )
    "
    code <- gsub("#name#", config@name, code)
    code <- gsub("#steps#", vec2code(config@steps, TRUE), code)
    code <- gsub("#mandatory#", vec2code(config@mandatory, FALSE), code)
    if (is.null(config@path_to_md_dir) || config@path_to_md_dir == '') {
        config@path_to_md_dir <- "\'\'"
    }
    code <- gsub("#path_to_md_dir#", config@path_to_md_dir, code)
    code
}


#' @noRd
#'
get_process_code_for_default_value_widgets <- function() {
    code <- "
    # Define default selected values for widgets
    # This is only for simple workflows
    widgets.default.values <- list()
    rv.custom.default.values <- list()
    "

    code
}


#' @noRd
#'
get_process_module_server_header <- function() {
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
    #eval(str2expression(Get_Code_Update_Config()))

    # Insert necessary code which is hosted by Magellan
    # DO NOT MODIFY THIS LINE
    eval(
      str2expression(
        Get_Worflow_Core_Code(
      w.names = names(widgets.default.values),
      rv.custom.names = names(rv.custom.default.values)
          )
        )
      )

    "

    code
}

#' @noRd
get_process_renderUI_for_steps <- function(steps) {
    code <- NULL

    for (i in names(steps)) {
      if (i == 'Description'){
        code <- paste0(
          code,
          Insert_Description_Step_code()
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

    code
}


#' @title xxx
#' @description This function inserts the necessary code for the 'Description' step
#' @noRd
#' 
Insert_Description_Step_code <- function(){

  
  code <- "
# >>>
# >>> START ------------- Code for Description UI---------------
# >>> 


output$Description <- renderUI({
  file <- paste0(config@path_to_md_dir, '/', id, '.md')
  
  tagList(
    # In this example, the md file is found in the module_examples directory
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
  dataOut$trigger <- Magellan::Timestamp()
  dataOut$value <- rv$dataIn
  rv$steps.status['Description'] <- global$VALIDATED
})


"




code
}



#' @noRd
get_process_output_func <- function() {
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


###
###
### Functions specific to pipelines
###
###


#' @noRd
#'
get_pipeline_ui_function <- function(name) {
    code <- "
    mod_#name#_ui <- function(id){
    ns <- NS(id)
    }

    "

    gsub("#name#", name, code)
}

#' @noRd
#'
get_pipeline_header_server_func <- function(name) {
    code <- "

    mod_#name#_server <- function(id,
        dataIn = reactive({NULL}),
        steps.enabled = reactive({NULL}),
        remoteReset = reactive({FALSE}),
        steps.status = reactive({NULL}),
        current.pos = reactive({1})
        ){
    "
    gsub("#name#", name, code)
}


#' @noRd
#'
get_pipeline_config_code <- function(config) {
    code <- "
    # This list contains the basic configuration of the process
    config <- Config(
    # Define the type of module
    mode = 'process',
    name = '#name#',
    # List of all steps of the process
    steps = #steps#,

    # A vector of boolean indicating if the steps are mandatory or not.
    mandatory = #mandatory#
    )
    "

    code <- gsub("#name#", vec2code(config@name, TRUE), code)
    code <- gsub("#steps#", vec2code(config@steps, TRUE), code)
    code <- gsub("#mandatory#", vec2code(config@mandatory, FALSE), code)
    code
}




#' @noRd
#'
get_pipeline_module_server <- function() {
    code <- "


    ###-------------------------------------------------------------###
    ###                                                             ###
    ### ------------------- MODULE SERVER --------------------------###
    ###                                                             ###
    ###-------------------------------------------------------------###
    moduleServer(id, function(input, output, session) {
        ns <- session$ns


    eval(
      str2expression(
        Get_Worflow_Core_Code(
      w.names = names(widgets.default.values),
          rv.custom.names = names(rv.custom.default.values)
          
          )
        )
      )

        # Insert code for the description renderUI()
    #eval(parse(text = Get_Code_for_module_Description(config@name)),
    #    envir = .GlobalEnv)

    # Insert necessary code which is hosted by Magellan
    # DO NOT MODIFY THIS LINE
    eval(parse(text = Module_Return_Func()))
    }
    )
}

    "

    code
}
