#' @title Create process template code
#' 
#' @description xxx
#' 
#' @param config xxx
#' 
#' @examples 
#' conf <- list(parent = "pipeline",
#' name = "process",
#' steps = c('Description', "Step 1", "Step 2", "Save"),
#' mandatory = c(TRUE, TRUE, FALSE, TRUE)
#' )
#' createProcessTemplateForExpert(conf)
#' 
#' @author Samuel Wieczorek
#' 
#' @export
#' 
createProcessTemplateForExpert <- function(config = NULL){
  
  # Check config integrity
  check <- CheckConfig(config)
  if (!check$passed)
    stop(paste0("Errors in 'config'", paste0(check$msg, collapse=' ')))
  
  mod.filename <- paste0('mod_', config$parent, '_', config$name, '.R')
  if (file.exists(mod.filename))
    file.remove(mod.filename)
  con <- file(mod.filename, open = 'a')
  
  write_ui_function(con, config$parent, config$name)
  writeLines("", con)
  writeLines("", con)
  write_header_server_func(con, config$parent, config$name)
  write_config_code(con, config)
  write_code_for_default_value_widgets(con)
  write_module_server_header(con)
  write_declaration_rv_widgets(con)

  write_rv_reactiveVariable(con)
  write_dataOut_variable(con)
  write_observeEvents(con)
  write_code_step0(con)
  write_return_value(con)
  
  
  
  close(con)
}


#' @noRd
#' 
write_ui_function <- function(con, parent, name){
  writeLines(paste0('mod_', parent, '_', name, '_ui <- function(id){'), con)
  writeLines("\tns <- NS(id)", con)
  writeLines("}", con)
}

#' @noRd
#' 
write_header_server_func <- function(con, parent, name){
  writeLines(paste0('mod_', parent, '_', name, '_server <- function(id,'), con)
  writeLines("\t\tdataIn = reactive({NULL}),", con)
  writeLines("\t\tsteps.enabled = reactive({NULL}),", con)
  writeLines("\t\tremoteReset = reactive({FALSE})", con)
  writeLines("\t\t){", con)
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
write_config_code <- function(con, config){
  writeLines('', con)
  writeLines("\t# This list contains the basic configuration of the process", con)
  writeLines("\tconfig <- list(", con)
  writeLines("\t\t# Name of the process", con)
  writeLines(paste0("\t\t", "name = '", config$name, "',"), con)
  writeLines("\t\t# Name of the pipeline it belongs to", con)
  writeLines(paste0("\t\t", "parent = '", config$parent, "',"), con)
  writeLines("\t\t# List of all steps of the process", con)
  
  writeLines(paste0("\t\t", "steps = ", vec2code(config$steps, TRUE), ","), con)
  
  writeLines("\t\t# A vector of boolean indicating if the steps are mandatory or not.", con)
  writeLines(paste0("\t\t", "mandatory = ", vec2code(config$mandatory, FALSE)), con)
  writeLines("\t\t)", con)
}


#' @noRd
#' 
write_code_for_default_value_widgets <- function(con){
  writeLines('', con)
  writeLines("\t# # Define default selected values for widgets", con)
  writeLines("\twidgets.default.values <- list(", con)
  writeLines("\t\t# The following lines are given as example. Our advice is to use the ", con)
  writeLines("\t\tsame nomenclature for the variables of widgets:", con)
  writeLines("\t\ttwo strings separated by '_', the first one is the name of the step", con)
  writeLines("\t\t while the secondone is the nameof the widget", con)
  writeLines("\t\t# Step1_select1 = 1,", con)
  writeLines("\t\t# Step1_select2 = NULL,", con)
  writeLines("\t\t# Step1_select3 = 1,", con)
  writeLines("\t\t# Step2_select2_1 = 1,", con)
  writeLines("\t# Step2_select2_2 = 1", con)
  writeLines("\t)", con)
}


#' @noRd
#' 
write_module_server_header <- function(con){
  writeLines('', con)
  writeLines("\t###-------------------------------------------------------------###", con)
  writeLines("\t###                                                             ###", con)
  writeLines("\t### ------------------- MODULE SERVER --------------------------###", con)
  writeLines("\t###                                                             ###", con)
  writeLines("\t###-------------------------------------------------------------###", con)
  writeLines("\tmoduleServer(id, function(input, output, session) {", con)
  writeLines("\t\t  ns <- session$ns", con)
 }
  
#' @noRd
#' 
write_declaration_rv_widgets <- function(con){
  writeLines("", con)
  writeLines("\t\t# Declaration of the variables that will contain the values of the widgets", con)
  writeLines("\t\t# To avoid confusion, the first string is the name of the step while the second is the name", con)
  writeLines("\t\t# of the widget", con)
  writeLines("\t\trv.widgets <- reactiveValues(", con)
  writeLines("\t\t\t#Step1_select1 = widgets.default.values$Step1_select1,", con)
  writeLines("\t\t\t#Step1_select2 = widgets.default.values$Step1_select2,", con)
  writeLines("\t\t\t#Step1_select3 = widgets.default.values$Step1_select3,", con)
  writeLines("\t\t\t#Step2_select2_1 = widgets.default.values$Step2_select2_1,", con)
  writeLines("\t\t\t#Step2_select2_2 = widgets.default.values$Step2_select2_2", con)
  writeLines("\t\t)", con)
}




#' @noRd
#' 
write_rv_reactiveVariable <- function(con){
  writeLines("", con)
  writeLines("\t\t# Reactive values during the run of the process", con)
  writeLines("\t\trv <- reactiveValues(", con)
  writeLines("\t\t\t# Stores the object given in input of the process", con)
  writeLines("\t\t\t  dataIn = NULL,", con)
  writeLines("\t\t\t  # A vector of boolean indicating the status (UNDONE, SKIPPED or VALIDATED) of the steps", con)
  writeLines("\t\t\t  steps.status = NULL,", con)
  writeLines("\t\t\t  # xxx", con)
  writeLines("\t\t\t  reset = NULL,", con)
  writeLines("\t\t\t # A vector of boolean indicating if the steps are enabled or disabled", con)
  writeLines("\t\t\t  steps.enabled = NULL", con)
  writeLines("\t\t)", con)
}


#' @noRd
#' 
write_dataOut_variable <- function(con){
  writeLines("", con)
  writeLines("\t\t# Returned value of the process", con)
  writeLines("\t\t# * The trigger variable is used to trigger an event that can be catched by the ", con)
  writeLines("\t\t#   Shiny functions observe() and observeEvent()", con)
  writeLines("\t\t# * The value variable contains the object to return to the instance that has called the process.", con)
  writeLines("\t\tdataOut <- reactiveValues(", con)
  writeLines("\t\t\t  trigger = NULL,", con)
  writeLines("\t\t\t  value = NULL", con)
  writeLines("\t\t\t)", con)
  
}

#' @noRd
#' 
write_observeEvents <- function(con){
  writeLines("", con)
  writeLines("\t# Initialization of the module", con)
  writeLines("\t\tobserveEvent(steps.enabled(), ignoreNULL = TRUE, {", con)
  writeLines("\t\tif (is.null(steps.enabled()))", con)
  writeLines("\t\t\trv$steps.enabled <- setNames(rep(FALSE, rv$length), ", con)
  writeLines("\t                                  rv$config$steps)", con)
  writeLines("\t\t  else", con)
  writeLines("\t    rv$steps.enabled <- steps.enabled()", con)
  writeLines("\t\t})", con)
  writeLines('', con)
  writeLines("\t# Set all the widgets to their default value after the remote Reset()", con)
  writeLines("\tobserveEvent(remoteReset(), {", con)
  writeLines("\t  lapply(names(rv.widgets), function(x){", con)
  writeLines("\t    rv.widgets[[x]] <- widgets.default.values[[x]]", con)
  writeLines("\t  })", con)
  writeLines("\t})", con)
}



#' @noRd
#' 
write_code_step0 <- function(con){
  writeLines("", con)
  writeLines("\t  ###### ------------------- Code for Description (step 0) -------------------------    #####", con)
  writeLines("\t  output$Description <- renderUI({", con)
  writeLines("\t  tagList(", con)
  writeLines("\t    includeMarkdown(paste0('md/', paste0(config$parent, '_', config$name, '.md'))),", con)
  writeLines("\t    uiOutput(ns('datasetDescription')),", con)
  writeLines("\t    uiOutput(ns('validationBtn_ui'))", con)
  writeLines("\t  )", con)
  writeLines("\t})", con)
  writeLines("", con)
  writeLines("", con)
  writeLines("\tobserveEvent(input$btn_validate_Description, ignoreInit = TRUE, ignoreNULL = TRUE, {", con)
  writeLines("\t  rv$dataIn <- dataIn()", con)
  writeLines("\t  rv$steps.status['Description'] <- global$VALIDATED", con)
  writeLines("\t  dataOut$trigger <- Magellan::Timestamp()", con)
  writeLines("\t  dataOut$value <- rv$dataIn", con)
  writeLines("\t})", con)
  writeLines("", con)
  writeLines("\toutput$validationBtn_ui <- renderUI({", con)
  writeLines("\t  if (isTRUE(rv$steps.enabled['Description'])  )", con)
  writeLines("\t    actionButton(ns('btn_validate_Description'),", con)
  writeLines("\t                 paste0('Start ', config$name),", con)
  writeLines("\t                 class = btn_success_color)", con)
  writeLines("\t  else", con)
  writeLines("\t    shinyjs::disabled(", con)
  writeLines("\t      actionButton(ns('btn_validate_Description'),", con)
  writeLines("\t                   paste0('Start ', config$name),", con)
  writeLines("\t                   class = btn_success_color)", con)
  writeLines("\t    )", con)
  writeLines("\t})", con)
  
}


#' @noRd
#' 
write_code_step1 <- function(con){
  writeLines("", con)
  writeLines("\t  ###### ------------------- Code for step 1 -------------------------    #####", con)
  writeLines("\t  # ObserveEvent of the widgets", con)
  writeLines("\t  observeEvent(input$select1, {rv.widgets$Step1_select1 <- input$select1})", con)
  writeLines("\t  observeEvent(input$select2, {rv.widgets$Step1_select2 <- input$select2})", con)
  writeLines("\t  observeEvent(input$select3, {rv.widgets$Step1_select3 <- input$select3})", con)
  writeLines("\t  observeEvent(input$select2_1, {rv.widgets$Step2_select1 <- input$select2_1})", con)
  writeLines("\t  observeEvent(input$select2_2, {rv.widgets$Step2_select2 <- input$select2_2})", con)
  writeLines('', con)
  
  
  
  writeLines("\t  output$test1 <-renderUI({", con)
  writeLines("\t\t  #rv$steps.enabled", con)
  writeLines("\t\t  rv.widgets$select1", con)
  writeLines("\t\t    if (rv$steps.enabled['Step1'])", con)
  writeLines("\t\t      selectInput(ns('select1'), 'Select 1 in renderUI',", con)
  writeLines("\t\t                  choices = 1:4,", con)
  writeLines("\t\t                  selected = rv.widgets$Step1_select1,", con)
  writeLines("\t\t                  width = '150px')", con)
  writeLines("\t\t    else", con)
  writeLines("\t\t      shinyjs::disabled(", con)
  writeLines("\t\t        selectInput(ns('select1'), 'Select 1 in renderUI',", con)
  writeLines("\t\t                    choices = 1:4,", con)
  writeLines("\t\t                    selected = rv.widgets$Step1_select1,", con)
  writeLines("\t\t                    width = '150px')", con)
  writeLines("\t\t      )", con)
  writeLines("\t\t  })", con)
  
  
  writeLines('', con)
  writeLines("\t  output$test2 <-renderUI({", con)
    
  writeLines("\t\t    rv$steps.enabled", con)
  writeLines("\t\t  if (rv$steps.enabled['Step1'])", con)
  writeLines("\t\t    selectInput(ns('select2'), 'Select 2 in renderUI',", con)
  writeLines("\t\t                choices = 1:3,", con)
  writeLines("\t\t                selected = rv.widgets$Step1_select2,", con)
  writeLines("\t\t                width = '150px')", con)
  writeLines("\t\t  else", con)
  writeLines("\t\t    shinyjs::disabled(", con)
  writeLines("\t\t      selectInput(ns('select2'), 'Select 2 in renderUI',", con)
  writeLines("\t\t                  choices = 1:4,", con)
  writeLines("\t\t                  selected = rv.widgets$Step1_select2,", con)
  writeLines("\t\t                  width = '150px')", con)
  writeLines("\t\t    )", con)
  writeLines("\t\t})", con)
  
  writeLines("\t# Buttons must be explicitly enabled/disabled with a full code", con)
  writeLines("\t# Otherwise, they do not disable", con)
  writeLines("\toutput$btn1_ui <- renderUI({", con)
  writeLines("\t\t  if (rv$steps.enabled['Step1'])", con)
  writeLines("\t\t    actionButton(ns('btn1'),", con)
  writeLines("\t\t                 'btn1',", con)
  writeLines("\t\t                 class = btn_success_color)", con)
  writeLines("\t\t  else", con)
  writeLines("\t\t    shinyjs::disabled(", con)
  writeLines("\t\t      actionButton(ns('btn1'),", con)
  writeLines("\t\t                   'btn1',", con)
  writeLines("\t\t                   class = btn_success_color)", con)
  writeLines("\t\t    )", con)
  writeLines("\t\t})", con)
  
  writeLines("\t# ------------------------ STEP 1 : UI ------------------------------------", con)
  writeLines("\toutput$Step1 <- renderUI({", con)
  writeLines("\t\t  name <- 'Step1'", con)
  writeLines("\t\t  wellPanel(id = ns('toto'),", con)
  writeLines("\t\t\t            uiOutput(ns('btn1_ui')),", con)
              
  writeLines("\t\t\t            tagList(", con)
  writeLines("\t\t\t\t            div(id=ns('Step1a'),", con)
  writeLines("\t\t\t\t            div(style='display:inline-block; vertical-align: middle;padding-right: 20px;',", con)
  writeLines("\t\t\t\t                      uiOutput(ns('test1'))", con)
  writeLines("\t\t\t\t                  ),", con)
  writeLines("\t\t\t\t                  div(style='display:inline-block; vertical-align: middle;padding-right: 20px;',", con)
  writeLines("\t\t\t\t                      uiOutput(ns('test2'))", con)
  writeLines("\t\t\t\t                  ),", con)
  writeLines("\t\t\t\t                  div(style='display:inline-block; vertical-align: middle; padding-right: 40px;',", con)
  writeLines("\t\t\t\t                      if (rv$steps.enabled['Step1'])", con)
  writeLines("\t\t\t\t                        selectInput(ns('select3'), 'Select step 3',", con)
  writeLines("\t\t\t\t                                    choices = 1:3,", con)
  writeLines("\t\t\t\t                                    selected = rv.widgets$Step1_select3,", con)
  writeLines("\t\t\t\t                                    width = '150px')", con)
  writeLines("\t\t\t\t                      else", con)
  writeLines("\t\t\t\t                        shinyjs::disabled(", con)
  writeLines("\t\t\t\t                          selectInput(ns('select3'), 'Select step 3',", con)
  writeLines("\t\t\t\t                                      choices = 1:5,", con)
  writeLines("\t\t\t\t                                      selected = rv.widgets$Step1_select3,", con)
  writeLines("\t\t\t\t                                      width = '150px')", con)
  writeLines("\t\t\t\t                        )", con)
  writeLines("\t\t\t\t                   ),", con)
  writeLines("\t\t\t                  div(style='display:inline-block; vertical-align: middle;padding-right: 20px;',", con)
  writeLines("\t\t\t\t                      if (rv$steps.enabled['Step1'])", con)
  writeLines("\t\t\t\t                        actionButton(ns(paste0('btn_validate_', name)),", con)
  writeLines("\t\t\t\t                                     'Perform',", con)
  writeLines("\t\t\t\t                                     class = btn_success_color)", con)
  writeLines("\t\t\t\t                      else", con)
  writeLines("\t\t\t                        shinyjs::disabled(", con)
  writeLines("\t\t\t\t                          actionButton(ns(paste0('btn_validate_', name)),", con)
  writeLines("\t\t\t\t                                       'Perform',", con)
  writeLines("\t\t\t\t                                       class = btn_success_color)", con)
  writeLines("\t\t\t\t                        )", con)
  writeLines("\t\t\t\t                  )", con)
  writeLines("\t\t\t\t              )", con)
  writeLines("\t\t\t\t            )", con)
  writeLines("\t\t\t\t  )", con)
  writeLines("\t\t\t\t})", con)
  writeLines('', con)
  
  writeLines("\t\t\tobserveEvent(input$btn_validate_Step1, ignoreInit = TRUE, {", con)
  writeLines("\t\t\t\t  # Add your stuff code here", con)
  writeLines("\t\t\t\t  # dataOut$trigger <- Send_Result_to_Caller(rv$dataIn)$trigger", con)
  writeLines("\t\t\t\t  # dataOut$value <- Send_Result_to_Caller(rv$dataIn)$value", con)
  writeLines("\t\t\t\t  dataOut$trigger <- Magellan::Timestamp()", con)
  writeLines("\t\t\t\t  dataOut$value <- rv$dataIn", con)
  writeLines("\t\t\t\t   rv$steps.status['Step1'] <- global$VALIDATED", con)
  writeLines("\t\t\t\t})", con)
  }


#' @noRd
#' 
write_return_value <- function(con){
  writeLines("", con)
  writeLines("\t# Return value of module", con)
  writeLines("\t# DO NOT MODIFY THIS PART", con)
  writeLines("\tlist(config = reactive({", con)
  writeLines("\t  config$ll.UI <- setNames(lapply(config$steps,", con)
  writeLines("\t                                  function(x){", con)
  writeLines("\t                                    do.call('uiOutput', list(ns(x)))", con)
  writeLines("\t                                  }),", con)
  writeLines("\t                           paste0('screen_', config$steps)", con)
  writeLines("\t\t\t\t  )", con)
  writeLines("\t\t\t  config", con)
  writeLines("\t\t\t}),", con)
  writeLines("\t\tdataOut = reactive({dataOut})", con)
  writeLines("\t#steps.status = reactive({rv$steps.status})", con)
  writeLines("\t)", con)
  
  writeLines("\t}", con)
  writeLines("\t)", con)
}
