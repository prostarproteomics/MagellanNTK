Get_Code_Update_Config <- function(){

  code <- "

  config$steps <- c('Description', config$steps)
  config$steps <- setNames(config$steps, nm = config$steps)
  config$mandatory <- c(TRUE, config$mandatory)


    "
  
  
  # code <- "
  # 
  #  config$steps <- c(paste0(config$name, '_Description'), config$steps)
  #  config$steps <- setNames(config$steps,
  #                             nm = gsub(paste0(config$name, '_'), '', config$steps))
  #    config$mandatory <- c(TRUE, config$mandatory)
  # 
  # "
  
  code
}



#' @title xxx
#' 
#' @description 
#' 
#' @param id xxx
#' 
#' 
Get_Code_for_module_Description <- function(id){
  
  code <- "
  
  mod_replaceId_Description_ui <- function(id){
    ns <- NS(id)
  }
  
  
  
  mod_replaceId_Description_server <- function(id,
                                             dataIn = reactive({NULL}),
                                             steps.enabled = reactive({NULL}),
                                             remoteReset = reactive({FALSE}),
                                             steps.status = reactive({NULL}),
                                             current.pos = reactive({1})
                                            ){

  config <- list(
    mode = 'process',
    
    name = 'Description',
    
    # List of all steps of the process
    steps = c('Description'),
    # A vector of boolean indicating if the steps are mandatory or not.
    mandatory = c(TRUE)
  )
  
  # Define default selected values for widgets
  # By default, this list is empty for the Description module
  # but it can be customized
  widgets.default.values <- list()
  
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
    config$steps <- setNames(config$steps, nm = config$steps)
    
    eval(parse(text = SimpleWorflowCoreCode(
    name = config$name,
    widgets = names(widgets.default.values),
    steps = config$steps )))
    
     
    ###### ------------------- Code for Description (step 0) -------------------------    #####
    output$Description <- renderUI({
      tagList(
        includeMarkdown(system.file('scripts/module_examples/md/', 
                                    paste0(id, '_Description.md'), 
                                    package='Magellan')),
        
        uiOutput(ns('datasetDescription')),
        
        # Insert validation button
        uiOutput(ns('Description_validationBtn_ui'))
      )
    })

    # Insert necessary code which is hosted by Magellan
    # DO NOT MODIFY THIS LINE
    eval(parse(text = Module_Return_Func()))

  }
  )
}

  
  "

code <- gsub('replaceId', id, code)
code

}






Get_Code_for_Description_renderUI <- function(id){
  
  code <- "
  
  
  output$Description <- renderUI({
      tagList(
        # In this example, the md file is found in the module_examples directory
        # but with a real app, it should be provided by the package which
        # contains the UI for the different steps of the process module.
        # system.file(xxx)
        
        includeMarkdown(system.file('scripts/module_examples/md/', 
                                    paste0('replaceid', '.md'), 
                                    package = 'Magellan'
                                    )
                      ),
        
       # Used to show some information about the dataset which is loaded
       # This function must be provided by the package of the process module
       uiOutput(ns('datasetDescription')),
        
        # Insert validation button
        uiOutput(ns('Description_validationBtn_ui'))
      )
    })
    
    
    "
  
  gsub('replaceid', id, code)
  
  
}


#' @title Code for declaring widgets.default.values reactive variable
#' 
#' @description This function create the source code needed inside a module
#' to declare the reactive variable called 'widgets.default.values'.
#' # Declaration of the variables that will contain the values of the widgets
#' To avoid confusion, the first string is the name of the step while the second is the name
#' of the widget
#' 
#' @param widgets.names xxx
#' 
#' @author Samuel Wieczorek
#' 
#' @examples 
#' \dontrun{
#' widgets <- paste0('widget', 1:3)
#' code <- Get_Code_Declare_widgetsDefaultValues(widgets)
#' cat(code)
#' }
#' 
Get_Code_Declare_widgetsDefaultValues <- function(widgets.names=NULL){
  # If one is on a composed workflow which do not have explicit ui
  if (is.null(widgets.names))
    declare_rv_widgets <- "rv.widgets <- reactiveValues()\n\n"
  else {
    basis <- "w.name = widgets.default.values$w.name"
    ls_list <- lapply(widgets.names,
                      function(x) gsub('w.name', x, basis) )
    declare_rv_widgets <- paste0("rv.widgets <- reactiveValues(\n", 
                                 paste0("\t", ls_list, sep="", collapse= ",\n"),
                                 "\n)\n\n")
  }
  
  declare_rv_widgets
}






#' @title Code for declaring xxx
#' 
#' @description This function xxx
#' # Generate dynamically the observeEvent function for each widget
#' 
#' @param widgets.names xxx
#' 
#' @author Samuel Wieczorek
#' 
#' @examples 
#' \dontrun{
#' widgets <- paste0('widget', 1:3)
#' code <- Get_Code_for_ObserveEvent_widgets(widgets)
#' cat(code)
#' }
#' 
Get_Code_for_ObserveEvent_widgets <- function(widgets.names = NULL){
  
  declare_rv_widgets <- NULL
  if(!is.null(widgets.names)){
    basis <- "observeEvent(input$widget.name, {rv.widgets$widget.name <- input$widget.name})"
    ls_list <- lapply(widgets.names, 
                      function(x) gsub('widget.name', x, basis)
    )
    
    declare_rv_widgets <- paste0(ls_list, collapse= "\n")
    declare_rv_widgets <- paste0(declare_rv_widgets, "\n\n\n")
  }
  declare_rv_widgets
}




#' @title Code for declaring widgets.default.values reactive variable
#' 
#' @description This function createxxx
#' 
#' @author Samuel Wieczorek
#' 
#' @examples 
#' \dontrun{
#' code <- Get_Code_for_rv_reactiveValues()
#' cat(code)
#' }
#' 
Get_Code_for_rv_reactiveValues <- function(){
  basis <- "rv <- reactiveValues(
    # Stores the object given in input of the process
    dataIn = NULL,
    # A vector of boolean indicating the status (UNDONE, SKIPPED or VALIDATED) of the steps
    steps.status = NULL,
    # xxx
    reset = NULL,
    # A vector of boolean indicating if the steps are enabled or disabled
    steps.enabled = NULL
  )
  
  "
  basis
}


#' @title Code for xxx
#' 
#' @description Returned value of the process
# * The trigger variable is used to trigger an event that can be catched by 
# the Shiny functions observe() and observeEvent()
# * The value variable contains the object to return to the instance that 
# has called the process.
# DO NOT MODIFY THIS FUNCTION
#' 
#' @author Samuel Wieczorek
#' 
#' @examples
#' \dontrun{
#' code <- Get_Code_for_dataOut()
#' cat(code)
#' }
#' 
Get_Code_for_dataOut <- function(){
  code <- "dataOut <- reactiveValues(
  trigger = NULL,
  value = NULL
)

"

code
}

#' @title Code for declaring xxx
#' 
#' @description xxx
#' 
#' @author Samuel Wieczorek
#' 
#' @examples 
#' \dontrun{
#' code <- Get_Code_for_observeEven_stepsEnabled()
#' cat(code)
#' }
#' 
Get_Code_for_observeEven_stepsEnabled  <- function(){
  code <- "observeEvent(steps.enabled(), ignoreNULL = TRUE, {
  if (is.null(steps.enabled()))
    rv$steps.enabled <- setNames(rep(FALSE, rv$length), 
                                 rv$config$steps)
  else
    rv$steps.enabled <- steps.enabled()
})

"
  
  code
}


#' @title Code for xxxx
#' 
#' @description xxxx
#' 
#' @author Samuel Wieczorek
#' 
#' @examples 
#' \dontrun{
#' code <- Get_Code_for_observeEvent_remoteReset()
#' cat(code)
#' }
#' 
Get_Code_for_observeEvent_remoteReset <- function(){
  code <- "observeEvent(remoteReset(), {
  lapply(names(rv.widgets), function(x){
    rv.widgets[[x]] <- widgets.default.values[[x]]
  })
})
  
  "
  code
}


#' @title Code for xxxx
#' 
#' @description xxxx
#' 
#' @author Samuel Wieczorek
#' 
#' @examples 
#' \dontrun{
#' code <- Code_ObserveEvent_ValidationBtns()
#' cat(code)
#' }
#' 
Code_ObserveEvent_ValidationBtns <- function(){
  code <- "
  # Observer for the validation buttons of all steps
  # DO NOT MODIFY THIS FUNCTION
  observeEvent(lapply(config$steps, function(x) input[[paste0(x, \"_btn_validate\")]]),
               ignoreInit = TRUE,
               ignoreNULL = TRUE,
               {
                 #browser()
                 test <- lapply(config$steps, function(x) input[[paste0(x, \"_btn_validate\")]])
                 if( sum(unlist(test)) != 1)
                   return()
                 
                 # Last step
                 if (current.pos() == length(config$steps)){
                   rv$dataIn <- Add_Datasets_to_Object(object = rv$dataIn,
                                                       dataset = rnorm(1:5),
                                                       name = id)
                 }
                 
                 # First step (Description)
                 if (current.pos() == 1 ){
                   rv$dataIn <- dataIn()
                 }
                 
                 dataOut$trigger <- Magellan::Timestamp()
                 dataOut$value <- rv$dataIn
                 rv$steps.status[current.pos()] <- global$VALIDATED
               })
               
               "
  
  code
}


#' @title Code for validation buttons renderUI()
#' 
#' @description This function Generates dynamically the observeEvent function for each widget
#' 
#' @param steps xxx
#' 
#' @author Samuel Wieczorek
#' 
#' @examples 
#' \dontrun{
#' steps <- paste0('step', 1:3)
#' code <- Generate_code_for_ValidationBtns_renderUI(steps)
#' cat(code)
#' }
#' 
Generate_code_for_ValidationBtns_renderUI <- function(steps){
  code <- "# Buttons must be explicitly enabled/disabled with a full code
  # Otherwise, they do not disable
  # DO NOT MODIFY THIS FUNCTION
  
  output$step.name_validationBtn_ui <- renderUI({
      if (isTRUE(rv$steps.enabled[\"step.name\"])  )
        actionButton(ns(\"step.name_btn_validate\"),
                     \"label\",
                     class = btn_success_color)
      else
        shinyjs::disabled(
          actionButton(ns(\"step.name_btn_validate\"),
                       \"label\",
                       class = btn_success_color)
        )
    })
    
    "
  
  ls_list <- lapply(steps, function(x) {
    code <- gsub("step.name", x, code)
    
    if (x == 'Description')
      new.label <- 'Start '
    else if (x == 'Save')
      new.label <- 'Save '
    else
      new.label <- 'Perform '
    
    code <- gsub('label', paste0(new.label, x, sep = " "), code)
  }
  )
  
  code <- paste0(ls_list, collapse= "\n")
  code
}



#' @title Code for declaring xxx
#' 
#' @description This function Generate dynamically the observeEvent function for each widget
#' 
#' @param widgets xxx
#' 
#' @author Samuel Wieczorek
#' 
#' @examples 
#' \dontrun{
#' widgets <- paste0('widget', 1:3)
#' code <- Generate_RenderUI_Code_For_Single_Widgets(widgets)
#' cat(code)
#' }
#'
Generate_RenderUI_Code_For_Single_Widgets <- function(widgets=NULL){
  code <- NULL 
  if(!is.null(widgets)){
    
    
    code <- "output$widget.name_ui <- renderUI({
      if (rv$steps.enabled[\"step.name\"])
        widget_widget.name()
      else
        shinyjs::disabled(widget_widget.name())
    })
    
    
    "
    
    ls_list <- lapply(widgets, 
                      function(x) {
                        step.name <- unlist(strsplit(x, split='_'))[1]
                        code <- gsub('widget.name', x, code)
                        code <- gsub('step.name', step.name, code)
                      }
    )
    
    code <- paste0(ls_list, collapse= "\n")
  }
  code
}



#' @title Code for declaring xxx
#' 
#' @description This function xxx
#' # Generate dynamically the observeEvent function for each widget
#' 
#' @author Samuel Wieczorek
#' 
#' @examples 
#' \dontrun{
#' code <- Module_Return_Func()
#' cat(code)
#' }
#' 
Module_Return_Func <- function(){
  
  code <- "# Return value of module
# DO NOT MODIFY THIS PART
list(config = reactive({
  config$ll.UI <- setNames(lapply(config$steps,
                                  function(x){
                                    do.call(\"uiOutput\", list(ns(x)))
                                  }),
                           paste0(\"screen_\", config$steps)
  )
  config
}),
dataOut = reactive({dataOut})
#steps.status = reactive({rv$steps.status})
)


"

code

}

#' @title Code for declaring xxx
#' 
#' @description This function xxx
#' # Generate dynamically the observeEvent function for each widget
#' 
#' @param widgets xxx
#' 
#' @param steps xxx
#' 
#' @author Samuel Wieczorek
#' 
#' @examples 
#' \dontrun{
#' widgets <- paste0('widget', 1:3)
#' steps <- paste0('step', 1:3)
#' code <- ComposedeWorflowCoreCode(widgets, steps)
#' cat(code)
#' }
#' 
ComposedeWorflowCoreCode <- function(name, steps){
  core <- paste0(Get_Code_Declare_widgetsDefaultValues(),
                 Get_Code_for_ObserveEvent_widgets(),
                 Get_Code_for_rv_reactiveValues(),
                 Get_Code_for_dataOut(),
                 Get_Code_for_observeEven_stepsEnabled(),
                 Get_Code_for_observeEvent_remoteReset(),
                 Code_ObserveEvent_ValidationBtns(),
                 Generate_code_for_ValidationBtns_renderUI(steps),
                 Generate_RenderUI_Code_For_Single_Widgets(),
                 
                 sep = "\n"
  )
  core
}




#' @title Code for declaring xxx
#' 
#' @description This function xxx
#' # Generate dynamically the observeEvent function for each widget
#' 
#' @param widgets xxx
#' 
#' @param steps xxx
#' 
#' @author Samuel Wieczorek
#' 
#' @examples 
#' \dontrun{
#' widgets <- paste0('widget', 1:3)
#' steps <- paste0('step', 1:3)
#' code <- SimpleWorflowCoreCode(widgets, steps)
#' cat(code)
#' }
#' 
SimpleWorflowCoreCode <- function(name, widgets, steps){
  core <- paste0(
    #Get_Code_Update_Config(),
                 Get_Code_Declare_widgetsDefaultValues(widgets),
                 Get_Code_for_ObserveEvent_widgets(widgets),
                 Get_Code_for_rv_reactiveValues(),
                 Get_Code_for_dataOut(),
                 Get_Code_for_observeEven_stepsEnabled(),
                 Get_Code_for_observeEvent_remoteReset(),
                 Code_ObserveEvent_ValidationBtns(),
                 Generate_code_for_ValidationBtns_renderUI(steps),
                 Generate_RenderUI_Code_For_Single_Widgets(widgets),
                 Get_Code_for_Description_renderUI(name),
                 sep = "\n"
  )
  core
}