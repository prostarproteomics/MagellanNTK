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
Get_Code_Declare_widgetsDefaultValues <- function(widgets.names){
  basis <- "w.name = widgets.default.values$w.name"
  ls_list <- lapply(widgets.names,
                    function(x) gsub('w.name', x, basis) )
  declare_rv_widgets <- paste0("rv.widgets <- reactiveValues(\n", 
                               paste0("\t", ls_list, sep="", collapse= ",\n"),
                               "\n)\n\n")
  
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
Get_Code_for_ObserveEvent_widgets <- function(widgets.names){

basis <- "observeEvent(input$widget.name, {rv.widgets$widget.name <- input$widget.name})"
ls_list <- lapply(widgets.names, 
                  function(x) gsub('widget.name', x, basis)
                  )

declare_rv_widgets <- paste0(ls_list, collapse= "\n")
declare_rv_widgets <- paste0(declare_rv_widgets, "\n\n\n")
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
  code <- "# Observer for the validation buttons of all steps
  # DO NOT MODIFY THIS FUNCTION
  observeEvent(lapply(config$steps, function(x) input[[paste0('btn_validate_', x)]]),
               ignoreInit = TRUE,
               ignoreNULL = TRUE,
               {
                 test <- lapply(config$steps, function(x) input[[paste0('btn_validate_', x)]])
                 if( sum(unlist(test)) != 1)
                   return()
                 
                 if (current.pos() == length(config$steps)){
                   rv$dataIn <- Add_Datasets_to_Object(object = rv$dataIn,
                                                       dataset = rnorm(1:5),
                                                       name = config$name)
                 }
                 
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
      if (isTRUE(rv$steps.enabled['step.name'])  )
        actionButton(ns('btn_validate_step.name'),
                     'label',
                     class = btn_success_color)
      else
        shinyjs::disabled(
          actionButton(ns('btn_validate_step.name'),
                       'label',
                       class = btn_success_color)
        )
    })
    
    "
  
  ls_list <- lapply(steps, function(x) {
    code <- gsub("step.name", x, code)
     if (x == 'Description')
      code <- gsub('label', paste0('Start ', x, sep = " "), code)
    else 
      code <- gsub('label', paste0('Perform ', x, sep = " "), code)
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
Generate_RenderUI_Code_For_Single_Widgets <- function(widgets){
    
    code <- "output$widget.name_ui <- renderUI({
      if (rv$steps.enabled['step.name'])
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
                                    do.call('uiOutput', list(ns(x)))
                                  }),
                           paste0('screen_', config$steps)
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
#' code <- ModuleCoreCode(widgets, steps)
#' cat(code)
#' }
#' 
ModuleCoreCode <- function(widgets, steps){
  core <- paste0(Get_Code_Declare_widgetsDefaultValues(widgets),
                 Get_Code_for_ObserveEvent_widgets(widgets),
                 Get_Code_for_rv_reactiveValues(),
                 Get_Code_for_dataOut(),
                 Get_Code_for_observeEven_stepsEnabled(),
                 Get_Code_for_observeEvent_remoteReset(),
                 Code_ObserveEvent_ValidationBtns(),
                 Generate_code_for_ValidationBtns_renderUI(steps),
                 Generate_RenderUI_Code_For_Single_Widgets(widgets),
                 sep = "\n"
                 )
  core
}