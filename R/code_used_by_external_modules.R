#' @title R code to update the 'config' variable of a process module
#'
#' @description This function generates the necessary code to
#' modify the variable 'config' (slots steps and mandatory). It adds
#' a 'Description' step and a TRUE value at the beginning of the 'steps'
#' and 'mandatory'
#'  list, erases all white spaces for the names of the steps.
#'
#' @name insertCodeForExternalModules
#' 
NULL








#' @title R code to update the 'config' variable of a process module
#'
#' @description This function generates the necessary code to
#' modify the variable 'config' (slots steps and mandatory). It adds
#' a 'Description' step and a TRUE value at the beginning of the 'steps'
#' and 'mandatory'
#'  list, erases all white spaces for the names of the steps.
#'
#' @return A `string` containing some R code
#'
#' @export
#' 
#' @rdname insertCodeForExternalModules
#' 
#' @examples
#' Get_Code_Update_Config_Variable()
#'
Get_Code_Update_Config_Variable <- function() {
    code <- "
    config@steps <- setNames(config@steps,
        nm = gsub(' ', '', config@steps, fixed = TRUE))


    "

    code
}



#' @title Code for declaring widgets.default.values reactive variable
#'
#' @description This function create the source code needed inside a module
#' to declare the reactive variable called 'widgets.default.values'.
#' # Declaration of the variables that will contain the values of the widgets
#' To avoid confusion, the first string is the name of the step while the
#' second is the name of the widget
#'
#' @param widgets.names A `list` containing the names of the widgets in all
#' steps of the module.
#'
#' @author Samuel Wieczorek
#'
#' @return NA
#'
#' @export
#' 
#' @rdname insertCodeForExternalModules
#'
#' @examples
#' widgets <- paste0("widget", 1:3)
#' Get_Code_Declare_widgets(widgets)
#'
Get_Code_Declare_widgets <- function(widgets.names = NULL) {
    # If one is on a composed workflow which do not have explicit ui
    # browser()
    if (is.null(widgets.names)) {
        declare_rv_widgets <- "rv.widgets <- reactiveValues()\n\n"
    } else {
        basis <- "w.name = widgets.default.values$w.name"
        ls_list <- lapply(
            widgets.names,
            function(x) gsub("w.name", x, basis)
        )
        declare_rv_widgets <- paste0(
            "rv.widgets <- reactiveValues(\n",
            paste0("\t", ls_list, sep = "", collapse = ",\n"),
            "\n)\n\n"
        )
    }
    declare_rv_widgets
}



#' @title Code for declaring rv.custom.default.values reactive variable
#'
#' @description This function create the source code needed inside a module
#' to declare the reactive variable called 'rv.custom.default.values'.
#' # Declaration of the variables that will contain the values of the user
#' variables.
#' To avoid confusion, the first string is the name of the step while the
#' second is the name of the widget
#'
#' @param rv.custom.names A `list` containing the names of the custom values.
#'
#' @author Samuel Wieczorek
#'
#' @return NA
#'
#' @export
#' 
#' @rdname insertCodeForExternalModules
#'
#' @examples
#' custom <- list(foo1 = list(), foo2 = 3)
#' Get_Code_Declare_rv_custom(names(custom))
#'
Get_Code_Declare_rv_custom <- function(rv.custom.names = NULL) {
    # If one is on a composed workflow which do not have explicit ui

    if (is.null(rv.custom.names)) {
        declare_rv_custom <- "rv.custom <- reactiveValues()\n\n"
    } else {
        basis <- "w.name = rv.custom.default.values$w.name"
        ls_list <- lapply(
            rv.custom.names,
            function(x) gsub("w.name", x, basis)
          )
        declare_rv_custom <- paste0(
            "rv.custom <- reactiveValues(\n",
            paste0("\t", ls_list, sep = "", collapse = ",\n"),
            "\n)\n\n"
        )
    }

    declare_rv_custom
}




#' @title Code for declaring xxx
#'
#' @description This function xxx
#' # Generate dynamically the observeEvent function for each widget
#'
#' @param widgets.names A `list` containing the names of the widgets in all
#' steps of the module.
#'
#' @author Samuel Wieczorek
#'
#' @export
#' 
#' @rdname insertCodeForExternalModules
#'
#' @examples
#' widgets <- paste0("widget", 1:3)
#' Get_Code_for_ObserveEvent_widgets(widgets)
#'
#' @return NA
#'
Get_Code_for_ObserveEvent_widgets <- function(widgets.names = NULL) {
    declare_rv_widgets <- NULL
    if (!is.null(widgets.names)) {
        basis <- "observeEvent(input$widget.name, {
    rv.widgets$widget.name <- input$widget.name})"
        ls_list <- lapply(
            widgets.names,
            function(x) gsub("widget.name", x, basis)
        )

        declare_rv_widgets <- paste0(ls_list, collapse = "\n")
        declare_rv_widgets <- paste0(declare_rv_widgets, "\n\n\n")
    }
    declare_rv_widgets
}




#' @title Code for declaring widgets.default.values reactive variable
#'
#' @description This function createxxx
#'
#' @return A `string` containing some R code
#'
#' @author Samuel Wieczorek
#'
#' @export
#' 
#' @rdname insertCodeForExternalModules
#'
#' @examples
#' Get_Code_for_rv_reactiveValues()
#'
Get_Code_for_rv_reactiveValues <- function() {
    basis <- "rv <- reactiveValues(
    # Stores the object given in input of the process
    dataIn = NULL,
    # A vector of boolean indicating the status (UNDONE, SKIPPED or VALIDATED)
    # of the steps
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
#' @export
#' 
#' @rdname insertCodeForExternalModules
#'
#' @examples
#' Get_Code_for_dataOut()
#'
#' @return NA
#'
Get_Code_for_dataOut <- function() {
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
#' @export
#' 
#' @rdname insertCodeForExternalModules
#'
#' @examples
#' Get_Code_for_General_observeEvents()
#'
#' @return NA
#'
Get_Code_for_General_observeEvents <- function() {
    code <- "observeEvent(steps.enabled(), ignoreNULL = TRUE, {
    if (is.null(steps.enabled()))
        rv$steps.enabled <- setNames(rep(FALSE, rv$length),
                                 nm = names(rv$config@steps))
    else
        rv$steps.enabled <- steps.enabled()
})

observeEvent(steps.status(), ignoreNULL = TRUE, {
    if (is.null(steps.enabled()))
        rv$steps.status <- setNames(rep(global$UNDONE, rv$length),
                                 nm = names(rv$config@steps))
    else
        rv$steps.status <- steps.status()
})

observeEvent(remoteReset(), {
      lapply(names(rv.widgets), function(x){
          rv.widgets[[x]] <- widgets.default.values[[x]]
        })
})


"

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
#' Module_Return_Func()
#'
#' @export
#' 
#' @rdname insertCodeForExternalModules
#'
#' @return NA
#'
Module_Return_Func <- function() {
    code <- "# Return value of module
# DO NOT MODIFY THIS PART
list(config = reactive({
    config@ll.UI <- setNames(lapply(names(config@steps),
                                  function(x){
                                    do.call(\"uiOutput\", list(ns(x)))
                                  }),
                           paste0(\"screen_\", names(config@steps))
    )
    config
    }),
dataOut = reactive({dataOut})
)


"

    code
}

#' @title Code for declaring xxx
#'
#' @description This function xxx
#' # Generate dynamically the observeEvent function for each widget
#'
#' @param w.names xxx
#' @param rv.custom.names xxx
#'
#' @author Samuel Wieczorek
#'
#' @examples
#' widgets <- c('widget1', 'widget2')
#' custom <- list(foo1 = list(), foo2 = 3)
#' Get_Worflow_Core_Code(widgets, names(custom))
#'
#' @export
#' 
#' @rdname insertCodeForExternalModules
#'
#' @return NA
#'
Get_Worflow_Core_Code <- function(
    w.names = NULL,
    rv.custom.names = NULL) {
  #browser()
    core <- paste0(
        Get_Code_Declare_widgets(w.names),
        #Get_Code_Update_Config_Variable(),
        Get_Code_for_ObserveEvent_widgets(w.names),
        Get_Code_for_rv_reactiveValues(),
        Get_Code_Declare_rv_custom(rv.custom.names),
        Get_Code_for_dataOut(),
        Get_Code_for_General_observeEvents(),
        sep = "\n"
    )

    core
}



#' @title Code for declaring xxx
#'
#' @description This function xxx
#' # Generate dynamically the observeEvent function for each widget
#'
#' @param w.names xxx
#' @param rv.custom.names xxx
#'
#' @author Samuel Wieczorek
#'
#' @examples
#' widgets <- c('widget1', 'widget2')
#' Get_AdditionalModule_Core_Code(widgets)
#'
#' @export
#' 
#' @rdname insertCodeForExternalModules
#'
#' @return NA
#'
Get_AdditionalModule_Core_Code <- function(
  w.names = NULL, 
  rv.custom.names = NULL) {
    core <- paste0(
        Get_Code_Declare_widgets(w.names),
        Get_Code_for_ObserveEvent_widgets(w.names),
        Get_Code_for_rv_reactiveValues(),
        Get_Code_Declare_rv_custom(rv.custom.names),
        Get_Code_for_dataOut(),
        Get_Code_for_AddMod_observeEvents(),
        sep = "\n"
    )

    core
}


#' @title Code for declaring xxx
#'
#' @description This function xxx
#' # Generate dynamically the observeEvent function for each widget

#' @author Samuel Wieczorek
#'
#' @examples
#' Get_Code_for_AddMod_observeEvents()
#'
#' @export
#' 
#' @rdname insertCodeForExternalModules
#'
#' @return NA
#'
Get_Code_for_AddMod_observeEvents <- function() {
    code <- "

observeEvent(reset(), {
    lapply(names(rv.widgets), function(x){
        rv.widgets[[x]] <- widgets.default.values[[x]]
    })
})


"

    code
}
