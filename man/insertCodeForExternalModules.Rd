% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/code_used_by_external_modules.R
\name{insertCodeForExternalModules}
\alias{insertCodeForExternalModules}
\alias{Get_Code_Update_Config_Variable}
\alias{Get_Code_Declare_widgets}
\alias{Get_Code_Declare_rv_custom}
\alias{Get_Code_for_ObserveEvent_widgets}
\alias{Get_Code_for_rv_reactiveValues}
\alias{Get_Code_for_dataOut}
\alias{Get_Code_for_General_observeEvents}
\alias{Module_Return_Func}
\alias{AdditionnalCodeForExternalModules}
\alias{Get_Workflow_Core_Code}
\alias{Get_AdditionalModule_Core_Code}
\alias{Get_Code_for_AddMod_observeEvents}
\title{R code to update the 'config' variable of a process module}
\usage{
Get_Code_Update_Config_Variable()

Get_Code_Declare_widgets(widgets.names = NULL)

Get_Code_Declare_rv_custom(rv.custom.names = NULL)

Get_Code_for_ObserveEvent_widgets(widgets.names = NULL)

Get_Code_for_rv_reactiveValues()

Get_Code_for_dataOut()

Get_Code_for_General_observeEvents()

Module_Return_Func()

AdditionnalCodeForExternalModules(w.names = NULL, rv.custom.names = NULL)

Get_Workflow_Core_Code(
  mode = NULL,
  name = NULL,
  w.names = NULL,
  rv.custom.names = NULL
)

Get_AdditionalModule_Core_Code(w.names = NULL, rv.custom.names = NULL)

Get_Code_for_AddMod_observeEvents()
}
\arguments{
\item{widgets.names}{A `list` containing the names of the widgets in all
steps of the module.}

\item{rv.custom.names}{xxx}

\item{w.names}{xxx}

\item{mode}{xxx}

\item{name}{xxx}
}
\value{
A `string` containing some R code

NA

NA

NA

A `string` containing some R code

NA

NA

NA

NA

NA

NA

NA
}
\description{
This function generates the necessary code to
modify the variable 'config' (slots steps and mandatory). It adds
a 'Description' step and a TRUE value at the beginning of the 'steps'
and 'mandatory'
 list, erases all white spaces for the names of the steps.

This function generates the necessary code to
modify the variable 'config' (slots steps and mandatory). It adds
a 'Description' step and a TRUE value at the beginning of the 'steps'
and 'mandatory'
 list, erases all white spaces for the names of the steps.

This function create the source code needed inside a module
to declare the reactive variable called 'widgets.default.values'.
# Declaration of the variables that will contain the values of the widgets
To avoid confusion, the first string is the name of the step while the
second is the name of the widget

This function create the source code needed inside a module
to declare the reactive variable called 'rv.custom.default.values'.
# Declaration of the variables that will contain the values of the user
variables.
To avoid confusion, the first string is the name of the step while the
second is the name of the widget

This function xxx
# Generate dynamically the observeEvent function for each widget

This function createxxx

Returned value of the process

xxx

This function xxx
# Generate dynamically the observeEvent function for each widget

This function xxx
# Generate dynamically the observeEvent function for each widget

This function xxx
# Generate dynamically the observeEvent function for each widget

This function generates dynamically the observeEvent function for each widget

This function xxx
Generate dynamically the observeEvent function for each widget
}
\author{
Samuel Wieczorek
}
