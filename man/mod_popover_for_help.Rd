% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mod_popover_for_help.R
\name{mod_popover_for_help}
\alias{mod_popover_for_help}
\alias{mod_popover_for_help_ui}
\alias{mod_popover_for_help_server}
\title{mod_popover_for_help_ui and mod_popover_for_help_server}
\usage{
mod_popover_for_help_ui(id)

mod_popover_for_help_server(id, title, content)
}
\arguments{
\item{id}{shiny id}

\item{title}{xxxx}

\item{content}{xxx}

\item{input}{internal}

\item{output}{internal}

\item{session}{internal}
}
\description{
A shiny Module.
}
\examples{
\dontshow{if () (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
shiny::runApp(popover_for_help('myTitle', 'myContent'))
\dontshow{\}) # examplesIf}
}
\keyword{internal}
