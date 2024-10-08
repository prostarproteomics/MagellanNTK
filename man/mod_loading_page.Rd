% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mod_loadapp.R
\name{mod_loading_page}
\alias{mod_loading_page}
\alias{loadapp_ui}
\alias{loadapp_server}
\alias{loadApp}
\title{Load customizable functions}
\usage{
loadapp_ui(id)

loadapp_server(id)

loadApp()
}
\arguments{
\item{id}{shiny id}
}
\value{
A list where each slot is named with the customizable functions. Each slot
contains the code to call this function from the package the user have
chosen.
}
\description{
A shiny Module.which allow to choose whether to use the default
functions (embedded in Magellan) or functions provided by external packages.
The names of these functions (shiny modules) are:
* convert(): xxx
* open_dataset(): xxx
* open_demoDataset(): xxx
* view_dataset(): xxxx
* infos_dataset(): xxx

Thus, MagellanNTK seraches in the global environment if any package exports
one or more of these functions. If so, 

For each of these functions, the shiny app lists all the packages that export
it. Once the user has make its choices, the module returns a list containing 
the infos.
}
\examples{
\dontrun{
app <- loadApp()
shiny::runApp(app)
}

}
