% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/write_excel.R
\name{write.excel}
\alias{write.excel}
\title{This function exports a data.frame to a Excel file.}
\usage{
write.excel(obj, filename = NULL)
}
\arguments{
\item{obj}{A list of data.frame() items}

\item{filename}{A character string for the name of the Excel file.}
}
\value{
A Excel file (.xlsx)
}
\description{
This is the default function fo export the current dataset as an Excel file.
}
\examples{
data(lldata)
write.excel(lldata, "foo.xlsx")

}
\author{
Samuel Wieczorek
}
