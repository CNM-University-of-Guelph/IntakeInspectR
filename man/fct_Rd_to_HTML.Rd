% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mod_by_cow_utils.R
\name{fct_Rd_to_HTML}
\alias{fct_Rd_to_HTML}
\title{Convert Rd to HTML}
\usage{
fct_Rd_to_HTML(Rd_filepath)
}
\arguments{
\item{Rd_filepath}{An .Rd file, normally in man/ e.g. "./man/f_flag_and_replace_outliers.Rd"}
}
\value{
HTML file itself
}
\description{
This is used to convert the documentation .Rd files into HTML that can be
used inside a modal.
}
\details{
For using in a modal, this function will be called inside a renderUI() that
is assigned to an output$ . Then, this output is called inside a
`showModal(modalDialog( uiOutput(HERE)))`
}
