% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mod_by_bin_utils.R
\name{fct_show_modal_bin_functions}
\alias{fct_show_modal_bin_functions}
\title{show modal helper function}
\usage{
fct_show_modal_bin_functions(doc, output, session)
}
\arguments{
\item{doc}{The documentation name to show. Matches input$f_step*}

\item{output, session}{parsed down from Shiny module}
}
\value{
Called for side effect of executing showModal()
}
\description{
Helper function for showing modal for all steps of function assigns to a
temporary output that is used by showModal once. Also includes a function from 'by animal'.
}
