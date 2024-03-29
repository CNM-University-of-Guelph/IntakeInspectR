% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/f_by_bin_plots.R
\name{plot_bin_regression}
\alias{plot_bin_regression}
\title{plot_bin_regression function}
\usage{
plot_bin_regression(df_in, x, y, col_colour, col_hover_text = NA_character_)
}
\arguments{
\item{df_in}{Dataframe that has been filtered via user input (`input$bin_id`) to only include 1 feed bin.}

\item{x, y}{columns to plot on x- and y-axis}

\item{col_colour}{column name to use for `aes(colour = )`}

\item{col_hover_text}{column name of extra text for tooltip. Usually from adding a column with paste, e.g. `paste('animal_id:', animal_id)`}
}
\value{
A ggplot2 object
}
\description{
A function to plot duration_sec vs feed_intake scatter plot.
This doesn't include an actual regression function because this is best done per animal, not per bin.
}
