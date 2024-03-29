% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/f_by_animal.R
\name{f_iterate_animals}
\alias{f_iterate_animals}
\title{Function to iterate each animal's data through the 'by animal' cleaning functions}
\usage{
f_iterate_animals(
  df_in,
  col_animal_id = .data$animal_id,
  col_bin_id = .data$bin_id,
  col_date = .data$date,
  col_start_time = .data$start_time,
  col_intake = .data$corrected_intake,
  col_duration = .data$duration_sec,
  max_duration_min = 60,
  min_intake_rate_kg_min = 0.05,
  max_intake_rate_kg_min = 1.5,
  outlier_exemption_max_duration = 1,
  outlier_exemption_max_intake = 0.2,
  sd_thresh = Inf,
  shiny.session = NULL,
  log = TRUE,
  verbose = TRUE
)
}
\arguments{
\item{df_in}{A data frame, typically that has been processed by
[f_by_bin_clean()]}

\item{col_animal_id, col_duration, col_bin_id, col_date, col_start_time, col_intake, }{Column names for the columns in df_in that contain animal ID, intake (kg),
duration (sec), feed bin ID, date and feeding event start time.}

\item{max_duration_min}{number of minutes. Events with a duration longer than
this will be classed as a 'manual outlier'}

\item{min_intake_rate_kg_min, max_intake_rate_kg_min}{number (kg/min). Events
with a rate of intake (i.e. intake / duration) greater than the
`max_intake_rate_kg_min` and less than the `min_intake_rate_kg_min` will be
classed as a 'manual outlier'}

\item{outlier_exemption_max_duration, outlier_exemption_max_intake}{number (kg
or min). Events below this `outlier_exemption_max_intake` (kg) & with a
duration less than `outlier_exemption_max_duration` (min) are exempt from
other outlier detection methods.}

\item{sd_thresh}{number to use as threshold for +/- scale (SD) of fitted
bisector regression}

\item{shiny.session}{session name, for R Shiny to update progress bar. Use
NULL if not using shiny.}

\item{log}{Boolean. Should log files be generated to a temporary directory?}

\item{verbose}{Boolean. Should more detail be printed in logs? This will print each animal number as it iterates and is useful for troubleshooting.}
}
\value{
A list with the full nested df (nested_out) and a small summary df
  (outlier_summary)
}
\description{
This splits the df_in into a list of data frames, which is parsed to
[f_flag_and_replace_outliers()] one at a time. Also can create a log.
}
\details{
Error types are: pos = new_y (i.e. new intake), neg = new_x (i.e. new
duration), and neg_intake = replaced with 0 kg and neg_duration = new
duration. See [f_flag_and_replace_outliers()]
}
