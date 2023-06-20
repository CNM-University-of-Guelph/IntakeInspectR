#' Insentec data for demonstrating IntakeInspectR
#'
#' A dataset derived of real intake data from Insentec Feed Bins which are
#' distributed to allow IntakeInspectR functionality to be tested. Cow numbers
#' have been changed to be anonymous. It contains data from 100 feed bins for 7 days.
#'
#' @format ## `demo_insentec_data`
#' A data frame with 94,026 rows and 11 columns:
#' \describe{
#'   \item{transponder_id}{Fake transponder identification number}
#'   \item{feed_bin_id}{Feed bin ID}
#'   \item{start_time, end_time}{Start and end date and time of feeding event}
#'   \item{feed_duration}{Duration of feed event in seconds}
#'   \item{start_weight, end_weight}{Start and end weight of feeding event, kg}
#'   \item{diet}{name of diet assigned to feed bin}
#'   \item{intake}{amount of feed eaten during event, kg}
#'   \item{date}{date of feeding event}
#'   \item{cow_id}{Fake cow identification number}
#'   ...
#' }
#' @source Elora Dairy Research Facility
