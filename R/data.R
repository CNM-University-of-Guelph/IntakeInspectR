#' Insentec data for demonstrating IntakeInspectR
#'
#' A dataset derived of real intake data from Insentec Feed Bins which are
#' distributed to allow IntakeInspectR functionality to be tested. Animal numbers
#' have been changed to be anonymous. It contains data from 100 feed bins for 7 days.
#'
#' @format ## `demo_insentec_data`
#' A data frame with 94,026 rows and 11 columns:
#' \describe{
#'   \item{transponder_id}{Fake transponder identification number}
#'   \item{bin_id}{Feed bin ID}
#'   \item{start_time, end_time}{Start and end date and time of feeding event}
#'   \item{duration_sec}{Duration of feed event in seconds}
#'   \item{start_weight_kg, end_weight_kg}{Start and end weight of feeding event, kg}
#'   \item{diet}{name of diet assigned to feed bin}
#'   \item{intake}{amount of feed eaten during event, kg}
#'   \item{date}{date of feeding event}
#'   \item{animal_id}{Fake animal identification number}
#'   ...
#' }
#' @source Ontario Dairy Research Centre, which is owned by the Agricultural Research Institute of Ontario and managed by the University of Guelph through the Ontario Agri-Food Innovation Alliance. The Government of Ontario does not provide any warranty of any kind regarding the accuracy, completeness, security or reliability of the material posted therein or provided through any sites linked directly or indirectly to the site.
"demo_insentec_data"
