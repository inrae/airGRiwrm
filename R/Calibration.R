#' Calibration of either a **airGR** model or a **airGRiwrm** semi-distributed model
#'
#' @param InputsModel the class of the first parameter determines which calibration algorithm is used
#' @param ... further arguments passed to or from other methods.
#'
#' @return \emph{OutputsCalib} or \emph{GRiwrmOutputsCalib} object
#' @export
Calibration <- function(InputsModel, ...) {
  UseMethod("Calibration", InputsModel)
}
