#' Calibration of either airGR model and GRIWRM semi-distributive model
#'
#' @param InputsModel the class of the first parameter determine which calibration is used
#' @param ... further arguments passed to or from other methods.
#'
#' @return \emph{OutputsCalib} or \emph{GriwrmOutputsCalib} object
#' @export
Calibration <- function(InputsModel, ...) {
  UseMethod("Calibration", InputsModel)
}
