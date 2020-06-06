#' Wrapper to \code{\link[airGR]{Calibration}}.
#'
#' @param InputsModel object of class \emph{InputsModel}, see \code{\link[airGR]{CreateInputsModel}} for details.
#' @param ... further arguments passed to \code{\link[airGR]{Calibration}}.
#'
#' @return \emph{CalibOutput} object.
#' @export
Calibration.InputsModel <- function(InputsModel, ...) {
  airGR::Calibration(InputsModel, FUN_MOD = InputsModel$FUN_MOD, ...)
}
