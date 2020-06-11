#' Wrapper to \code{\link[airGR]{Calibration}} for one sub-basin.
#'
#' @inherit airGR::Calibration
#' @export
Calibration.InputsModel <- function(InputsModel, ...) {
  airGR::Calibration(InputsModel, FUN_MOD = InputsModel$FUN_MOD, ...)
}
