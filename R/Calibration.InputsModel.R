#' @rdname Calibration
#' @export
Calibration.InputsModel <- function(InputsModel, ...) {
  airGR::Calibration(InputsModel, FUN_MOD = InputsModel$FUN_MOD, ...)
}
