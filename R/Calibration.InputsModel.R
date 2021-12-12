#' @rdname Calibration
#' @export
Calibration.InputsModel <- function(InputsModel, ...) {
  if (!exists("FUN_MOD") && !is.null(InputsModel$FUN_MOD)) {
    airGR::Calibration(InputsModel, FUN_MOD = InputsModel$FUN_MOD, ...)
  } else {
    airGR::Calibration(InputsModel, ...)
  }
}
