#' @rdname Calibration
#' @export
Calibration.InputsModel <- function(InputsModel,
                                    CalibOptions,
                                    ...) {
  if (!exists("FUN_MOD") && !is.null(InputsModel$FUN_MOD)) {
    if (!any(is.na(CalibOptions$FixedParam))) {
      message("Parameters already fixed - no need for calibration")
      message("\t     Param = ", paste(sprintf("%8.3f", CalibOptions$FixedParam), collapse = ", "))
      OC <- list(ParamFinalR = CalibOptions$FixedParam)
      class(OC) <- c("OutputsCalib", class(OC))
      return(OC)
    } else {
      if (!is.null(InputsModel$hasDiversion) && InputsModel$hasDiversion) {
        class(InputsModel) <- setdiff(class(InputsModel), "SD")
        FUN_MOD = RunModel.InputsModel
      } else {
        FUN_MOD = InputsModel$FUN_MOD
      }
      airGR::Calibration(InputsModel,
                         CalibOptions = CalibOptions,
                         FUN_MOD = FUN_MOD, ...)
    }
  } else {
    airGR::Calibration(InputsModel,
                       CalibOptions = CalibOptions,
                       ...)
  }
}
