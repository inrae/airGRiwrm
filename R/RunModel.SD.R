#' Run SD Model from run-off model outputs
#'
#' @inheritParams airGR::RunModel_Lag
#' @param x `InputsModel` used as `InputsModel` parameter for [airGR::RunModel_Lag]
#' @param QsimDown a [numeric] corresponding to the runoff of the sub-basin (Typically the `Qsim` outputs of the GR model)
#' @param ... further arguments passed to or from other methods
#'
#' @return `OutputsModel` object. See [airGR::RunModel_Lag]
#' @export
#'
RunModel.SD <- function(x, RunOptions, Param, QsimDown, ...) {
  x$OutputsModel <- list(Qsim = QsimDown)
  RunModel_Lag(x, RunOptions = RunOptions, Param = Param[1])
}
