#' Run SD Model from run-off model outputs
#'
#' @inheritParams airGR::RunModel_Lag
#' @param x `InputsModel` used as `InputsModel` parameter for [airGR::RunModel]
#' @param OutputsModel `OutputsModel` object returned by a GR model by [airGR::RunModel]
#' @param ... further arguments passed to or from other methods
#'
#' @return `OutputsModel` object. See [airGR::RunModel_Lag]
#' @export
#'
RunModel.SD <- function(x, RunOptions, Param, OutputsModel, ...) {
  message("RunModel.SD")
  x$OutputsModel <- OutputsModel
  RunModel_Lag(x, RunOptions = RunOptions, Param = Param[1])
}
