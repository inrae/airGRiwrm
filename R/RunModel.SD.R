#' Run SD Model from run-off model outputs
#'
#' @inheritParams airGR::RunModel_Lag
#' @param OutputsModel `OutputsModel` object returned by a GR model by [airGR::RunModel]
#'
#' @return `OutputsModel` object. See [airGR::RunModel_Lag]
#' @export
#'
RunModel.SD <- function(InputsModel, RunOptions, Param, OutputsModel, ...) {
  InputsModel$OutputsModel <- OutputsModel
  RunModel_Lag(InputsModel, RunOptions, Param[1])
}
