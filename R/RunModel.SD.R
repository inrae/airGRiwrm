#' Run a semi-distributed model from rainfall-runoff model outputs
#'
#' @inheritParams airGR::RunModel_Lag
#' @param x \[object of class `InputsModel`\] used as `InputsModel` parameter for [airGR::RunModel_Lag]
#' @param ... further arguments passed to or from other methods
#'
#' @return `OutputsModel` object. See [airGR::RunModel_Lag]
#' @export
#'
RunModel.SD <- function(x, RunOptions, Param, QcontribDown, ...) {
  airGR::RunModel_Lag(x, RunOptions = RunOptions, Param = Param[1], QcontribDown = QcontribDown)
}
