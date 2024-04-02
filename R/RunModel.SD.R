#' Run a semi-distributed model from rainfall-runoff model outputs
#'
#' @inheritParams airGR::RunModel_Lag
#' @param x \[object of class `InputsModel`\] used as `InputsModel` parameter for [airGR::RunModel_Lag]
#' @param ... further arguments passed to or from other methods
#'
#' @return `OutputsModel` object. See [airGR::RunModel_Lag]
#' @noRd
#'
RunModel.SD <- function(x, RunOptions, Param, QcontribDown, ...) {
  if (x$isReservoir) {
    OutputsModel <- RunModel_Reservoir(x,
                                       RunOptions = RunOptions,
                                       Param = Param[1:2])
  } else {
    OutputsModel <- airGR::RunModel_Lag(x,
                                        RunOptions = RunOptions,
                                        Param = Param[1],
                                        QcontribDown = QcontribDown)
  }
  OutputsModel$RunOptions$TimeStep <- RunOptions$FeatFUN_MOD$TimeStep
  return(OutputsModel)
}
