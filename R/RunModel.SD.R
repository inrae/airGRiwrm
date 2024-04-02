#' Run a hydraulic routing model from rainfall-runoff model outputs
#'
#' @inheritParams airGR::RunModel_Lag
#' @param x \[object of class `InputsModel`\] used as `InputsModel` parameter for [airGR::RunModel_Lag]
#' @param ... further arguments passed to or from other methods
#'
#' @return `OutputsModel` object. See [airGR::RunModel_Lag]
#' @noRd
#'
RunModel.SD <- function(x, RunOptions, Param, QcontribDown = NULL, ...) {
  if (is.null(QcontribDown)) {
    QcontribDown <- list(
      RunOptions = list(
        WarmUpQsim = rep(0, length(RunOptions$IndPeriod_WarmUp))
      ),
      Qsim = rep(0, length(RunOptions$IndPeriod_Run))
    )
    class(QcontribDown) <- c("OutputsModel", class(RunOptions)[-1])
    x$BasinAreas[length(x$BasinAreas)] <- 1E-6
  }
  OutputsModel <- airGR::RunModel_Lag(x,
                                      RunOptions = RunOptions,
                                      Param = Param[1],
                                      QcontribDown = QcontribDown)
  OutputsModel$DatesR <- x$DatesR[RunOptions$IndPeriod_Run]
  if ("WarmUpQsim" %in% RunOptions$Outputs_Sim) {
    OutputsModel$RunOptions$WarmUpQsim_m3 <-
      OutputsModel$RunOptions$WarmUpQsim * sum(x$BasinAreas, na.rm = TRUE) * 1e3
  }
  OutputsModel <- calcOverAbstraction(OutputsModel, FALSE)
  OutputsModel$RunOptions <- calcOverAbstraction(OutputsModel$RunOptions, TRUE)

  OutputsModel$RunOptions$TimeStep <- RunOptions$FeatFUN_MOD$TimeStep
  return(OutputsModel)
}
