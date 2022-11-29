#' Wrapper for [airGR::RunModel] for one sub-basin
#'
#' @details This function calls [airGR::RunModel] (See [airGR::RunModel] for further details).
#'
#' The list produced by the function (See Value section of [airGR::RunModel_GR4J]) is here completed by an item *$Qsim_m3* storing the simulated discharge series in m3/s.
#'
#' @inheritParams airGR::RunModel
#' @param x \[object of class \emph{InputsModel}\] see [airGR::CreateInputsModel] for details
#' @param ... Further arguments for compatibility with S3 methods
#'
#' @inherit airGR::RunModel return return
#'
#' @export
RunModel.InputsModel <- function(x, RunOptions, Param, FUN_MOD = NULL, ...) {
  if(is.null(FUN_MOD)) {
    FUN_MOD <- x$FUN_MOD
  }
  FUN_MOD <- match.fun(FUN_MOD)
  if (identical(FUN_MOD, RunModel_Lag)) {
    QcontribDown <- list(
      RunOptions = list(
        WarmUpQsim = rep(0, length(RunOptions$IndPeriod_WarmUp))
      ),
      Qsim = rep(0, length(RunOptions$IndPeriod_Run))
    )
    class(QcontribDown) <- c("OutputsModel", class(RunOptions)[-1])
    x$BasinAreas[length(x$BasinAreas)] <- 1
    OutputsModel <- RunModel_Lag(x, RunOptions, Param, QcontribDown)
  } else {
    OutputsModel <- airGR::RunModel(x, RunOptions, Param, FUN_MOD)
  }
  if (is.null(OutputsModel$Qsim_m3)) {
    # Add Qsim_m3 in m3/timestep
    OutputsModel$Qsim_m3 <-
      OutputsModel$Qsim * sum(x$BasinAreas, na.rm = TRUE) * 1e3
  }
  if ("WarmUpQsim" %in% RunOptions$Outputs_Sim) {
    OutputsModel$RunOptions$WarmUpQsim_m3 <-
      OutputsModel$RunOptions$WarmUpQsim * sum(x$BasinAreas, na.rm = TRUE) * 1e3
  }
  if (x$hasDiversion) {
    OutputsModel <- RunModel_Diversion(x, RunOptions, OutputsModel)
  }
  return(OutputsModel)
}


#' Model the diversion of a flow from an existing modeled node
#'
#' On a Diversion node, this function is called after `airGR::RunModel` to
#' divert a part of the flow to another node than the original downstream one.
#'
#' @param InputsModel \[object of class \emph{InputsModel}\] see
#'        [airGR::CreateInputsModel] for details
#' @param RunOptions Same parameter as in [RunModel.GRiwrmInputsModel]
#' @param OutputsModel Output of [airGR::RunModel]
#' @param updateQsim [logical] for updating Qsim after diversion in the output
#'
#' @return Updated `OutputsModel` object after diversion
#' @noRd
#'
RunModel_Diversion <- function(InputsModel,
                               RunOptions,
                               OutputsModel,
                               updateQsim = TRUE) {
  OutputsModel$Qnat <- OutputsModel$Qsim
  lQ <- calc_Qdiv(OutputsModel$Qsim_m3,
                  InputsModel$Qdiv[RunOptions$IndPeriod_Run],
                  InputsModel$Qmin[RunOptions$IndPeriod_Run])
  #message(paste(InputsModel$Qdiv[RunOptions$IndPeriod_Run], lQ$Qdiv, lQ$Qsim, InputsModel$Qmin[RunOptions$IndPeriod_Run], sep = ", "))
  OutputsModel$Qdiv_m3 <- lQ$Qdiv
  OutputsModel$Qsim_m3 <- lQ$Qsim
  if (updateQsim) {
    OutputsModel$Qsim <-
      OutputsModel$Qsim_m3 / sum(InputsModel$BasinAreas, na.rm = TRUE) / 1e3
  }
  if ("WarmUpQsim" %in% RunOptions$Outputs_Sim) {
    lQ <- calc_Qdiv(OutputsModel$RunOptions$WarmUpQsim_m3,
                    InputsModel$Qdiv[RunOptions$IndPeriod_WarmUp],
                    InputsModel$Qmin[RunOptions$IndPeriod_WarmUp])
    OutputsModel$RunOptions$WarmUpQdiv_m3 <- lQ$Qdiv
    OutputsModel$RunOptions$WarmUpQsim_m3 <- lQ$Qsim
  }
  return(OutputsModel)
}


#' Compute diverted and simulated flow at a diversion
#'
#' @param Qnat [numeric] time series of flow before diversion (m3/time step)
#' @param Qdiv [numeric] time series of planned diverted flow (m3/time step)
#' @param Qmin [numeric] time series of minimum flow after diversion (m3/time step)
#'
#' @return A [list] with items:
#' - Qdiv, the diverted flow after limitation of minimum flow
#' - Qsim, the simulated flow after diversion and limitation
#' @noRd
calc_Qdiv<- function(Qnat, Qdiv, Qmin) {
  Qsim <- Qnat - Qdiv
  indexQmin <- which(Qsim < Qmin & Qdiv > 0)
  if (any(indexQmin)) {
    #Qsim[indexQmin] <- sapply(indexQmin, function(i) min(Qnat[i], Qmin[i]))
    Qsim[indexQmin] <- pmin(Qnat[indexQmin], Qmin[indexQmin])
  }
  return(list(Qsim = Qsim, Qdiv = Qnat - Qsim))
}
