#' Check of the parameters of RunModel methods
#'
#' Stop the execution if an error is detected.
#'
#' @param InputsModel \[`GRiwrmInputsModel` object\] see [CreateInputsModel.GRiwrm] for details
#' @param RunOptions \[`GRiwrmRunOptions` object\] see [CreateRunOptions.GRiwrmInputsModel] for details
#' @param Param [list] of containing model parameter values of each node of the network
#' @noRd
checkRunModelParameters <- function(InputsModel, RunOptions, Param) {
  if(!inherits(InputsModel, "GRiwrmInputsModel")) stop("`InputsModel` parameter must of class 'GRiwrmInputsModel' (See ?CreateRunOptions.GRiwrmInputsModel)")
  if(!inherits(RunOptions, "GRiwrmRunOptions")) stop("Argument `RunOptions` parameter must of class 'GRiwrmRunOptions' (See ?CreateRunOptions.GRiwrmInputsModel)")
  if(!is.list(Param) || !all(names(InputsModel) %in% names(Param))) stop("Argument `Param` must be a list with names equal to nodes IDs")
}


#' Creation of a data.frame with simulated flows at each node of the GRiwrm object
#'
#' @details
#' This function can only be called inside [RunModel.GRiwrmInputsModel] or [RunModel.Supervisor]
#' because it needs a `GRiwrmInputsModel` object internally modified by these functions
#' (`Qupstream` updated with simulated flows).
#'
#' @param InputsModel \[`GRiwrmInputsModel` object\] see [CreateInputsModel.GRiwrm] for details
#' @param OutputsModel \[`GRiwrmOutputsModel` object\] see [RunModel.GRiwrmInputsModel] or [RunModel.Supervisor] for details
#' @param IndPeriod_Run [numeric] index of period to be used for the model run [-]. See [airGR::CreateRunOptions] for details
#'
#' @return a [data.frame] containing the simulated flows (in m3/time step) structured with the following columns:
#' - 'DatesR' vector of dates  of the time series
#' - one column by node with the simulated flows
#' @noRd
OutputsModelQsim <- function(InputsModel, OutputsModel, IndPeriod_Run) {
  griwrm <- attr(InputsModel, "GRiwrm")
  # Get simulated flow for each node
  # Flow for each node is available in OutputsModel except for Direct Injection
  # nodes where it is stored in InputsModel$Qupstream of the downstream node
  QsimRows <- getDiversionRows(griwrm, TRUE)
  lQsim <- lapply(
    QsimRows,
    function(i) {
      x <- griwrm[i, ]
      if (is.na(x$model)) {
        InputsModel[[x$down]]$Qupstream[IndPeriod_Run, x$id]
      } else {
        OutputsModel[[x$id]]$Qsim_m3
      }
    }
  )
  names(lQsim) <- griwrm$id[QsimRows]
  dfQsim <- cbind(data.frame(DatesR = InputsModel[[1]]$DatesR[IndPeriod_Run]),
                  do.call(cbind,lQsim) / attr(InputsModel, "TimeStep"))
  class(dfQsim) <- c("Qm3s", class(dfQsim)) # For S3 methods
  return(dfQsim)
}


#' Convert IniStates list into a vector
#'
#' @param IniStates see [CreateIniStates]
#'
#' @return A vector as in `RunOptions$IniStates`
#' @noRd
#'
serializeIniStates <- function(IniStates) {
  unlist(IniStates)
}

