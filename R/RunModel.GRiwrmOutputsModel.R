#' RunModel for hot restart after a previous simulation period
#'
#' This function allows to restart a simulation at the end of a previous
#' simulation period. Parameters `Qinf`, `Qrelease`, and `Qmin` can be
#' redefined for this new simulation period.
#'
#' @details
#' `IndPeriod_Run` or `DatesR` must must be continuous periods starting the
#' time step after the last simulation time step of the `GRiwrmOutputsModel`
#' object provided through the argument `x`.
#'
#' `Qinf`, `Qmin`, and `Qrelease` are used for overwriting the corresponding
#' arguments provided to [CreateInputsModel.GRiwrm] for the period to be simulated.
#' Therefore, the number of rows of these arguments must correspond to
#' `IndPeriod_Run` or `DatesR` lengths.
#'
#' @inheritParams getNextTimeSteps
#' @inheritParams RunModel.GRiwrmInputsModel
#' @inheritParams airGR::CreateRunOptions
#' @param DatesR (optional) [POSIXt] vector of dates of period to be used for
#' the model run. See details
#' @param Qinf (optional) [matrix] or [data.frame] of [numeric] containing
#'        observed flows. It must be provided only for nodes of type "Direct
#'        injection" and "Diversion" \[m3 per time step\].
#'        Column names correspond to node IDs. Negative flows are abstracted from
#'        the model and positive flows are injected to the model. See details
#' @param Qmin (optional) [matrix] or [data.frame] of [numeric] containing
#'        minimum flows to let downstream of a node with a Diversion \[m3 per
#'        time step\]. Default is zero. Column names correspond to node IDs.
#'        See details
#' @param Qrelease (optional) [matrix] or [data.frame] of [numeric] containing
#'        release flows by nodes using the model `RunModel_Reservoir` \[m3 per
#'        time step\]. See details
#' @param merge_outputs [logical] Merge simulation outputs with the one provided
#' in argument `x`
#' @param ... Further arguments for compatibility with S3 methods
#'
#' @inherit RunModel.GRiwrmInputsModel return
#' @export
#'
RunModel.GRiwrmOutputsModel <- function(x,
                                        InputsModel,
                                        RunOptions,
                                        IndPeriod_Run = which(InputsModel[[1]]$DatesR %in% DatesR),
                                        DatesR = getNextTimeSteps(x),
                                        Qinf = NULL,
                                        Qrelease = NULL,
                                        Qmin = NULL,
                                        merge_outputs = TRUE,
                                        ...) {
  stopifnot(inherits(x, "GRiwrmOutputsModel"),
            inherits(InputsModel, "GRiwrmInputsModel"),
            inherits(RunOptions, "GRiwrmRunOptions"))
  # Check Run Period
  next_time_step <- getNextTimeSteps(x)
  next_index <- which(InputsModel[[1]]$DatesR == next_time_step)
  if (IndPeriod_Run[1] != next_index) {
    stop("`IndPeriod_Run` should have its first element equal to ", next_index)
  }

  # State Initiation
  for (id in names(RunOptions)) {
    # Run model for the sub-basin and one time step
    RunOptions[[id]]$IniResLevels <- NULL
    RunOptions[[id]]$IniStates <- serializeIniStates(x[[id]]$StateEnd)
    RunOptions[[id]]$IndPeriod_WarmUp <- 0L
    RunOptions[[id]]$IndPeriod_Run <- IndPeriod_Run
  }

  # Inputs change
  checkInputsModelArguments(
    attr(InputsModel, "GRiwrm"),
    InputsModel[[1]]$DatesR[IndPeriod_Run],
    Qinf = Qinf,
    Qrelease = Qrelease,
    Qmin = Qmin
  )
  inputs <- list(Qinf = Qinf, Qrelease = Qrelease, Qmin = Qmin)
  inputs[sapply(inputs, is.null)] <- NULL
  for (inputArg in names(inputs)) {
    input <- inputs[[inputArg]]
    if (length(IndPeriod_Run) != nrow(input)) {
      stop("The Argument ", inputArg,
           " must have a number of rows identical to the lenght of `IndPeriod_Run`")
    }
    for (id in colnames(input)) {
      v <- input[, id, drop = TRUE]
      if (inputArg %in% c("Qrelease", "Qmin")) {
        if (inputArg == "Qrelease" && !InputsModel[[id]]$isReservoir) {
            stop("The column ", id, " of the argument `Qrelease` does not refer to a Reservoir node")
        }
        if (inputArg == "Qmin" && !InputsModel[[id]]$hasDiversion) {
          stop("The column ", id, " of the argument `Qmin` does not refer to a Diversion node")
        }
        if (is.null(InputsModel[[id]][[inputArg]])) {
            stop("InputsModel[['", id, "']] should contain a `", inputArg, "` item")
        }
        InputsModel[[id]][[inputArg]][IndPeriod_Run] <- v
      }
      if (inputArg == "Qinf") {
        g <- attr(InputsModel, "GRiwrm")
        if (is.null(InputsModel[[id]])) {
          # Direct Injection
          id_down <- g$down[g$id == id]
          InputsModel[[id_down]]$Qupstream[IndPeriod_Run, id] <- v
        } else {
          if (!InputsModel[[id]]$hasDiversion) {
            stop("The column ", id, " of the argument `Qinf` does not refer to a DirectInjection or a Diversion node")
          }
          # Update withdrawal due to Diversion
          InputsModel[[id]]$Qdiv[IndPeriod_Run] <- -v
        }
      }
    }
  }

  # Run the model
  OM <- suppressMessages(
    RunModel(
      InputsModel,
      RunOptions = RunOptions,
      Param = extractParam(x)
    )
  )

  if (merge_outputs) {
    OM <- merge(x, OM)
  }

  return(OM)
}
