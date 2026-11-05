#' RunModel for hot restart after a previous simulation period
#'
#' This function restarts a simulation using the state at the end of a previous
#' simulation (`GRiwrmOutputsModel` object `x`). It allows redefining the
#' boundary conditions `Qinf`, `Qrelease`, and `Qmin` for the new period.
#'
#' @details
#' `IndPeriod_Run` or `DatesR` must be continuous periods starting the time step
#' after the last simulation time step of the `GRiwrmOutputsModel` object
#' provided through the argument `x`.
#'
#' `Qinf`, `Qmin`, and `Qrelease` are used to overwrite the corresponding
#' arguments provided to [CreateInputsModel.GRiwrm] for the period to be
#' simulated. Therefore, the number of rows of these arguments must correspond
#' to `IndPeriod_Run` or `DatesR` lengths.
#'
#' @param x `GRiwrmOutputsModel` object resulting from a previous run.
#' @param InputsModel `GRiwrmInputsModel` object (see [CreateInputsModel.GRiwrm])
#'   or `Supervisor` object (see [CreateSupervisor]).
#' @param RunOptions List of run options created with [CreateRunOptions].
#' @param IndPeriod_Run Integer vector indicating the indices of the time steps
#'   to run. Must start at the index immediately following the previous run.
#' @param DatesR (optional) `POSIXt` vector of dates for the simulation period.
#'   See details.
#' @param Qinf (optional) `matrix` or `data.frame` of `numeric` observed flows for
#'   nodes of type "Direct injection" and "Diversion" (m³ per time step).
#'   Column names correspond to node IDs. Negative flows are abstracted from the
#'   model and positive flows are injected to the model. See details.
#' @param Qmin (optional) `matrix` or `data.frame` of `numeric` minimum flows for
#'   downstream of a Diversion node (m³ per time step). Default is zero.
#'   Column names correspond to node IDs. See details.
#' @param Qrelease (optional) `matrix` or `data.frame` of `numeric` release flows by
#'   nodes using the model `RunModel_Reservoir` (m³ per time step). See details.
#' @param merge_outputs `logical` Merge simulation outputs with the one provided
#'   in argument `x`.
#' @param ... Further arguments for compatibility with S3 methods.
#'
#' @return An object of class `GRiwrmOutputsModel` (see [RunModel.GRiwrmInputsModel]
#'   for details).
#' @seealso [CreateGRiwrm()], [CreateInputsModel.GRiwrm()], [CreateRunOptions()]
#' @seealso Vignette "V07_Combine_tactical_operational_management" in package airGRiwrm
#' @export
#'
RunModel.GRiwrmOutputsModel <- function(
  x,
  InputsModel,
  RunOptions,
  IndPeriod_Run = which(InputsModel[[1]]$DatesR %in% DatesR),
  DatesR = getNextTimeSteps(x),
  Qinf = NULL,
  Qrelease = NULL,
  Qmin = NULL,
  merge_outputs = TRUE,
  ...
) {
  stopifnot(
    inherits(x, "GRiwrmOutputsModel"),
    inherits(InputsModel, "GRiwrmInputsModel") || is.Supervisor(InputsModel),
    inherits(RunOptions, "GRiwrmRunOptions")
  )

  if (is.Supervisor(InputsModel)) {
    use_supervisor <- TRUE
    sv <- InputsModel
    InputsModel <- sv$InputsModel
  } else {
    use_supervisor <- FALSE
  }

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
    RunOptions[[id]]$IniStates <- serializeIniStates(
      x[[id]]$StateEnd,
      InputsModel[[id]]
    )
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
      stop(
        "The Argument ",
        inputArg,
        " must have a number of rows identical to the lenght of `IndPeriod_Run`"
      )
    }
    for (id in colnames(input)) {
      v <- input[, id, drop = TRUE]
      if (inputArg %in% c("Qrelease", "Qmin")) {
        if (inputArg == "Qrelease" && !InputsModel[[id]]$isReservoir) {
          stop(
            "The column ",
            id,
            " of the argument `Qrelease` does not refer to a Reservoir node"
          )
        }
        if (inputArg == "Qmin" && !InputsModel[[id]]$hasDiversion) {
          stop(
            "The column ",
            id,
            " of the argument `Qmin` does not refer to a Diversion node"
          )
        }
        if (is.null(InputsModel[[id]][[inputArg]])) {
          stop(
            "InputsModel[['",
            id,
            "']] should contain a `",
            inputArg,
            "` item"
          )
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
            stop(
              "The column ",
              id,
              " of the argument `Qinf` does not refer to a DirectInjection or a Diversion node"
            )
          }
          # Update withdrawal due to Diversion
          InputsModel[[id]]$Qdiv[IndPeriod_Run] <- -v
        }
      }
    }
  }

  if (use_supervisor) {
    sv$InputsModel <- InputsModel
    Yinit <- getYinit(sv, InputsModel, x)
    OM <- suppressMessages(
      RunModel(
        sv,
        RunOptions = RunOptions,
        Param = extractParam(x),
        Yinit = Yinit
      )
    )
  } else {
    OM <- suppressMessages(
      RunModel(
        InputsModel,
        RunOptions = RunOptions,
        Param = extractParam(x)
      )
    )
  }

  # Run the model

  if (merge_outputs) {
    OM <- merge(x, OM)
  }

  return(OM)
}
