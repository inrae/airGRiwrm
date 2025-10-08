#' Check of the parameters of RunModel methods
#'
#' Stop the execution if an error is detected.
#'
#' @param InputsModel[GRiwrmInputsModel][CreateInputsModel.GRiwrm] object see
#' [CreateInputsModel.GRiwrm] for details
#' @param RunOptions [GRiwrmRunOptions][CreateRunOptions.GRiwrmInputsModel] object
#' see [CreateRunOptions.GRiwrmInputsModel] for details
#' @param Param [list] of containing model parameter values of each node of the network
#' @noRd
checkRunModelParameters <- function(InputsModel, RunOptions, Param) {
  if (!inherits(InputsModel, "GRiwrmInputsModel")) {
    stop(
      "`InputsModel` parameter must of class 'GRiwrmInputsModel' (See ?CreateRunOptions.GRiwrmInputsModel)"
    )
  }
  if (!inherits(RunOptions, "GRiwrmRunOptions")) {
    stop(
      "Argument `RunOptions` parameter must of class 'GRiwrmRunOptions' (See ?CreateRunOptions.GRiwrmInputsModel)"
    )
  }
  if (!is.list(Param) || !all(names(InputsModel) %in% names(Param))) {
    stop("Argument `Param` must be a list with names equal to nodes IDs")
  }
}


#' Creation of a data.frame with simulated flows at each node of the GRiwrm object
#'
#' @details
#' This function can only be called inside [RunModel.GRiwrmInputsModel] or [RunModel.Supervisor]
#' because it needs a `GRiwrmInputsModel` object internally modified by these functions
#' (`Qupstream` updated with simulated flows).
#'
#' @param InputsModel [GRiwrmInputsModel][CreateInputsModel.GRiwrm] object see [CreateInputsModel.GRiwrm] for details
#' @param OutputsModel [GRiwrmOutputsModel][RunModel.GRiwrmInputsModel] object see [RunModel.GRiwrmInputsModel] for details
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
  dfQsim <- cbind(
    data.frame(DatesR = InputsModel[[1]]$DatesR[IndPeriod_Run]),
    do.call(cbind, lQsim) / attr(InputsModel, "TimeStep")
  )
  dfQsim <- as.Qm3s(dfQsim)
  rownames(dfQsim) <- NULL
  return(dfQsim)
}


#' Convert IniStates list into a vector
#'
#' @param IniStates see [CreateIniStates]
#'
#' @return A vector as in `RunOptions$IniStates`
#' @noRd
#'
serializeIniStates <- function(IniStates, InputsModel) {
  stopifnot(inherits(InputsModel, "InputsModel"))
  if (!is.list(IniStates)) {
    return(IniStates)
  }
  ObjectClass <- class(InputsModel)
  if (!"CemaNeige" %in% ObjectClass) {
    fields <- c("G", "eTG", "Gthr", "Glocmax")

    for (f in fields) {
      if (any(is.na(IniStates$CemaNeigeLayers[[f]]))) {
        IniStates$CemaNeigeLayers[[f]] <- NULL
      }
    }
  }

  IniStates$Store$Rest <- rep(NA, 3)
  IniStates <- unlist(IniStates)
  IniStates[is.na(IniStates) & !grepl("SD", names(IniStates))] <- 0
  return(IniStates)
}


#' Cap negative `OutputsModel$Qsim_m3` to zero and fill `OutputsModel$Qover_m3`
#' with over-abstracted volumes
#'
#' @param O Either `OutputsModel` or `OutputsModel$RunOptions` (for warm-up Qsim)
#' @param WarmUp `TRUE` if `O` is `OutputsModel$RunOptions`
#'
#' @return Modified `OutputsModel` or `OutputsModel$RunOptions`
#' @noRd
#'
calcOverAbstraction <- function(O, WarmUp) {
  f <- list(sim = "Qsim_m3", over = "Qover_m3")
  if (WarmUp) {
    f <- lapply(f, function(x) paste0("WarmUp", x))
  }
  if (!is.null(O[[f$sim]])) {
    O[[f$over]] <- rep(0, length(O[[f$sim]]))
    if (any(!is.na(O[[f$sim]]) & O[[f$sim]] < 0)) {
      O[[f$over]][O[[f$sim]] < 0] <- -O[[f$sim]][
        !is.na(O[[f$sim]]) & O[[f$sim]] < 0
      ]
      O[[f$sim]][!is.na(O[[f$sim]]) & O[[f$sim]] < 0] <- 0
    }
  }
  return(O)
}


#' Get the next time steps date/time of a simulation
#'
#' @param x Object returned by [RunModel.GRiwrmInputsModel],
#' [RunModel.Supervisor], or [RunModel.GRiwrmOutputsModel]
#' @param TimeStep [integer] number of time steps to get after the end of the
#' simulation
#'
#' @return A [POSIXct] containing the date/time of the time steps following
#' the end of the simulation.
#' @export
#'
getNextTimeSteps <- function(x, TimeStep = 1L) {
  stopifnot(inherits(x, "GRiwrmOutputsModel"), is.integer(TimeStep))
  last_date <- dplyr::last(x[[1]]$DatesR)
  first_date <- last_date + attr(x, "TimeStep")
  return(seq(first_date, length.out = TimeStep, by = attr(x, "TimeStep")))
}


#' Merge Two outputs of airGR simulations
#'
#' @param x,y **OutputsModel** objects from [airGR::RunModel],
#' [RunModel.GRiwrmInputsModel], [RunModel.GRiwrmOutputsModel], [RunModel.Supervisor]
#' @param ... Not used
#'
#' @return An object **OutputsModel** with merged times series of simulation
#' results.
#' @rdname merge.OutputsModel
#' @export
#'
merge.OutputsModel <- function(x, y, ...) {
  items <- names(x)
  items <- items[!grepl("RunOptions|StateEnd", items)]
  for (item in items) {
    y[[item]] <- c(x[[item]], y[[item]])
  }
  # We keep original warm-up data
  if (!is.null(x$RunOptions$WarmUpQsim)) {
    y$RunOptions$WarmUpQsim <- x$RunOptions$WarmUpQsim
  }
  if (!is.null(x$RunOptions$WarmUpQsim_m3)) {
    y$RunOptions$WarmUpQsim_m3 <- x$RunOptions$WarmUpQsim_m3
  }
  return(y)
}

#' @rdname merge.OutputsModel
#' @export
merge.GRiwrmOutputsModel <- function(x, y, ...) {
  y_attributes <- attributes(y)
  y <- lapply(setNames(nm = names(y)), function(id) {
    merge(x[[id]], y[[id]])
  })
  attributes(y) <- y_attributes
  attr(y, "Qm3s") <- rbind(attr(x, "Qm3s"), attr(y, "Qm3s"))
  rownames(attr(y, "Qm3s")) <- NULL
  return(y)
}

add_OutputsModel_attributes <- function(
  InputsModel,
  OutputsModel,
  IndPeriod_Run
) {
  attr(OutputsModel, "Qm3s") <- OutputsModelQsim(
    InputsModel,
    OutputsModel,
    IndPeriod_Run
  )
  attr(OutputsModel, "GRiwrm") <- attr(InputsModel, "GRiwrm")
  attr(OutputsModel, "TimeStep") <- attr(InputsModel, "TimeStep")
  return(OutputsModel)
}

complete_OutputsModel <- function(OutputsModel, RunOptions, BasinAreas) {
  OutputsModel$RunOptions$TimeStep <- RunOptions$FeatFUN_MOD$TimeStep
  if (is.null(OutputsModel$Qsim_m3)) {
    # Add Qsim_m3 in m3/timestep
    OutputsModel$Qsim_m3 <-
      OutputsModel$Qsim * sum(BasinAreas, na.rm = TRUE) * 1e3
  }
  if (
    "WarmUpQsim" %in%
      RunOptions$Outputs_Sim &&
      is.null(OutputsModel$RunOptions$WarmUpQsim_m3)
  ) {
    OutputsModel$RunOptions$WarmUpQsim_m3 <-
      OutputsModel$RunOptions$WarmUpQsim * sum(BasinAreas, na.rm = TRUE) * 1e3
  }
  return(OutputsModel)
}
