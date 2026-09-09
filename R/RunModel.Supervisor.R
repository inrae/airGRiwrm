#' RunModel function for a Supervisor
#'
#' This function acts as [RunModel.GRiwrmInputsModel] but for a model
#' containing a [Supervisor][CreateSupervisor].
#'
#' @param x Object of class [Supervisor][CreateSupervisor] see [CreateSupervisor]
#' for details
#' @param RunOptions Object of class [GRiwrmRunOptions][CreateRunOptions],
#' see [CreateRunOptions] for details
#' @param Param [list] of parameter values (See .
#' The list item names are the IDs of the sub-basins.
#' Each item is a vector of numerical parameters
#' @param Yinit [list] of initial values for the Y variables of the controllers
#' (See details)
#' @param ... Further arguments for compatibility with S3 methods
#'
#' @details
#' `Yinit` is used for allowing to run the supervisor at the first supervision
#' time step when no simulation data are available.
#' It's a list with items named by the controller ids, and each item is a
#' matrix with one column by controlled and/or measured variable `Y` for this
#' controller and one row by supervision time step (See [CreateSupervisor]).
#'
#' @return \emph{GRiwrmOutputsModel} object which is a list of \emph{OutputsModel} objects (See [airGR::RunModel]) for each node of the semi-distributed model
#' @export
#'
#' @example man-examples/RunModel.Supervisor.R
RunModel.Supervisor <- function(x, RunOptions, Param, Yinit = NULL, ...) {
  stopifnot(is.Supervisor(x), inherits(RunOptions, "GRiwrmRunOptions"))

  # Save InputsModel for restoration at the end (Supervisor is an environment...)
  InputsModelBackup <- x$InputsModel

  # Time steps handling
  IndPeriod_Run <- RunOptions[[1]]$IndPeriod_Run
  x$ts.index0 <- IndPeriod_Run[1] - 1
  ts.start <- IndPeriod_Run[1]
  ts.end <- IndPeriod_Run[length(IndPeriod_Run)]
  superTSstarts <- seq(ts.start, ts.end, x$.TimeStep)
  lSuperTS <- lapply(
    superTSstarts,
    function(x, TS, xMax) {
      seq(x, min(x + TS - 1, xMax))
    },
    TS = x$.TimeStep,
    xMax = ts.end
  )

  # Run runoff model for each sub-basin
  x$OutputsModel <- lapply(X = x$InputsModel, FUN = function(IM) {
    if (inherits(IM, "GR")) {
      OM_GR <- RunModel.GR(
        IM,
        RunOptions = RunOptions[[IM$id]],
        Param = Param[[IM$id]]
      )
      if (IM$hasDiversion) {
        OM_GR$Qnat <- OM_GR$Qsim
      }
      return(OM_GR)
    }
  })
  class(x$OutputsModel) <- c("GRiwrmOutputsModel", class(x$OutputsModel))
  OutputsModelGR <- x$OutputsModel

  # Copy simulated pure runoff flows (no SD nor Diversion nodes) to Qupstream
  for (id in getNoSD_Ids(x$InputsModel, include_diversion = FALSE)) {
    updateQupstream.Supervisor(x, id, IndPeriod_Run)
  }

  # Initialization of model states by running the model with no supervision on warm-up period
  if (!identical(RunOptions[[1]]$IndPeriod_WarmUp, 0L)) {
    RunOptionsWarmUp <- RunOptions
    for (id in names(x$InputsModel)) {
      RunOptionsWarmUp[[id]]$IndPeriod_Run <- RunOptionsWarmUp[[
        id
      ]]$IndPeriod_WarmUp
      RunOptionsWarmUp[[id]]$IndPeriod_WarmUp <- 0L
      RunOptionsWarmUp[[id]]$Outputs_Sim <- c("StateEnd", "Qsim")
      if (x$InputsModel[[id]]$isReservoir) {
        RunOptionsWarmUp[[id]]$Outputs_Sim <- c(
          RunOptionsWarmUp[[id]]$Outputs_Sim,
          "Qsim_m3"
        )
      }
    }
    OM_WarmUp <- suppressMessages(
      RunModel.GRiwrmInputsModel(
        x$InputsModel,
        RunOptions = RunOptionsWarmUp,
        Param = Param
      )
    )
  } else {
    OM_WarmUp <- NULL
  }

  SD_Ids <- setNames(nm = getSD_Ids(x$InputsModel, add_diversions = TRUE))

  # Set Outputs to archive for final restitution
  outputVars <- lapply(SD_Ids, function(id) {
    ov <- c("Qsim_m3", "Qover_m3")
    if (inherits(x$InputsModel[[id]], "GR")) {
      ov <- c(ov, "Qsim", "QsimDown")
    }
    if (x$InputsModel[[id]]$hasDiversion) {
      ov <- c(ov, "Qdiv_m3")
      if (!x$InputsModel[[id]]$isReservoir) {
        ov <- c(ov, "Qnat")
      }
    }
    if (x$InputsModel[[id]]$isReservoir) {
      ov <- c(ov, "Qinflows_m3", "Vsim")
    }
    return(ov)
  })

  # Adapt RunOptions to step by step simulation and copy states
  for (id in SD_Ids) {
    RunOptions[[id]]$IndPeriod_WarmUp <- 0L
    RunOptions[[id]]$Outputs_Sim <- c(
      "Qsim",
      "Qsim_m3",
      "QsimDown",
      "StateEnd",
      "Param"
    )
    if (!is.null(OM_WarmUp)) {
      x$OutputsModel[[id]]$StateEnd <- OM_WarmUp[[id]]$StateEnd
    } else {
      x$OutputsModel[[id]]$StateEnd <- RunOptions[[id]]$IniStates
    }
  }

  # Store OutputsModel for step by step simulation
  x$storedOutputs <- initStoredOutputs(x, outputVars)

  message("Processing: 0%", appendLF = FALSE)
  iProgressSteps <- round(length(lSuperTS) * seq(0.1, 0.9, 0.1))

  # Loop over time steps with a step equal to the supervision time step
  for (i in seq_along(lSuperTS)) {
    iProgressMessage <- which(i == iProgressSteps)
    if (length(iProgressMessage) == 1) {
      message(" ", 10 * iProgressMessage, "%", appendLF = FALSE)
    }
    iTS <- lSuperTS[[i]]
    # Run regulation on the whole basin for the current time step
    x$ts.current <- iTS
    x$ts.index <- iTS - x$ts.index0
    x$ts.date <- x$InputsModel[[1]]$DatesR[iTS]
    # Regulation occurs from second time step
    if (iTS[1] > ts.start || !is.null(Yinit)) {
      if (iTS[1] == ts.start) {
        checkYinit(x, Yinit)
        doSupervision(x, Yinit)
      } else {
        doSupervision(x)
      }
    }
    # Loop over sub-basin using SD model
    for (id in SD_Ids) {
      # Run model for the sub-basin and one time step
      RunOptions[[id]]$IniStates <- serializeIniStates(
        x$OutputsModel[[id]]$StateEnd,
        x$InputsModel[[id]]
      )
      RunOptions[[id]]$IndPeriod_Run <- iTS
      # Route upstream flows for SD nodes
      if (x$InputsModel[[id]]$isReservoir) {
        x$OutputsModel[[id]] <- RunModel_Reservoir(
          x$InputsModel[[id]],
          RunOptions = RunOptions[[id]],
          Param = Param[[id]]
        )
      } else {
        if (id %in% colnames(x$storedOutputs$QcontribDown)) {
          QcontribDown <- x$storedOutputs$QcontribDown[x$ts.index, id]
        } else {
          QcontribDown <- NULL
        }
        x$OutputsModel[[id]] <- suppressWarnings(
          RunModel_Routing(
            x$InputsModel[[id]],
            RunOptions = RunOptions[[id]],
            Param = Param[[id]],
            QcontribDown = QcontribDown
          )
        )
      }
      if (x$InputsModel[[id]]$hasDiversion) {
        # Compute diverted and simulated flows on Diversion nodes
        x$OutputsModel[[id]] <-
          RunModel_Diversion(
            x$InputsModel[[id]],
            RunOptions = RunOptions[[id]],
            OutputsModel = x$OutputsModel[[id]]
          )
      }
      # Storing Qsim_m3 and Qdiv_m3 data.frames
      for (outputVar in outputVars[[id]]) {
        x$storedOutputs[[outputVar]][x$ts.index, id] <- x$OutputsModel[[id]][[
          outputVar
        ]]
      }
      # Routing Qsim_m3 and Qdiv_m3 to Qupstream of downstream nodes
      updateQupstream.Supervisor(x, id, iTS)
    }
    x$ts.previous <- x$ts.index
  }

  message(" 100%")

  for (id in SD_Ids) {
    if (inherits(x$InputsModel[[id]], "GR")) {
      # Copy OutputsModel from original run of GR contribution for the whole time series
      # Except for StateEnd which has been modified
      StateEnd <- x$OutputsModel[[id]]$StateEnd
      x$OutputsModel[[id]] <- OutputsModelGR[[id]]
      class_StateEnd <- class(x$OutputsModel[[id]]$StateEnd)
      x$OutputsModel[[id]]$StateEnd <- c(
        x$OutputsModel[[id]]$StateEnd,
        StateEnd
      )
      class(x$OutputsModel[[id]]$StateEnd) <- class_StateEnd
    } else {
      # Add missing DatesR for non GR models
      x$OutputsModel[[id]]$DatesR <- x$InputsModel[[1]]$DatesR[IndPeriod_Run]
    }
    # Copy stored outputs for the whole time series
    for (outputVar in outputVars[[id]]) {
      x$OutputsModel[[id]][[outputVar]] <- x$storedOutputs[[outputVar]][, id]
    }
    if (sum(x$InputsModel[[id]]$BasinAreas, na.rm = TRUE) > 0) {
      x$OutputsModel[[id]]$Qsim <-
        x$storedOutputs$Qsim_m3[, id] /
        sum(x$InputsModel[[id]]$BasinAreas, na.rm = TRUE) /
        1e3
      x$OutputsModel[[id]]$RunOptions$WarmUpQsim <- OM_WarmUp[[id]]$Qsim_m3 /
        sum(x$InputsModel[[id]]$BasinAreas, na.rm = TRUE) /
        1e3
    }
    x$OutputsModel[[id]]$RunOptions$WarmUpQsim_m3 <- OM_WarmUp[[id]]$Qsim_m3
    x$OutputsModel[[id]]$RunOptions$Param <- Param[[id]]
  }

  x$OutputsModel <- add_OutputsModel_attributes(
    x$InputsModel,
    x$OutputsModel,
    IndPeriod_Run
  )

  # restoration of InputsModel (Supervisor is an environment...)
  x$InputsModel <- InputsModelBackup

  return(x$OutputsModel)
}


updateQupstream.Supervisor <- function(x, id, iTS) {
  downId <- x$InputsModel[[id]]$down
  if (!is.null(x$InputsModel[[downId]])) {
    x$InputsModel[[downId]]$Qupstream[iTS, id] <-
      x$OutputsModel[[id]]$Qsim_m3
  }
  if (x$InputsModel[[id]]$hasDiversion) {
    divOutId <- x$InputsModel[[id]]$diversionOutlet
    if (!is.na(divOutId)) {
      x$InputsModel[[divOutId]]$Qupstream[iTS, id] <-
        x$OutputsModel[[id]]$Qdiv_m3
    }
  }
}
