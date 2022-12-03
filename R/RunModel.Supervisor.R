#' RunModel function for a GRiwrmInputsModel object
#'
#' @param x \[object of class `Supervisor`\] see [CreateSupervisor] for details
#' @param RunOptions \[object of class \emph{GRiwrmRunOptions}\] see \code{[CreateRunOptions.GRiwrm]} for details
#' @param Param [list] parameter values. The list item names are the IDs of the sub-basins. Each item is a vector of numerical parameters
#' @param ... Further arguments for compatibility with S3 methods
#'
#' @return \emph{GRiwrmOutputsModel} object which is a list of \emph{OutputsModel} objects (See [airGR::RunModel]) for each node of the semi-distributed model
#' @export
#'
#' @example man-examples/RunModel.Supervisor.R
RunModel.Supervisor <- function(x, RunOptions, Param, ...) {

  stopifnot(is.Supervisor(x),
            inherits(RunOptions, "GRiwrmRunOptions"))

  # Save InputsModel for restoration at the end (Supervisor is an environment...)
  InputsModelBackup <- x$InputsModel

  # Time steps handling
  IndPeriod_Run <- RunOptions[[1]]$IndPeriod_Run
  x$ts.index0 <- IndPeriod_Run[1] - 1
  ts.start <- IndPeriod_Run[1]
  ts.end <- IndPeriod_Run[length(IndPeriod_Run)]
  superTSstarts <- seq(ts.start, ts.end, x$.TimeStep)
  lSuperTS <- lapply(
    superTSstarts, function(x, TS, xMax) {
      seq(x, min(x + TS - 1, xMax))
    },
    TS = x$.TimeStep,
    xMax = ts.end
  )

  # Run runoff model for each sub-basin
  x$OutputsModel <- lapply(X = x$InputsModel, FUN = function(IM) {
    if(inherits(IM, "GR")) {
      OM_GR <- RunModel.GR(IM,
                           RunOptions = RunOptions[[IM$id]],
                           Param = Param[[IM$id]])
      if (IM$hasDiversion) OM_GR$Qnat <- OM_GR$Qsim
      return(OM_GR)
    }
  })
  class(x$OutputsModel) <- c("GRiwrmOutputsModel", class(x$OutputsModel))

  # Copy simulated pure runoff flows (no SD nor Diversion nodes) to Qupstream
  for(id in getNoSD_Ids(x$InputsModel, include_diversion = FALSE)) {
    updateQupstream.Supervisor(x, id, IndPeriod_Run)
  }

  # Store OutputsModel for step by step simulation
  x$storedOutputs <- initStoredOutputs(x)

  # Initialization of model states by running the model with no supervision on warm-up period
  RunOptionsWarmUp <- RunOptions
  for(id in names(x$InputsModel)) {
    RunOptionsWarmUp[[id]]$IndPeriod_Run <- RunOptionsWarmUp[[id]]$IndPeriod_WarmUp
    RunOptionsWarmUp[[id]]$IndPeriod_WarmUp <- 0L
    RunOptionsWarmUp[[id]]$Outputs_Sim <- c("StateEnd", "Qsim")
  }
  OM_WarmUp <- suppressMessages(
    RunModel.GRiwrmInputsModel(x$InputsModel,
                               RunOptions = RunOptionsWarmUp,
                               Param = Param)
  )

  # Adapt RunOptions to step by step simulation and copy states
  SD_Ids <- getSD_Ids(x$InputsModel)
  names(SD_Ids) <- SD_Ids
  for(id in SD_Ids) {
    RunOptions[[id]]$IndPeriod_WarmUp <- 0L
    RunOptions[[id]]$Outputs_Sim <- c("Qsim_m3", "StateEnd")
    x$OutputsModel[[id]]$StateEnd <- serializeIniStates(OM_WarmUp[[id]]$StateEnd)
  }

  # Set Outputs to archive for final restitution
  outputVars <- lapply(SD_Ids, function(id) {
    ov <- "Qsim_m3"
    if (x$InputsModel[[id]]$hasDiversion) {
      ov <- c(ov, "Qdiv_m3", "Qnat")
    } else if (x$InputsModel[[id]]$isReservoir) {
      ov <- c(ov, "Qinflows_m3", "Vsim")
    }
    return(ov)
  })

  message("Processing: 0%", appendLF = FALSE)
  iProgressSteps <- round(length(lSuperTS) * seq(0.1, 0.9, 0.1))

  # Loop over time steps with a step equal to the supervision time step
  for(i in seq_along(lSuperTS)) {
    iProgressMessage <- which(i == iProgressSteps)
    if (length(iProgressMessage) == 1) {
      message(" ", 10 * iProgressMessage, "%", appendLF = FALSE)
    }
    iTS <- lSuperTS[[i]]
    # Run regulation on the whole basin for the current time step
    x$ts.index <- iTS - x$ts.index0
    x$ts.date <- x$InputsModel[[1]]$DatesR[iTS]
    # Regulation occurs from second time step
    if(iTS[1] > ts.start) {
      doSupervision(x)
    }
    # Loop over sub-basin using SD model
    for(id in getSD_Ids(x$InputsModel, add_diversion = TRUE)) {
      # Run model for the sub-basin and one time step
      RunOptions[[id]]$IniStates <- serializeIniStates(x$OutputsModel[[id]]$StateEnd)
      RunOptions[[id]]$IndPeriod_Run <- iTS
      if (RunOptions[[id]]$FeatFUN_MOD$IsSD) {
        # Route upstream flows for SD nodes
        x$OutputsModel[[id]] <- RunModel.SD(
          x$InputsModel[[id]],
          RunOptions = RunOptions[[id]],
          Param = Param[[id]],
          QcontribDown = x$storedOutputs$QcontribDown[x$ts.index, id]
        )
      } else {
        x$OutputsModel[[id]]$Qsim_m3 <- x$storedOutputs$Qsim_m3[x$ts.index, id]
      }
      if (x$InputsModel[[id]]$hasDiversion) {
        # Compute diverted and simulated flows on Diversion nodes
        x$OutputsModel[[id]] <-
          RunModel_Diversion(x$InputsModel[[id]],
                             RunOptions = RunOptions[[id]],
                             OutputsModel = x$OutputsModel[[id]])
      }
      # Storing Qsim_m3 and Qdiv_m3 data.frames
      for (outputVar in outputVars[[id]]) {
        x$storedOutputs[[outputVar]][x$ts.index, id] <- x$OutputsModel[[id]][[outputVar]]
      }
      # Routing Qsim_m3 and Qdiv_m3 to Qupstream of downstream nodes
      updateQupstream.Supervisor(x, id, iTS)
    }
    x$ts.previous <- x$ts.index
  }

  message(" 100%")

  for(id in getSD_Ids(x$InputsModel)) {
    x$OutputsModel[[id]]$DatesR <- x$DatesR[IndPeriod_Run]
    for (outputVar in outputVars[[id]]) {
      x$OutputsModel[[id]][[outputVar]] <- x$storedOutputs[[outputVar]][, id]
    }
    x$OutputsModel[[id]]$Qsim <-
      x$storedOutputs$Qsim_m3[, id] / sum(x$InputsModel[[id]]$BasinAreas, na.rm = TRUE) / 1e3
  }
  attr(x$OutputsModel, "Qm3s") <- OutputsModelQsim(x$InputsModel, x$OutputsModel, IndPeriod_Run)

  # restoration of InputsModel (Supervisor is an environment...)
  x$InputsModel <- InputsModelBackup

  return(x$OutputsModel)
}

updateQupstream.Supervisor <- function(x, id, iTS) {
  downId <- x$InputsModel[[id]]$down
  if(!is.null(x$InputsModel[[downId]])) {
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
