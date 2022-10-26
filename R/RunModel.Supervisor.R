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
    OM_GR <- RunModel.GR(IM,
                         RunOptions = RunOptions[[IM$id]],
                         Param = Param[[IM$id]])
    if (IM$hasDiversion) OM_GR$Qnat <- OM_GR$Qsim
    return(OM_GR)
  })
  class(x$OutputsModel) <- c("GRiwrmOutputsModel", class(x$OutputsModel))

  # Copy simulated pure runoff flows (no SD nor Diversion nodes) to Qupstream
  for(id in getNoSD_Ids(x$InputsModel, include_diversion = FALSE)) {
    updateQupstream.Supervisor(x, id, IndPeriod_Run)
  }

  # Save OutputsModel for step by step simulation
  QcontribDown <- do.call(
    cbind,
    lapply(x$OutputsModel, "[[", "Qsim")
  )
  Qsim_m3 <- do.call(
    cbind,
    lapply(x$OutputsModel, "[[", "Qsim_m3")
  )
  if (length(getDiversionRows(x$griwrm)) > 0) {
    # Outputs of Diversion nodes
    Qdiv_m3 <- Qsim_m3[, x$griwrm$id[getDiversionRows(x$griwrm)], drop = FALSE] * NA
    Qnat <- Qdiv_m3
  }

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
  for(id in getSD_Ids(x$InputsModel)) {
    RunOptions[[id]]$IndPeriod_WarmUp <- 0L
    RunOptions[[id]]$Outputs_Sim <- c("Qsim_m3", "StateEnd")
    x$OutputsModel[[id]]$StateEnd <- serializeIniStates(OM_WarmUp[[id]]$StateEnd)
  }

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
          QcontribDown = QcontribDown[x$ts.index, id]
        )
      } else {
        x$OutputsModel[[id]]$Qsim_m3 <- Qsim_m3[x$ts.index, id]
      }
      if (x$InputsModel[[id]]$hasDiversion) {
        # Compute diverted and simulated flows on Diversion nodes
        x$OutputsModel[[id]] <-
          RunModel_Diversion(x$InputsModel[[id]],
                             RunOptions = RunOptions[[id]],
                             OutputsModel = x$OutputsModel[[id]])
      }
      # Storing Qsim_m3 and Qdiv_m3 data.frames
      Qsim_m3[x$ts.index, id] <- x$OutputsModel[[id]]$Qsim_m3
      if (x$InputsModel[[id]]$hasDiversion) {
        Qdiv_m3[x$ts.index, id] <- x$OutputsModel[[id]]$Qdiv_m3
      }
      # Routing Qsim_m3 and Qdiv_m3 to Qupstream of downstream nodes
      updateQupstream.Supervisor(x, id, iTS)
    }
    x$ts.previous <- x$ts.index
  }

  message(" 100%")

  for(id in getSD_Ids(x$InputsModel)) {
    x$OutputsModel[[id]]$Qsim_m3 <- Qsim_m3[, id]
    x$OutputsModel[[id]]$Qsim <-
      Qsim_m3[, id] / sum(x$InputsModel[[id]]$BasinAreas, na.rm = TRUE) / 1e3
    if (x$InputsModel[[id]]$hasDiversion) {
      x$OutputsModel[[id]]$Qdiv_m3 <- Qdiv_m3[, id]
    }
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
