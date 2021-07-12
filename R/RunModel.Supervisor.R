#' RunModel function for a GRiwrmInputsModel object
#'
#' @param x \[object of class `Supervisor`\] see [CreateSupervisor] for details
#' @param RunOptions \[object of class \emph{GRiwrmRunOptions}\] see \code{[CreateRunOptions.GRiwrm]} for details
#' @param Param [list] parameter values. The list item names are the IDs of the sub-basins. Each item is a vector of numerical parameters
#' @param ... Further arguments for compatibility with S3 methods
#'
#' @return \emph{GRiwrmOutputsModel} object which is a list of \emph{OutputsModel} objects (See [airGR::RunModel]) for each node of the semi-distributed model
#' @export
RunModel.Supervisor <- function(x, RunOptions, Param, ...) {

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
    RunModel.GR(IM,
                RunOptions = RunOptions[[IM$id]],
                Param = Param[[IM$id]])
  })
  class(x$OutputsModel) <- append(class(x$OutputsModel), "GRiwrmOutputsModel")
  # Copy simulated pure runoff flows (no SD nodes) to Qupstream in downstream SD nodes
  for(id in getNoSD_Ids(x$InputsModel)) {
    downId <- x$InputsModel[[id]]$down
    x$InputsModel[[downId]]$Qupstream[RunOptions[[downId]]$IndPeriod_Run, id] <-
      x$OutputsModel[[id]]$Qsim_m3
  }

  # Save Qsim for step by step simulation
  QcontribDown <- do.call(
    cbind,
    lapply(x$OutputsModel, "[[", "Qsim")
  )

  Qsim_m3 <- do.call(
    cbind,
    lapply(x$OutputsModel, "[[", "Qsim_m3")
  )

  # Initialisation of model states by running the model with no supervision on warm-up period
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

  # Loop over time steps with a step equal to the supervision time step
  for(iTS in lSuperTS) {
    # Run regulation on the whole basin for the current time step
    x$ts.index <- iTS - x$ts.index0
    x$ts.date <- x$InputsModel[[1]]$DatesR[iTS]
    # Regulation occurs from second time step
    if(iTS[1] > ts.start) {
      doSupervision(x)
    }

    # Loop over sub-basin using SD model
    for(id in getSD_Ids(x$InputsModel)) {
      # Run the SD model for the sub-basin and one time step
      RunOptions[[id]]$IndPeriod_Run <- iTS
      RunOptions[[id]]$IniStates <- serializeIniStates(x$OutputsModel[[id]]$StateEnd)
      x$OutputsModel[[id]] <- RunModel.SD(
        x$InputsModel[[id]],
        RunOptions = RunOptions[[id]],
        Param = Param[[id]],
        QcontribDown = QcontribDown[x$ts.index, id]
      )
      # Storing Qsim in the data.frame Qsim
      Qsim_m3[x$ts.index, id] <- x$OutputsModel[[id]]$Qsim_m3
      # Routing Qsim to Qupstream of downstream nodes
      if(!is.na(x$InputsModel[[id]]$down)) {
        x$InputsModel[[x$InputsModel[[id]]$down]]$Qupstream[iTS, id] <-
          x$OutputsModel[[id]]$Qsim_m3
      }
    }
    x$ts.previous <- x$ts.index
  }
  for(id in getSD_Ids(x$InputsModel)) {
    x$OutputsModel[[id]]$Qsim_m3 <- Qsim_m3[, id]
    x$OutputsModel[[id]]$Qsim <-
      Qsim_m3[, id] / sum(x$InputsModel[[id]]$BasinAreas, na.rm = TRUE) / 1e3
  }
  attr(x$OutputsModel, "Qm3s") <- OutputsModelQsim(x$InputsModel, x$OutputsModel, IndPeriod_Run)

  # restoration of InputsModel (Supervisor is an environment...)
  x$InputsModel <- InputsModelBackup

  return(x$OutputsModel)
}
