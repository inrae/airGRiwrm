#' RunModel function for GRiwrmInputsModel object
#'
#' @param x object of class `Supervisor`, see [CreateSupervisor] for details.
#' @param RunOptions object of class \emph{GRiwrmRunOptions}, see \code{[CreateRunOptions.GRiwrm]} for details.
#' @param Param list of parameter. The list item names are the IDs of the sub-basins. Each item is a vector of numerical parameters.
#' @param ... Mandatory for S3 method signature function compatibility with generic.
#'
#' @return \emph{GRiwrmOutputsModel} object which is a list of \emph{OutputsModel} objects (See \code{\link[airGR]{RunModel}}) for each node of the semi-distributed model.
#' @export
RunModel.Supervisor <- function(x, RunOptions, Param, ...) {

  # Time steps handling
  x$ts.index0 <- RunOptions[[1]]$IndPeriod_Run[1] - 1
  ts.start <- RunOptions[[1]]$IndPeriod_Run[1]
  ts.end <- RunOptions[[1]]$IndPeriod_Run[length(RunOptions[[1]]$IndPeriod_Run)]
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
      x$OutputsModel[[id]]$Qsim
  }

  # Save Qsim for step by step simulation
  Qsim <- lapply(x$OutputsModel, function(OM) {
    OM$Qsim
  })

  # Adapt RunOptions to step by step simulation
  for(id in getSD_Ids(x$InputsModel)) {
    RunOptions[[id]]$IndPeriod_WarmUp <- 0L
    RunOptions[[id]]$Outputs_Sim <- "StateEnd"
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
      RunOptions[[id]]$IniStates <- unlist(x$OutputsModel[[id]]$StateEnd)
      x$OutputsModel[[id]] <- RunModel.SD(
        x$InputsModel[[id]],
        RunOptions = RunOptions[[id]],
        Param = Param[[id]],
        QsimDown = Qsim[[id]][x$ts.index]
      )
      # Storing Qsim in the data.frame Qsim
      Qsim[[id]][x$ts.index] <- x$OutputsModel[[id]]$Qsim
      # Routing Qsim to the downstream node
      if(!is.na(x$InputsModel[[id]]$down)) {
        x$InputsModel[[x$InputsModel[[id]]$down]]$Qupstream[iTS, id] <-
          x$OutputsModel[[id]]$Qsim
      }
    }
    x$ts.previous <- x$ts.index
  }
  for(id in getSD_Ids(x$InputsModel)) {
    x$OutputsModel[[id]]$Qsim <- Qsim[[id]]
  }
  attr(x$OutputsModel, "Qm3s") <- OutputsModelQsim(x$InputsModel, x$OutputsModel, RunOptions[[1]]$IndPeriod_Run)
  return(x$OutputsModel)
}
