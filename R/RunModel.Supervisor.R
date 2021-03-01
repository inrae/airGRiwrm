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

  x$ts.index0 <- sapply(RunOptions, function(x) {
    x$IndPeriod_Run[1] - 1
  })

  # Run runoff model for each sub-basin
  x$OutputsModel <- lapply(X = x$InputsModel, FUN = function(IM) {
    RunModel.GR(IM,
                RunOptions = RunOptions[[IM$id]],
                Param = Param[[IM$id]])
    })
  class(x$OutputsModel) <- append(class(x$OutputsModel), "GRiwrmOutputsModel")
  # Save Qsim for step by step simulation
  Qsim <- lapply(x$OutputsModel, function(OM) {
    OM$Qsim
  })

  # Adapt RunOptions to step by step simulation
  for(id in getSD_Ids(x$InputsModel)) {
    RunOptions[[id]]$IndPeriod_WarmUp <- 0L
    RunOptions[[id]]$Outputs_Sim <- "StateEnd"
  }

  # Loop over time steps
  for(iTS in RunOptions[[1]]$IndPeriod_Run) {
    # Run regulation on the whole basin for the current time step
    x$ts.index <- iTS
    x$ts.date <- x$InputsModel[[1]]$DatesR[iTS]
    doSupervision(x)

    # Loop over sub-basin using SD model
    for(id in getSD_Ids(x$InputsModel)) {

      # Update InputsModel$Qupstream with simulated upstream flows
      for(i in which(x$InputsModel[[id]]$UpstreamIsRunoff)) {
        x$InputsModel[[id]]$Qupstream[iTS, i] <-
          x$OutputsModel[[x$InputsModel[[id]]$UpstreamNodes[i]]]$Qsim[iTS - x$ts.index0]
      }

      # Run the SD model for the sub-basin and one time step
      RunOptions[[id]]$IndPeriod_Run <- iTS
      RunOptions[[id]]$IniStates <- unlist(x$OutputsModel[[id]]$StateEnd)
      x$OutputsModel[[id]] <- RunModel.SD(
        x$InputsModel[[id]],
        RunOptions = RunOptions[[id]],
        Param = Param[[id]],
        QsimDown = Qsim[[id]][iTS - x$ts.index0]
      )
      Qsim[[id]][iTS - x$ts.index0] <- x$OutputsModel[[id]]$Qsim
    }
  }
  for(id in getSD_Ids(x$InputsModel)) {
    x$OutputsModel[[id]]$Qsim <- Qsim[[id]]
  }
  return(x$OutputsModel)
}
