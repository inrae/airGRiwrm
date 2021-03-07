#' RunModel function for GRiwrmInputsModel object
#'
#' @param x object of class \emph{GRiwrmInputsModel}, see \code{[CreateInputsModel.GRiwrm]} for details.
#' @param RunOptions object of class \emph{GRiwrmRunOptions}, see \code{[CreateRunOptions.GRiwrm]} for details.
#' @param Param list of parameter. The list item names are the IDs of the sub-basins. Each item is a vector of numerical parameters.
#' @param ... Mandatory for S3 method signature function compatibility with generic.
#'
#' @return \emph{GRiwrmOutputsModel} object which is a list of \emph{OutputsModel} objects (See \code{\link[airGR]{RunModel}}) for each node of the semi-distributed model.
#' @export
RunModel.GRiwrmInputsModel <- function(x, RunOptions, Param, ...) {

  checkRunModelParameters(x, RunOptions, Param)

  OutputsModel <- list()
  class(OutputsModel) <- c("GRiwrmOutputsModel", class(OutputsModel))

  for(id in names(x)) {
    message("RunModel.GRiwrmInputsModel: Treating sub-basin ", x[[id]]$id, "...")

    # Update x[[id]]$Qupstream with simulated upstream flows
    if(any(x[[id]]$UpstreamIsRunoff)) {
      x[[id]] <- UpdateQsimUpstream(x[[id]], RunOptions[[id]]$IndPeriod_Run, OutputsModel)
    }

    # Run the model for the sub-basin
    OutputsModel[[id]] <- RunModel.InputsModel(
      x[[id]],
      RunOptions = RunOptions[[id]],
      Param = Param[[id]]
    )
  }
  attr(OutputsModel, "Qm3s") <- OutputsModelQsim(x, OutputsModel, RunOptions[[1]]$IndPeriod_Run)
  return(OutputsModel)
}
