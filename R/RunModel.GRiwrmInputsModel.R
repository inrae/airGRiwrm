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

  for(IM in x) {
    message("RunModel.GRiwrmInputsModel: Treating sub-basin ", IM$id, "...")

    # Update x$Qupstream with simulated upstream flows
    if(any(IM$UpstreamIsRunoff)) {
      IM <- UpdateQsimUpstream(IM, RunOptions[[IM$id]]$IndPeriod_Run, OutputsModel)
    }

    # Run the model for the sub-basin
    OutputsModel[[IM$id]] <- RunModel.InputsModel(
      IM,
      RunOptions = RunOptions[[IM$id]],
      Param = Param[[IM$id]]
    )

  }
  return(OutputsModel)
}
