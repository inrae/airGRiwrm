#' RunModel function for \emph{GRiwrmInputsModel} object
#'
#' @param x \[object of class \emph{GRiwrmInputsModel}\] see [CreateInputsModel.GRiwrm] for details
#' @param RunOptions \[object of class \emph{GRiwrmRunOptions}\] see [CreateRunOptions.GRiwrmInputsModel] for details
#' @param Param [list] parameter values. The list item names are the IDs of the sub-basins. Each item is a [numeric] [vector]
#' @param ... Further arguments for compatibility with S3 methods
#'
#' @return An object of class \emph{GRiwrmOutputsModel}. This object is a [list] of *OutputsModel* objects produced by [RunModel.InputsModel] for each node of the semi-distributed model.
#' @export
#' @seealso [CreateGRiwrm()], [CreateInputsModel.GRiwrm()], [CreateRunOptions()]
#' @example man-examples/RunModel.GRiwrmInputsModel.R
RunModel.GRiwrmInputsModel <- function(x, RunOptions, Param, ...) {

  checkRunModelParameters(x, RunOptions, Param)

  OutputsModel <- list()
  class(OutputsModel) <- c("GRiwrmOutputsModel", class(OutputsModel))

  for (id in names(x)) {
    message("RunModel.GRiwrmInputsModel: Processing sub-basin ", x[[id]]$id, "...")

    # Update x[[id]]$Qupstream with simulated upstream flows
    if (any(x[[id]]$UpstreamIsModeled)) {
      x[[id]] <- UpdateQsimUpstream(x[[id]], RunOptions[[id]], OutputsModel)
    }
    # Run the model for the sub-basin
    OutputsModel[[id]] <- RunModel.InputsModel(
      x[[id]],
      RunOptions = RunOptions[[id]],
      Param = Param[[id]]
    )
  }
  attr(OutputsModel, "Qm3s") <- OutputsModelQsim(x, OutputsModel, RunOptions[[1]]$IndPeriod_Run)
  attr(OutputsModel, "GRiwrm") <- attr(x, "GRiwrm")
  attr(OutputsModel, "TimeStep") <- attr(x, "TimeStep")
  return(OutputsModel)
}
