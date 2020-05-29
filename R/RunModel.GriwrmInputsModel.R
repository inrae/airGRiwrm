#' Title
#'
#' @param InputsModel object of class \emph{GriwrmInputsModel}, see \code{[CreateInputsModel.Griwrm]} for details.
#' @param RunOptions object of class \emph{GriwrmRunOptions}, see \code{[CreateRunOptions.Griwrm]} for details.
#' @param girop Girop object giving the run-off model parameters, see \code{[Girop]}.
#' @param verbose (optional) boolean indicating if the function is run in verbose mode or not, default = \code{TRUE}
#' @param ... Mandatory for S3 method signature function compatibility with generic.
#'
#' @return \emph{GriwrmOutputsModel} object which is a list of \emph{OutputsModel} objects (See \code{\link[airGR]{RunModel}}) for each node of the semi-distributed model.
#' @export
RunModel.GriwrmInputsModel <- function(InputsModel, RunOptions, girop, verbose = TRUE, ...) {

  OutputsModel <- list()
  class(OutputsModel) <- append(class(OutputsModel), "GriwrmOutputsModel")

  for(IM in InputsModel) {
    if(verbose) cat("RunModel.GriwrmInputsModel: Treating sub-basin", IM$id, "...\n")

    # Update InputsModel$QobsUpstr with simulated upstream flows
    IM <- UpdateQsimUpstream(IM, OutputsModel)

    # Run the model for the sub-basin
    OutputsModel[[IM$id]] <- RunModel(
      InputsModel = IM,
      RunOptions = RunOptions[[IM$id]],
      Param = unlist(girop$params[girop$id == IM$id])
    )

  }
  return(OutputsModel)
}
