#' RunModel function for GRiwrmInputsModel object
#'
#' @param InputsModel object of class \emph{GRiwrmInputsModel}, see \code{[CreateInputsModel.GRiwrm]} for details.
#' @param RunOptions object of class \emph{GRiwrmRunOptions}, see \code{[CreateRunOptions.GRiwrm]} for details.
#' @param Param list of parameter. The list item names are the IDs of the sub-basins. Each item is a vector of numerical parameters.
#' @param ... Mandatory for S3 method signature function compatibility with generic.
#'
#' @return \emph{GRiwrmOutputsModel} object which is a list of \emph{OutputsModel} objects (See \code{\link[airGR]{RunModel}}) for each node of the semi-distributed model.
#' @export
RunModel.GRiwrmInputsModel <- function(InputsModel, RunOptions, Param, ...) {

  # Run runoff model for each sub-basin
  OutputsModel <- lapply(X = InputsModel, FUN = function(IM) {
    RunModel.GR(InputsModel = IM,
                RunOptions = RunOptions[[IM$id]],
                Param = Param[[IM$id]])
    })
  class(OutputsModel) <- append(class(OutputsModel), "GRiwrmOutputsModel")

  # Loop over sub-basin using SD model
  for(id in getSD_Ids(InputsModel)) {
    IM <- InputsModel[[id]]
    message("RunModel.GRiwrmInputsModel: Treating sub-basin ", id, "...")

    # Update InputsModel$Qupstream with simulated upstream flows
    if(any(IM$UpstreamIsRunoff)) {
      IM <- UpdateQsimUpstream(IM, RunOptions[[id]]$IndPeriod_Run, OutputsModel)
    }

    # Run the SD model for the sub-basin
    OutputsModel[[id]] <- RunModel.SD(
      InputsModel = IM,
      RunOptions = RunOptions[[id]],
      Param = Param[[id]],
      OutputsModel[[id]]
    )

  }
  return(OutputsModel)
}
