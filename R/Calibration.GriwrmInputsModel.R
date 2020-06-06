#' Calibration of a semi-distributed run-off model
#'
#' @param InputsModel object of class \emph{GriwrmInputsModel}, see \code{\link{CreateInputsModel.Griwrm}} for details.
#' @param RunOptions object of class \emph{GriwrmRunOptions}, see \code{\link{CreateRunOptiosn.Griwrm}} for details.
#' @param InputsCrit object of class \emph{GriwrmInputsCrit}, see \code{\link{CreateInputsCrit.Griwrm}} for details.
#' @param CalibOptions object of class \emph{GriwrmCalibOptions}, see \code{\link{CreateCalibOptions.Griwrm}} for details.
#' @param useUpstreamQsim boolean describing if simulated (\code{TRUE}) or observed (\code{FALSE}) flows are used for calibration. Default is \code{TRUE}.
#' @param verbose (optional) boolean indicating if the function is run in verbose mode or not, default = \code{TRUE}
#' @param ... further arguments passed to \code{\link[airGR]{Calibration}}.
#'
#' @return GriwrmOutputsCalib object which is a list of OutputsCalib (See \code{\link[airGR]{Calibration}}) for each node of the semi-distributed model.
#' @export
Calibration.GriwrmInputsModel <- function(InputsModel,
                                          RunOptions,
                                          InputsCrit,
                                          CalibOptions,
                                          useUpstreamQsim = TRUE,
                                          verbose = TRUE,
                                          ...) {

  OutputsCalib <- list()
  class(OutputsCalib) <- append(class(OutputsCalib), "GriwrmOutputsCalib")

  OutputsModel <- list()
  class(OutputsModel) <- append(class(OutputsModel), "GriwrmOutputsModel")

  for(IM in InputsModel) {
    if(verbose) cat("Calibration.GriwrmInputsModel: Treating sub-basin", IM$id, "...\n")

    if(useUpstreamQsim) {
      # Update InputsModel$Qupstream with simulated upstream flows
      IM <- UpdateQsimUpstream(IM, OutputsModel)
    }

    OutputsCalib[[IM$id]] <- Calibration.InputsModel(
      InputsModel = IM,
      RunOptions = RunOptions[[IM$id]],
      InputsCrit = InputsCrit[[IM$id]],
      CalibOptions = CalibOptions[[IM$id]],
      ...
    )

    if(useUpstreamQsim) {
      # Run the model for the sub-basin
      OutputsModel[[IM$id]] <- RunModel(
        InputsModel = IM,
        RunOptions = RunOptions[[IM$id]],
        Param = OutputsCalib[[IM$id]]$ParamFinalR
      )
    }

  }

  return(OutputsCalib)

}
