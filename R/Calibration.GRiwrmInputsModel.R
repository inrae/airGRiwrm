#' Calibration of a semi-distributed run-off model
#'
#' @param InputsModel object of class \emph{GRiwrmInputsModel}, see \code{\link{CreateInputsModel.GRiwrm}} for details.
#' @param RunOptions object of class \emph{GRiwrmRunOptions}, see \code{\link{CreateRunOptiosn.GRiwrm}} for details.
#' @param InputsCrit object of class \emph{GRiwrmInputsCrit}, see \code{\link{CreateInputsCrit.GRiwrm}} for details.
#' @param CalibOptions object of class \emph{GRiwrmCalibOptions}, see \code{\link{CreateCalibOptions.GRiwrm}} for details.
#' @param useUpstreamQsim boolean describing if simulated (\code{TRUE}) or observed (\code{FALSE}) flows are used for calibration. Default is \code{TRUE}.
#' @param verbose (optional) boolean indicating if the function is run in verbose mode or not, default = \code{TRUE}
#' @param ... further arguments passed to \code{\link[airGR]{Calibration}}.
#'
#' @return GRiwrmOutputsCalib object which is a list of OutputsCalib (See \code{\link[airGR]{Calibration}}) for each node of the semi-distributed model.
#' @export
Calibration.GRiwrmInputsModel <- function(InputsModel,
                                          RunOptions,
                                          InputsCrit,
                                          CalibOptions,
                                          useUpstreamQsim = TRUE,
                                          verbose = TRUE,
                                          ...) {

  OutputsCalib <- list()
  class(OutputsCalib) <- append(class(OutputsCalib), "GRiwrmOutputsCalib")

  OutputsModel <- list()
  class(OutputsModel) <- append(class(OutputsModel), "GRiwrmOutputsModel")

  for(IM in InputsModel) {
    if(verbose) cat("Calibration.GRiwrmInputsModel: Treating sub-basin", IM$id, "...\n")

    if(useUpstreamQsim && any(IM$UpstreamIsRunoff)) {
      # Update InputsModel$Qupstream with simulated upstream flows
      IM <- UpdateQsimUpstream(IM, RunOptions[[IM$id]]$IndPeriod_Run, OutputsModel)
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
