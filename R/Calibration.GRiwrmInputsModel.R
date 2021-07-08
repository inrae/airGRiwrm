#' @param useUpstreamQsim boolean describing if simulated (\code{TRUE}) or observed (\code{FALSE}) flows are used for calibration. Default is \code{TRUE}
#' @rdname Calibration
#' @export
Calibration.GRiwrmInputsModel <- function(InputsModel,
                                          RunOptions,
                                          InputsCrit,
                                          CalibOptions,
                                          useUpstreamQsim = TRUE,
                                          ...) {

  OutputsCalib <- list()
  class(OutputsCalib) <- append(class(OutputsCalib), "GRiwrmOutputsCalib")

  OutputsModel <- list()
  class(OutputsModel) <- append(class(OutputsModel), "GRiwrmOutputsModel")

  for(IM in InputsModel) {
    message("Calibration.GRiwrmInputsModel: Treating sub-basin ", IM$id, "...")

    if(useUpstreamQsim && any(IM$UpstreamIsRunoff)) {
      # Update InputsModel$Qupstream with simulated upstream flows
      IM <- UpdateQsimUpstream(IM, RunOptions[[IM$id]]$IndPeriod_Run, OutputsModel)
    }

    OutputsCalib[[IM$id]] <- Calibration(
      InputsModel = IM,
      RunOptions = RunOptions[[IM$id]],
      InputsCrit = InputsCrit[[IM$id]],
      CalibOptions = CalibOptions[[IM$id]],
      ...
    )

    if(useUpstreamQsim) {
      # Run the model for the sub-basin
      OutputsModel[[IM$id]] <- RunModel(
        x = IM,
        RunOptions = RunOptions[[IM$id]],
        Param = OutputsCalib[[IM$id]]$ParamFinalR
      )
    }

  }

  return(OutputsCalib)

}
