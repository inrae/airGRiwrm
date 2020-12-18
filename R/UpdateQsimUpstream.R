#' Update InputsModel$Qupstream with simulated upstream flows provided by GRiwrmOutputsModels object.
#'
#' @param InputsModel \emph{GRiwrmInputsModel} object. See \code{[CreateInputsModel.GRiwrm]}.
#' @param IndPeriod_Run numeric index of period to be used for the model run (-)
#' @param OutputsModel \emph{GRiwrmOutputsModel} object provided by \code{[RunModel.GRiwrmInputsModel]}.
#'
#' @description This function is used by \code{\link{RunModel.GRiwrmInputsModel}} and \code{\link{Calibration.GRiwrmInputsModel}} in order to provide upstream simulated flows to a node.
#'
#' @return InputsModel object with updated QobsUpsr
#'
UpdateQsimUpstream <- function(InputsModel, IndPeriod_Run, OutputsModel) {
  iQ <- which(!is.na(InputsModel$BasinAreas[1:length(InputsModel$LengthHydro)]))
  for(i in iQ) {
    InputsModel$Qupstream[IndPeriod_Run, i] <- OutputsModel[[InputsModel$UpstreamNodes[i]]]$Qsim
  }
  return(InputsModel)
}
