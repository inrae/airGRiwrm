#' Update InputsModel$Qupstream with simulated upstream flows provided by GriwrmOutputsModels object.
#'
#' @param InputsModel \emph{GriwrmInputsModel} object. See \code{[CreateInputsModel.Griwrm]}.
#' @param OutputsModel \emph{GriwrmOutputsModel} object provided by \code{[RunModel.GriwrmInputsModel]}.
#'
#' @description This function is used by \code{\link{RunModel.GriwrmInputsModel}} and \code{\link{Calibration.GriwrmInputsModel}} in order to provide upstream simulated flows to a node.
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
