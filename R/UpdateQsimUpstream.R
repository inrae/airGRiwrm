#' Update of InputsModel$Qupstream with simulated upstream flows provided by a GRiwrmOutputsModels object
#'
#' @param InputsModel \[\emph{InputsModel} object\] see [airGR::CreateInputsModel] for details
#' @param IndPeriod_Run [numeric] index of period to be used for the model run (-)
#' @param OutputsModel \[\emph{GRiwrmOutputsModel} object\] output from [RunModel.GRiwrmInputsModel]
#'
#' @description This function is used by [RunModel.GRiwrmInputsModel] and [Calibration.GRiwrmInputsModel] in order to provide upstream simulated flows to a node
#'
#' @return `InputsModel` object with updated `Qupstream`
#' @noRd
UpdateQsimUpstream <- function(InputsModel, IndPeriod_Run, OutputsModel) {
  iQ <- which(InputsModel$UpstreamIsRunoff)
  for(i in iQ) {
      InputsModel$Qupstream[IndPeriod_Run, i] <- OutputsModel[[InputsModel$UpstreamNodes[i]]]$Qsim_m3
  }
  return(InputsModel)
}
