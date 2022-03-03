#' Update of InputsModel$Qupstream with simulated upstream flows provided by a GRiwrmOutputsModels object
#'
#' @param InputsModel \emph{InputsModel} object. See [airGR::CreateInputsModel]
#' @param RunOptions \emph{RunOptions} object. See [airGR::CreateRunOptions]
#' @param OutputsModel \emph{GRiwrmOutputsModel} object provided by [RunModel.GRiwrmInputsModel].
#'
#' @description This function is used by [RunModel.GRiwrmInputsModel] and [Calibration.GRiwrmInputsModel]
#' in order to provide upstream simulated flows to a node.
#'
#' @return `InputsModel` object with updated `Qupstream` (See [airGR::CreateInputsModel] for the detail of the object).
#' @noRd
#'
UpdateQsimUpstream <- function(InputsModel, Runoptions, OutputsModel) {
  iQ <- which(InputsModel$UpstreamIsRunoff)
  for(i in iQ) {
      InputsModel$Qupstream[Runoptions$IndPeriod_Run, i] <- OutputsModel[[InputsModel$UpstreamNodes[i]]]$Qsim_m3
      if (!is.null(OutputsModel[[InputsModel$UpstreamNodes[i]]]$RunOptions$WarmUpQsim_m3)) {
        InputsModel$Qupstream[Runoptions$IndPeriod_WarmUp, i] <- OutputsModel[[InputsModel$UpstreamNodes[i]]]$RunOptions$WarmUpQsim_m3
      }
  }
  return(InputsModel)
}
