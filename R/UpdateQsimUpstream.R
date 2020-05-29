#' Update InputsModel$QobsUpstr with simulated upstream flows provided by GriwrmOutputsModels object.
#'
#' @param InputsModel \emph{GriwrmInputsModel} object. See \code{[CreateInputsModel.Griwrm]}.
#' @param OutputsModels \emph{GriwrmOutputsModel} object provided by \code{[RunModel.GriwrmInputsModel]}.
#'
#' @description This function is used by \code{\link{RunModel.GriwrmInputsModel}} and \code{\link{Calibration.GriwrmInputsModel}} in order to provide upstream simulated flows to a node.
#'
#' @return InputsModel object with updated QobsUpsr
#'
UpdateQsimUpstream <- function(InputsModel, OutputsModels) {
  if(length(InputsModel$UpstreamNodes) > 0) {
    for(i in 1:length(InputsModel$UpstreamNodes)) {
      QobsUpstr1 <- matrix(
        c(
          rep(0, length(RunOptions[[InputsModel$id]]$IndPeriod_WarmUp)),
          OutputsModels[[InputsModel$UpstreamNodes[i]]]$Qsim
        ), ncol = 1
      )
      if(i == 1) {
        InputsModel$QobsUpstr <- QobsUpstr1
      } else {
        InputsModel$QobsUpstr <- cbind(InputsModel$QobsUpstr, QobsUpstr1)
      }
    }
  }
  return(InputsModel)
}
