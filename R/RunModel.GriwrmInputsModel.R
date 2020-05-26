#' Title
#'
#' @param ginet
#' @param girop
#' @param gits
#' @param IndPeriod_Run
#' @param IndPeriod_WarmUp
#'
#' @return
#' @export
#'
#' @examples
RunModel.GriwrmInputsModel <- function(InputsModel, RunOptions, girop, verbose = TRUE) {

  OutputsModels <- list()

  for(IM in InputsModel) {
    if(verbose) cat("RunModel.GriwrmInputsModel: Treating sub-basin", IM$id, "...\n")

    # Update InputsModel$QobsUpstr with simulated upstream flows
    if(length(IM$UpstreamNodes) > 0) {
      for(i in 1:length(IM$UpstreamNodes)) {
        QobsUpstr1 <- matrix(
          c(
            rep(0, length(RunOptions[[IM$id]]$IndPeriod_WarmUp)),
            OutputsModels[[IM$UpstreamNodes[i]]]$Qsim
          ), ncol = 1
        )
        if(i == 1) {
          IM$QobsUpstr <- QobsUpstr1
        } else {
          IM$QobsUpstr <- cbind(IM$QobsUpstr, QobsUpstr1)
        }
      }
    }

    # Run the model for the sub-basin
    OutputsModels[[IM$id]] <- RunModel(
      InputsModel = IM,
      RunOptions = RunOptions[[IM$id]],
      Param = unlist(girop$params[girop$id == IM$id])
    )

  }
  return(OutputsModels)
}
