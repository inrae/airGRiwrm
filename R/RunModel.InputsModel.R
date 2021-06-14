#' Wrapper for [airGR::RunModel] for one sub-basin
#'
#' @inherit airGR::RunModel
#' @param x \[object of class \emph{InputsModel}\] see [airGR::CreateInputsModel] for details
#' @param ... Further arguments for compatibility with S3 method
#' @export
RunModel.InputsModel <- function(x, RunOptions, Param, FUN_MOD = NULL, ...) {
  if(is.null(FUN_MOD)) {
    FUN_MOD <- x$FUN_MOD
  }
  OutputsModel <- airGR::RunModel(x, RunOptions, Param, FUN_MOD)
  if (is.null(OutputsModel$Qsim_m3)) {
    # Add Qsim_m3 in m3/timestep
    OutputsModel$Qsim_m3 <- OutputsModel$Qsim * sum(x$BasinAreas) * 1e3
  }
  return(OutputsModel)
}
