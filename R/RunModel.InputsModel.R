#' Wrapper for [airGR::RunModel] for one sub-basin
#'
#' @details This function calls [airGR::RunModel] (See [airGR::RunModel] for further details).
#'
#' The list produced by the function (See Value section of [airGR::RunModel_GR4J]) is here completed by an item *$Qsim_m3* storing the simulated discharge series in m3/s.
#'
#' @inheritParams airGR::RunModel
#' @param x \[object of class \emph{InputsModel}\] see [airGR::CreateInputsModel] for details
#' @param ... Further arguments for compatibility with S3 methods
#'
#' @inherit airGR::RunModel return return
#'
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
  if ("WarmUpQsim" %in% RunOptions$Outputs_Sim) {
    OutputsModel$RunOptions$WarmUpQsim_m3 <- OutputsModel$RunOptions$WarmUpQsim * sum(x$BasinAreas) * 1e3
  }
  return(OutputsModel)
}
