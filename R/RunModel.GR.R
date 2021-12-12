#' Run of a rainfall-runoff model on a sub-basin
#'
#' @param x \[object of class `InputsModel`\] `InputsModel` for [airGR::RunModel]
#' @param RunOptions \[object of class *RunOptions*\] see [airGR::CreateRunOptions] for details
#' @param Param [numeric] vector of model parameters (See details for SD lag model)
#' @param ... further arguments passed to or from other methods
#'
#' @inherit airGR::RunModel description details return
#' @export
#'
RunModel.GR <- function(x, RunOptions, Param, ...) {

  if (inherits(x, "SD")) {
    # Lag model take one parameter at the beginning of the vector
    iFirstParamRunOffModel <- 2
    RunOptions$FeatFUN_MOD$NbParam <- RunOptions$FeatFUN_MOD$NbParam - 1
  } else {
    # All parameters
    iFirstParamRunOffModel <- 1
  }

  FUN_MOD <- match.fun(x$FUN_MOD)
  OutputsModel <- FUN_MOD(x, RunOptions = RunOptions,
          Param = Param[iFirstParamRunOffModel:length(Param)])
  # Add Qsim_m3 in m3/timestep
  OutputsModel$Qsim_m3 <- OutputsModel$Qsim * sum(x$BasinAreas) * 1e3

  return(OutputsModel)
}
