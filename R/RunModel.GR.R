#' Run rainfall-runoff part of a sub-basin model
#'
#' @inherit airGR::RunModel
#' @param x `InputsModel` used as `InputsModel` parameter for [airGR::RunModel]
#' @param ... further arguments passed to or from other methods
#'
#' @export
#'
RunModel.GR <- function(x, RunOptions, Param, ...) {

  if (inherits(x, "SD")) {
    # Lag model take one parameter at the beginning of the vector
    iFirstParamRunOffModel <- 2
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
