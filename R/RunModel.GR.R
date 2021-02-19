#' Run rainfall-runoff part of a sub-basin model
#'
#' @inherit airGR::RunModel
#' @param x `InputsModel` used as `InputsModel` parameter for [airGR::RunModel]
#' @param ... further arguments passed to or from other methods
#'
#' @export
#'
RunModel.GR <- function(x, RunOptions, Param, ...) {
  message("RunModel.GR")

  if (inherits(x, "SD")) {
    # Lag model take one parameter at the beginning of the vector
    iFirstParamRunOffModel <- 2
  } else {
    # All parameters
    iFirstParamRunOffModel <- 1
  }

  FUN_MOD <- match.fun(x$FUN_MOD)
  FUN_MOD(x, RunOptions = RunOptions,
          Param = Param[iFirstParamRunOffModel:length(Param)])
}
