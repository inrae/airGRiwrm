#' Run rainfall-runoff part of a sub-basin model
#'
#' @inherit airGR::RunModel
#' @param ... further arguments passed to or from other methods
#'
#' @export
#'
RunModel.GR <- function(InputsModel, RunOptions, Param, ...) {

  if (inherits(InputsModel, "SD")) {
    # Lag model take one parameter at the beginning of the vector
    iFirstParamRunOffModel <- 2
  } else {
    # All parameters
    iFirstParamRunOffModel <- 1
  }

  FUN_MOD <- match.fun(InputsModel$FUN_MOD)
  FUN_MOD(InputsModel = InputsModel, RunOptions = RunOptions,
          Param = Param[iFirstParamRunOffModel:length(Param)])
}
