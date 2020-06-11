#' Wrapper for \code{\link[airGR]{RunModel}} for one sub-basin.
#'
#' @inherit airGR::RunModel
#' @export
RunModel.InputsModel <- function(InputsModel, RunOptions, Param, FUN_MOD = NULL, ...) {
  if(is.null(FUN_MOD)) {
    FUN_MOD <- InputsModel$FUN_MOD
  }
  airGR::RunModel(InputsModel, RunOptions, Param, FUN_MOD)
}
