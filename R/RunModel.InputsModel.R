#' Wrapper for \code{\link[airGR]{RunModel}} for one sub-basin.
#'
#' @inherit airGR::RunModel
#' @param x `InputsModel` used as `InputsModel` parameter for [airGR::RunModel]
#' @param ... Further arguments for compatibility with S3 method
#' @export
RunModel.InputsModel <- function(x, RunOptions, Param, FUN_MOD = NULL, ...) {
  if(is.null(FUN_MOD)) {
    FUN_MOD <- x$FUN_MOD
  }
  airGR::RunModel(x, RunOptions, Param, FUN_MOD)
}
