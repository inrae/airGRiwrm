#' Wrapper for \code{\link[airGR]{CreateInputsModel}} for one sub-basin.
#'
#' @inherit airGR::CreateInputsModel
#' @export
#'
CreateInputsModel.default <- function(x,
                                      ...) {

  airGR::CreateInputsModel(FUN_MOD = x, ...)
}
