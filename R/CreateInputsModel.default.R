#' Wrapper for \code{\link[airGR]{CreateInputsModel}} for one sub-basin.
#'
#' @inherit airGR::CreateInputsModel
#' @import airGR
#' @export
#'
CreateInputsModel.default <- function(x,
                                      ...) {

  airGR::CreateInputsModel(FUN_MOD = x, ...)
}
