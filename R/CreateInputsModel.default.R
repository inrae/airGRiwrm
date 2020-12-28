#' Wrapper for \code{\link[airGR]{CreateInputsModel}} for one sub-basin.
#'
#' @param x [function] hydrological model function (e.g. \code{\link[airGR]{RunModel_GR4J}}...)
#' @param ... arguments passed to \code{\link[airGR]{CreateInputsModel}}
#' @import airGR
#' @export
#'
CreateInputsModel.default <- function(x,
                                      ...) {

  airGR::CreateInputsModel(FUN_MOD = x, ...)
}
