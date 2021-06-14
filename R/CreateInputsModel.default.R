#' Wrapper for [airGR::CreateInputsModel] for one sub-basin
#'
#' @param x [function] hydrological model function (e.g. [airGR::RunModel_GR4J]...)
#' @param ... arguments passed to [airGR::CreateInputsModel]
#' @import airGR
#' @export
#'
CreateInputsModel.default <- function(x,
                                      ...) {

  airGR::CreateInputsModel(FUN_MOD = x, ...)
}
