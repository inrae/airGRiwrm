#' Wrapper for the airGR::CreateInputsModel function
#'
#' @param x hydrological model function (e.g. \code{\link[airGR]{RunModel_GR4J}}, \code{\link[airGR]{RunModel_CemaNeigeGR4J}})
#' @param ... further arguments passed to \code{\link[airGR]{CreateInputsModel}}.
#'
#' @return object of class \emph{InputsModel}, see \code{\link[airGR]{CreateInputsModel}} for details.
#' @import airGR
#' @export
#' @seealso The original function in airGR package: \code{\link[airGR]{CreateInputsModel}}.
#'
CreateInputsModel.default <- function(x,
                                      ...) {

  airGR::CreateInputsModel(FUN_MOD = x, ...)
}
