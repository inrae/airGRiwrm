#' Title
#'
#' @param InputsModel InputsModel for GR-IWRM (See \code{[CreateInputsModel.Griwrm]}) or AirGR (See \code{\link[airGR]{CreateInputsModel}})
#' @param ... further arguments passed to or from other methods.
#'
#' @return
#' @export
CreateInputsCrit <- function(InputsModel, ...) {
  UseMethod("CreateInputsCrit", InputsModel)
}
