#' Creation of the InputsCrit object required to the ErrorCrit functions
#'
#' @param InputsModel InputsModel for **airGRiwrm** (See \code{[CreateInputsModel.GRiwrm]}) or **airGR** (See \code{\link[airGR]{CreateInputsModel}})
#' @param ... further arguments passed to or from other methods.
#'
#' @return Either a `InputsCrit` or a `GRiwrmInputsCrit` object
#' @export
CreateInputsCrit <- function(InputsModel, ...) {
  UseMethod("CreateInputsCrit", InputsModel)
}
