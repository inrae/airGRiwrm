#' Creation of the InputsCrit object required to the ErrorCrit functions 
#' 
#' Creation of the InputsCrit object required to the ErrorCrit functions either for \emph{InputsModel} or for \emph{GRwirmInputsModel} objects
#'
#' @param InputsModel InputsModel for **airGRiwrm** (See \code{[CreateInputsModel.GRiwrmInputsModel]}) or **airGR** (See [airGR::CreateInputsModel])
#' @param ... further arguments passed to or from other methods.
#'
#' @return Either a `InputsCrit` or a `GRiwrmInputsCrit` object
#' @export
CreateInputsCrit <- function(InputsModel, ...) {
  UseMethod("CreateInputsCrit", InputsModel)
}
