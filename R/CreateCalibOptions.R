#' CreateCalibOptions both available for \emph{InputsModel} and \emph{GRwirmInputsModel} objects
#'
#' @param InputsModel object of class \emph{InputsModel} or \emph{GRwirmInputsModel}. See \code{\link{CreateInputsModel}} for details
#' @param ... further arguments passed to or from other methods
#'
#' @return Either a `CalibOptions` or a `GRiwrmCalibOptions` object
#' @export
CreateCalibOptions <- function(InputsModel, ...) {
  UseMethod("CreateCalibOptions", InputsModel)
}
