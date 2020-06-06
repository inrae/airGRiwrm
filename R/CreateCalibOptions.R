#' CreateCalibOptions both available for \emph{InputsModel} and \emph{GrwirmInputsModel} objects
#'
#' @param InputsModel object of class \emph{InputsModel}, see \code{\link[airGR]{CreateInputsModel}} for details.
#' @param ... further arguments passed to or from other methods.
#'
#' @return
#' @export
CreateCalibOptions <- function(InputsModel, ...) {
  UseMethod("CreateCalibOptions", InputsModel)
}
