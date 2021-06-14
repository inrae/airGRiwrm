#' Creation of the CalibOptions object either for \emph{InputsModel} or for \emph{GRwirmInputsModel} objects
#'
#' @param InputsModel object of class \emph{InputsModel} or \emph{GRwirmInputsModel}. See [CreateInputsModel] for details
#' @param ... further arguments passed to or from other methods
#'
#' @return [list] Either a `CalibOptions` or a `GRiwrmCalibOptions` object
#' @export
CreateCalibOptions <- function(InputsModel, ...) {
  UseMethod("CreateCalibOptions", InputsModel)
}
