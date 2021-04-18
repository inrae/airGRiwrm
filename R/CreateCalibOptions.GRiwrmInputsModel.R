#' Title
#'
#' @param InputsModel object of class \emph{GRiwrmInputsModel}, see [CreateInputsModel.GRiwrm] for details.
#' @param ... further arguments passed to [airGR::CreateCalibOptions].
#'
#' @return \emph{GRiwrmCalibOptions} object.
#' @export
CreateCalibOptions.GRiwrmInputsModel <- function(InputsModel, ...) {

  CalibOptions <- list()

  for(IM in InputsModel) {
    CalibOptions[[IM$id]] <- CreateCalibOptions.InputsModel(
      InputsModel = IM,
      ...
    )
  }
  return(CalibOptions)
}
