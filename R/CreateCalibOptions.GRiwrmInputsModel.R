#' @rdname CreateCalibOptions
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
