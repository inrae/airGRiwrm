#' @rdname CreateCalibOptions
#' @export
CreateCalibOptions.GRiwrmInputsModel <- function(InputsModel, ...) {

  CalibOptions <- list()
  class(CalibOptions) <- c("GRiwrmCalibOptions", class(CalibOptions))

  for(IM in InputsModel) {
    CalibOptions[[IM$id]] <- CreateCalibOptions.InputsModel(
      InputsModel = IM,
      ...
    )
  }
  return(CalibOptions)
}
