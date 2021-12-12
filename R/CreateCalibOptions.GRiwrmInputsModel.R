#' @rdname CreateCalibOptions
#' @export
CreateCalibOptions.GRiwrmInputsModel <- function(x, ...) {

  CalibOptions <- list()
  class(CalibOptions) <- c("GRiwrmCalibOptions", class(CalibOptions))

  for(IM in x) {
    CalibOptions[[IM$id]] <- CreateCalibOptions(
      IM,
      ...
    )
  }
  return(CalibOptions)
}
