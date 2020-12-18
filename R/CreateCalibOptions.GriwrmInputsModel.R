#' Title
#'
#' @param InputsModel object of class \emph{GRiwrmInputsModel}, see \code{\link{CreateInputsModel.GRiwrm}} for details.
#' @param ... further arguments passed to \code{\link[airGR]{CreateCalibOptions}}.
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
