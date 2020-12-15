#' Title
#'
#' @param InputsModel object of class \emph{GriwrmInputsModel}, see \code{\link{CreateInputsModel.GRiwrm}} for details.
#' @param ... further arguments passed to \code{\link[airGR]{CreateCalibOptions}}.
#'
#' @return \emph{GriwrmCalibOptions} object.
#' @export
CreateCalibOptions.GriwrmInputsModel <- function(InputsModel, ...) {

  CalibOptions <- list()

  for(IM in InputsModel) {
    CalibOptions[[IM$id]] <- CreateCalibOptions.InputsModel(
      InputsModel = IM,
      ...
    )
  }
  return(CalibOptions)
}
