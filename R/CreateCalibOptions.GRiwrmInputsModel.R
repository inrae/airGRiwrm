#' @rdname CreateCalibOptions
#' @export
CreateCalibOptions.GRiwrmInputsModel <- function(x, ...) {

  CalibOptions <- list()
  class(CalibOptions) <- c("GRiwrmCalibOptions", class(CalibOptions))

  np <- getAllNodesProperties(attr(x, "GRiwrm"))
  gaugedIds <- np$id[np$hydrology == "Gauged"]
  for(id in gaugedIds) {
    IM <- x[[id]]
    CalibOptions[[IM$id]] <- CreateCalibOptions(
      IM,
      ...
    )
  }
  return(CalibOptions)
}
