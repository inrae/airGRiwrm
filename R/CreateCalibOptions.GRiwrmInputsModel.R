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

  if (any(np$Reservoir)) {
    message("The following nodes modelled with `RunModel_Reservoir` must have their parameters fixed: ",
            paste(paste0("\"",np$id[np$Reservoir], "\""), collapse = ", "), "\n",
            "Fix these parameters by using the command:\n",
            "`CalibOptions[[id_of_reservoir_node]]$FixedParam <- c(Vmax, celerity)`")
  }
  return(CalibOptions)
}
