#' @rdname CreateCalibOptions
#' @export
  dots <- list(...)
  if ("IsHyst" %in% names(dots)) {
    warning("The parameter `IsHyst` will be ignored. It should be defined before with `CreateInputsModel`")
  }

  CalibOptions <- list()
  class(CalibOptions) <- c("GRiwrmCalibOptions", class(CalibOptions))

  np <- getAllNodesProperties(attr(x, "GRiwrm"))
  gaugedIds <- np$id[np$calibration == "Gauged"]
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
