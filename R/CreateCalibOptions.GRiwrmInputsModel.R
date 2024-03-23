#' @rdname CreateCalibOptions
#' @export
CreateCalibOptions.GRiwrmInputsModel <- function(x, FixedParam = NULL, ...) {
  dots <- list(...)
  if ("IsHyst" %in% names(dots)) {
    warning("The parameter `IsHyst` will be ignored. It should be defined before with `CreateInputsModel`")
  }
  np <- getAllNodesProperties(attr(x, "GRiwrm"))
  gaugedIds <- np$id[np$calibration == "Gauged"]

  if (!is.null(FixedParam)) {
    if (!(is.list(FixedParam) || is.numeric(FixedParam))) {
      stop("Argument `FixedParam` should be of type numeric or list")
    }
    if (!is.list(FixedParam)) {
      FixedParam <- list("*" = FixedParam)
    }
    if (!all(names(FixedParam) %in% c(gaugedIds, "*"))) {
      stop("Each item of the list `FixedParam` should correspond to a gauged node ids:\n",
           "Unknown id(s): ", paste(names(FixedParam)[which(!names(FixedParam) %in% gaugedIds)], sep = ", "))
    }
    if (!all(sapply(FixedParam, is.numeric))) {
      stop("All items of the list `FixedParam` should be numeric")
    }
    if ("*" %in% names(FixedParam)) {
      aFP <- FixedParam[["*"]]
      FixedParam <- lapply(
        setNames(nm = gaugedIds),
        function(id) {
          if (is.null(FixedParam[[id]])) {
            FP <- aFP[x[[id]]$model$indexParamUngauged]
            if (all(is.na(FP))) FP <- NULL
            return(FP)
          } else {
            return(FixedParam[[id]])
          }
        }
      )
    }
  }

  CalibOptions <- list()
  class(CalibOptions) <- c("GRiwrmCalibOptions", class(CalibOptions))

  for(id in gaugedIds) {
    IM <- x[[id]]
    if (!is.null(FixedParam)) {
      FP <- FixedParam[[id]]
    } else {
      FP <- NULL
    }
    CalibOptions[[IM$id]] <- CreateCalibOptions(
      IM,
      FixedParam = FP,
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
