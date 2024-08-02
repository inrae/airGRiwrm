updateQinfQrelease <- function(g, Qinf, Qrelease) {
  reservoirIds <- g$id[!is.na(g$model) & g$model == "RunModel_Reservoir"]
  # Fill Qrelease with Qinf
  warn_ids <- NULL
  for (id in reservoirIds) {
    if (!id %in% colnames(Qrelease)) {
      if (id %in% colnames(Qinf)) {
        if (!any(g$id == id & (!is.na(g$model) & g$model == "Diversion"))) {
          if (is.null(Qrelease)) {
            Qrelease = Qinf[, id, drop = FALSE]
          } else {
            Qrelease = cbind(Qrelease, Qinf[, id, drop = FALSE])
          }
          Qinf <- Qinf[, colnames(Qinf) != id, drop = FALSE]
          warn_ids = c(warn_ids, id)
        }
      }
    }
  }
  if (!is.null(warn_ids)) {
    warning("Use of the `Qinf` parameter for reservoir releases is deprecated, please use `Qrelease` instead.\n",
            "Processing `Qrelease <- cbind(Qrelease, Qinf[, c(\"", paste(warn_ids, collapse = "\", `"), "\"])`...")
  }
  return(list(Qinf = Qinf, Qrelease = Qrelease))
}

checkQinfQrelease <- function(g, varname, Q) {
  if (varname == "Qinf") {
    directFlowIds <- g$id[is.na(g$model) | g$model == "Diversion"]
  } else {
    directFlowIds <- g$id[!is.na(g$model) & g$model == "RunModel_Reservoir"]
  }
  if (length(directFlowIds) > 0) {
    err <- FALSE
    if (is.null(Q)) {
      err <- TRUE
    } else {
      Q <- as.matrix(Q)
      if (is.null(colnames(Q))) {
        err <- TRUE
      } else if (!all(directFlowIds %in% colnames(Q))) {
        err <- TRUE
      }
    }
    if (err) stop(sprintf("'%s' column names must at least contain %s", varname, paste(directFlowIds, collapse = ", ")))
  }
  if (!all(colnames(Q) %in% directFlowIds)) {
    warning(
      sprintf("The following columns in '%s' are ignored since they don't match with ", varname),
      ifelse(varname == "Qinf",
             c("Direction Injection (model=`NA`), ",
               "or Diversion nodes (model=\"Diversion\"): "),
             "Reservoir nodes (model=\"RunModelReservoir\"): "),
      paste(setdiff(colnames(Q), directFlowIds), collapse = ", ")
    )
    Q <- Q[, directFlowIds]
  }
  return(Q)
}
