#' Retrieval of data in the model for the current time steps
#'
#' Function to be called inside a Supervisor
#'
#' @param loc location of the data
#' @param sv \[object of class `Supervisor`\] see [CreateSupervisor] for details
#'
#' @return [numeric] retrieved data at the location
#' @noRd
getDataFromLocation <- function(ctrlr, sv) {
  l <- lapply(seq(length(ctrlr$Ynodes)), function(i) {
    nodeY <- ctrlr$Ynodes[i]
    varY <- ctrlr$Yvars[i]
    if (varY != "Qupstream") {
      if (sv$nodeProperties[nodeY, "Upstream"]) {
        sv$OutputsModel[[nodeY]][[varY]][sv$ts.previous]
      } else {
        sv$OutputsModel[[nodeY]][[varY]]
      }
    } else {
      # Direct injection node => read Qupstream of downstream node
      node <- sv$griwrm$down[sv$griwrm$id == nodeY]
      sv$InputsModel[[node]]$Qupstream[sv$ts.current, nodeY]
    }
  })
  return(do.call(cbind, l))
}


#' Writing of data to model input for the current time step
#'
#' @param ctrlr \[object of type `Controller`\] see [CreateController] for details
#' @param sv \[object of type `Supervisor`\] see [CreateSupervisor] for details
#'
#' @return [NULL]
#' @noRd
setDataToLocation <- function(ctrlr, sv) {
  l <- lapply(seq(length(ctrlr$Unodes)), function(i) {
    # limit U size to the number of simulation time steps of the current supervision time step
    U <- ctrlr$U[seq.int(length(sv$ts.index)), i]
    nodeU <- ctrlr$Unodes[i]
    varU <- ctrlr$Uvars[i]

    if (varU == "Qupstream") {
      # Direct injection node => update Qusptream of downstream node
      node <- sv$griwrm4U$down[sv$griwrm4U$id == nodeU]
      # ! Qupstream contains warm up period and run period => the index is shifted
      if (!is.null(sv$InputsModel[[node]])) {
        sv$InputsModel[[node]]$Qupstream[sv$ts.current, nodeU] <- U
      }
    } else if (varU == "Qdiv") {
      # Diversion node => update Qdiv with -U
      sv$InputsModel[[nodeU]]$Qdiv[sv$ts.current] <- -U
    } else if (varU == "Qrelease") {
      sv$InputsModel[[nodeU]]$Qrelease[sv$ts.current] <- U
    }
  })
}


#' Supervision for the current time step
#'
#' @param supervisor `Supervisor` (See [CreateSupervisor])
#' @noRd
doSupervision <- function(supervisor) {
  for (id in names(supervisor$controllers)) {
    supervisor$controller.id <- id
    # Read Y from locations in the model
    supervisor$controllers[[id]]$Y <-
      getDataFromLocation(supervisor$controllers[[id]], sv = supervisor)
    # Run logic
    supervisor$controllers[[id]]$U <-
      supervisor$controllers[[id]]$FUN(supervisor$controllers[[id]]$Y)
    if (is.vector(supervisor$controllers[[id]]$U)) {
      supervisor$controllers[[id]]$U <- matrix(
        supervisor$controllers[[id]]$U,
        nrow = 1
      )
    }
    # Check U output
    if (
      ncol(supervisor$controllers[[id]]$U) !=
        length(supervisor$controllers[[id]]$Unodes) |
        (!nrow(supervisor$controllers[[id]]$U) %in%
          c(supervisor$.TimeStep, length(supervisor$ts.index)))
    ) {
      stop(
        "The logic function of the controller ",
        supervisor$controllers[[id]]$name,
        " should return a matrix of dimension ",
        supervisor$.TimeStep,
        ", ",
        length(supervisor$controllers[[id]]$Unodes)
      )
    }
    # For the last supervisor time step which can be truncated
    if (length(supervisor$ts.index) < supervisor$.TimeStep) {
      supervisor$controllers[[id]]$U <-
        supervisor$controllers[[id]]$U[seq(length(supervisor$ts.index)), ]
    }
    # Write U to locations in the model
    setDataToLocation(supervisor$controllers[[id]], sv = supervisor)
  }
}


initStoredOutputs <- function(x, outputVars) {
  QcontribDown <- do.call(
    cbind,
    lapply(x$OutputsModel, "[[", "Qsim")
  )
  so <- lapply(setNames(nm = unique(unlist(outputVars))), function(ov) {
    s <- sapply(outputVars, function(y) "Qsim_m3" %in% y)
    ids <- names(s)[s]
    if (length(ids) > 0) {
      m <- matrix(NA, nrow = nrow(QcontribDown), ncol = length(ids))
      colnames(m) <- ids
      return(m)
    }
    return(NULL)
  })
  so$QcontribDown <- QcontribDown
  return(so)
}
