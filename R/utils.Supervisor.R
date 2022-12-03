#' Retrieval of data in the model for the current time steps
#'
#' Function to be called inside a Supervisor
#'
#' @param loc location of the data
#' @param sv \[object of class `Supervisor`\] see [CreateSupervisor] for details
#'
#' @return [numeric] retrieved data at the location
#' @noRd
getDataFromLocation <- function(loc, sv) {
  if (length(grep("\\[[0-9]+\\]$", loc)) > 0) {
    stop("Reaching output of other controller is not implemented yet")
  } else {
    if (!sv$nodeProperties[[loc]]$DirectInjection) {
      if (sv$nodeProperties[[loc]]$Upstream) {
        sv$OutputsModel[[loc]]$Qsim_m3[sv$ts.previous]
      } else {
        sv$OutputsModel[[loc]]$Qsim_m3
      }
    } else {
      node <- sv$griwrm$down[sv$griwrm$id == loc]
      sv$InputsModel[[node]]$Qupstream[sv$ts.index0 + sv$ts.previous, loc]
    }
  }
}


#' Writing of data to model input for the current time step
#'
#' @param ctrlr \[object of type `Controller`\] see [CreateController] for details
#' @param sv \[object of type `Supervisor`\] see [CreateSupervisor] for details
#'
#' @return [NULL]
#' @noRd
setDataToLocation <- function(ctrlr, sv) {
  l <- lapply(seq(length(ctrlr$Unames)), function(i) {
    # limit U size to the number of simulation time steps of the current supervision time step
    U <- ctrlr$U[seq.int(length(sv$ts.index)), i]

    locU <- ctrlr$Unames[i]

    if (sv$nodeProperties[[locU]]$DirectInjection) {
      # Direct injection node => update Qusptream of downstream node
      node <- sv$griwrm4U$down[sv$griwrm4U$id == locU]
      # ! Qupstream contains warm up period and run period => the index is shifted
      if(!is.null(sv$InputsModel[[node]])) {
        sv$InputsModel[[node]]$Qupstream[sv$ts.index0 + sv$ts.index, locU] <- U
      }
    } else if (sv$nodeProperties[[locU]]$Diversion){
      # Diversion node => update Qdiv with -U
      sv$InputsModel[[locU]]$Qdiv[sv$ts.index0 + sv$ts.index] <- -U
    } else if (sv$nodeProperties[[locU]]$Reservoir) {
      sv$InputsModel[[locU]]$Qrelease[sv$ts.index0 + sv$ts.index] <- U
    } else {
      stop("Node ", locU, " must be a Direct Injection or a Diversion node")
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
    supervisor$controllers[[id]]$Y <- do.call(
      cbind,
      lapply(supervisor$controllers[[id]]$Ynames, getDataFromLocation, sv = supervisor)
    )
    # Run logic
    supervisor$controllers[[id]]$U <-
      supervisor$controllers[[id]]$FUN(supervisor$controllers[[id]]$Y)
    if(is.vector(supervisor$controllers[[id]]$U)) {
      supervisor$controllers[[id]]$U <- matrix(supervisor$controllers[[id]]$U, nrow = 1)
    }
    # Check U output
    if (
      ncol(supervisor$controllers[[id]]$U) != length(supervisor$controllers[[id]]$Unames) |
      (!nrow(supervisor$controllers[[id]]$U) %in% c(supervisor$.TimeStep, length(supervisor$ts.index)))
    ) {
      stop("The logic function of the controller ",
           supervisor$controllers[[id]]$name,
           " should return a matrix of dimension ",
           supervisor$.TimeStep, ", ", length(supervisor$controllers[[id]]$Unames))
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


initStoredOutputs <- function(x) {
  np <- getAllNodesProperties(x$griwrm)
  so <- list()
  so$QcontribDown <- do.call(
    cbind,
    lapply(x$OutputsModel, "[[", "Qsim")
  )
  so$Qsim_m3 <- do.call(
    cbind,
    lapply(x$OutputsModel, "[[", "Qsim_m3")
  )
  if (sum(np$Diversion) > 0) {
    # Outputs of Diversion nodes
    so$Qdiv_m3 <- so$Qsim_m3[, np$id[np$Diversion], drop = FALSE] * NA
    so$Qnat <- so$Qdiv_m3
  }
  if (sum(np$Reservoir) > 0) {
    # Specific Outputs of RunModel_Reservoir
    so$Vsim <- matrix(rep(NA, sum(np$Reservoir) * nrow(so$Qsim_m3)),
                      nrow = nrow(so$Qsim_m3))
    colnames(so$Vsim) <- np$id[np$Reservoir]
    so$Qinflows_m3 <- so$Vsim
    # Add columns Qsim_m3 at reservoir (out of the scope of GR models calculated above)
    so$Qsim_m3 <- cbind(so$Qsim_m3, so$Vsim)
  }
  return(so)
}
