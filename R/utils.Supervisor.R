#' Retrieval of data in the model for the current time steps
#'
#' Function to be called inside a Supervisor
#'
#' @param loc location of the data
#' @param sv \[object of class `Supervisor`\] see [CreateSupervisor] for details
#' @param OutputsModel [list] model outputs
#' @param InputsModel [list] model inputs
#' @param idx.output_previous [integer] previous time step indexes for OutputsModel
#' @param idx.input_previous [integer] previous time step indexes for InputsModel
#' @param inDoSupervision [logical] whether in supervision
#'
#' @return [numeric] retrieved data at the location
#' @noRd
getDataFromLocation <- function(
  ctrlr,
  sv,
  OutputsModel = sv$OutputsModel,
  InputsModel = sv$InputsModel,
  idx.output_previous = sv$idx.output_previous,
  idx.input_previous = sv$idx.input_previous,
  inDoSupervision = TRUE
) {
  if (is.null(ctrlr$Ynodes)) {
    return(NULL)
  }
  l <- lapply(seq(length(ctrlr$Ynodes)), function(i) {
    nodeY <- ctrlr$Ynodes[i]
    varY <- ctrlr$Yvars[i]
    if (varY != "Qupstream") {
      if (!inDoSupervision || sv$nodeProperties[nodeY, "Upstream"]) {
        OutputsModel[[nodeY]][[varY]][idx.output_previous]
      } else {
        OutputsModel[[nodeY]][[varY]]
      }
    } else {
      # Direct injection node => read Qupstream of downstream node
      node <- sv$griwrm$down[sv$griwrm$id == nodeY]
      InputsModel[[node]]$Qupstream[idx.input_previous, nodeY]
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
    U <- ctrlr$U[seq.int(length(sv$idx.output)), i]
    nodeU <- ctrlr$Unodes[i]
    varU <- ctrlr$Uvars[i]

    if (varU == "Qupstream") {
      # Direct injection node => update Qusptream of downstream node
      node <- sv$griwrm4U$down[sv$griwrm4U$id == nodeU]
      # ! Qupstream contains warm up period and run period => the index is shifted
      if (!is.null(sv$InputsModel[[node]])) {
        sv$InputsModel[[node]]$Qupstream[sv$idx.input, nodeU] <- U
      }
    } else if (varU == "Qdiv") {
      # Diversion node => update Qdiv with -U
      sv$InputsModel[[nodeU]]$Qdiv[sv$idx.input] <- -U
    } else if (varU == "Qrelease") {
      sv$InputsModel[[nodeU]]$Qrelease[sv$idx.input] <- U
    }
  })
}


#' Supervision for the current time step
#'
#' @param supervisor `Supervisor` (See [CreateSupervisor])
#' @noRd
doSupervision <- function(supervisor, Yinit = NULL) {
  for (id in names(supervisor$controllers)) {
    supervisor$controller.id <- id
    # Read Y from locations in the model
    if (!is.null(Yinit)) {
      supervisor$controllers[[id]]$Y <- Yinit[[id]]
    } else {
      supervisor$controllers[[id]]$Y <-
        getDataFromLocation(supervisor$controllers[[id]], sv = supervisor)
    }
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
          c(supervisor$.TimeStep, length(supervisor$idx.output)))
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
    if (length(supervisor$idx.output) < supervisor$.TimeStep) {
      supervisor$controllers[[id]]$U <-
        supervisor$controllers[[id]]$U[
          seq(length(supervisor$idx.output)),
          ,
          drop = FALSE
        ]
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

checkYinit <- function(sv, Yinit) {
  lapply(names(sv$controllers), function(id) {
    if (!id %in% names(Yinit)) {
      stop("Missing Yinit for controller ", id)
    }
    if (!is.matrix(Yinit[[id]])) {
      stop("Yinit for controller ", id, " should be a matrix")
    }
    if (
      ncol(Yinit[[id]]) != length(sv$controllers[[id]]$Ynodes) ||
        (nrow(Yinit[[id]]) != sv$.TimeStep)
    ) {
      stop(
        "Yinit for controller ",
        id,
        " should be a matrix of dimension ",
        sv$.TimeStep,
        ", ",
        length(sv$controllers[[id]]$Ynodes)
      )
    }
  })
}

getYinit <- function(sv, InputsModel, OutputsModel) {
  lapply(setNames(sv$controllers, nm = names(sv$controllers)), function(ctrlr) {
    getDataFromLocation(
      ctrlr,
      InputsModel = InputsModel,
      OutputsModel = OutputsModel,
      idx.output_previous = sv$idx.output_previous,
      inDoSupervision = FALSE
    )
  })
}
