#' Function to obtain the ID of sub-basins using SD model
#'
#' @param InputsModel \[`GRiwrmInputsModel` object\]
#'
#' @return [character] IDs of the sub-basins using SD model
#' @export
getSD_Ids <- function(InputsModel) {
  if (!inherits(InputsModel, "GRiwrmInputsModel")) {
    stop("Argument `InputsModel` should be of class GRiwrmInputsModel")
  }
  bSDs <- sapply(InputsModel, function (IM) {
    inherits(IM, "SD")
  })
  names(InputsModel)[bSDs]
}

#' Function to obtain the ID of sub-basins not using SD model
#'
#' @param InputsModel \[`GRiwrmInputsModel` object\]
#'
#' @return [character] IDs of the sub-basins not using the SD model
#' @export
getNoSD_Ids <- function(InputsModel) {
  if (!inherits(InputsModel, "GRiwrmInputsModel")) {
    stop("Argument `InputsModel` should be of class GRiwrmInputsModel")
  }
  bSDs <- sapply(InputsModel, function (IM) {
    !inherits(IM, "SD")
  })
  names(InputsModel)[bSDs]
}


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
    if(sv$nodeProperties[[loc]]["hydrology"] != "DirectInjection") {
      if (sv$nodeProperties[[loc]]["position"] == "Upstream") {
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
    node <- sv$griwrm$down[sv$griwrm$id == ctrlr$Unames[i]]
    # limit U size to the number of simulation time steps of the current supervision time step
    U <- ctrlr$U[seq.int(length(sv$ts.index)),i]
    # ! Qupstream contains warm up period and run period => the index is shifted
    if(!is.null(sv$InputsModel[[node]])) {
      sv$InputsModel[[node]]$Qupstream[sv$ts.index0 + sv$ts.index,
                                       ctrlr$Unames[i]] <- U
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
    # Write U to locations in the model
    setDataToLocation(supervisor$controllers[[id]], sv = supervisor)
  }
}


#' Check of the parameters of RunModel methods
#'
#' Stop the execution if an error is detected.
#'
#' @param InputsModel \[`GRiwrmInputsModel` object\] see [CreateInputsModel.GRiwrm] for details
#' @param RunOptions \[`GRiwrmRunOptions` object\] see [CreateRunOptions.GRiwrmInputsModel] for details
#' @param Param [list] of containing model parameter values of each node of the network
#' @noRd
checkRunModelParameters <- function(InputsModel, RunOptions, Param) {
  if(!inherits(InputsModel, "GRiwrmInputsModel")) stop("`InputsModel` parameter must of class 'GRiwrmInputsModel' (See ?CreateRunOptions.GRiwrmInputsModel)")
  if(!inherits(RunOptions, "GRiwrmRunOptions")) stop("Argument `RunOptions` parameter must of class 'GRiwrmRunOptions' (See ?CreateRunOptions.GRiwrmInputsModel)")
  if(!is.list(Param) || !all(names(InputsModel) %in% names(Param))) stop("Argument `Param` must be a list with names equal to nodes IDs")
}


#' Creation of a data.frame with simulated flows at each node of the GRiwrm object
#'
#' @details
#' This function can only be called inside [RunModel.GRiwrmInputsModel] or [RunModel.Supervisor]
#' because it needs a `GRiwrmInputsModel` object internally modified by these functions
#' (`Qupstream` updated with simulated flows).
#'
#' @param InputsModel \[`GRiwrmInputsModel` object\] see [CreateInputsModel.GRiwrm] for details
#' @param OutputsModel \[`GRiwrmOutputsModel` object\] see [RunModel.GRiwrmInputsModel] or [RunModel.Supervisor] for details
#' @param IndPeriod_Run [numeric] index of period to be used for the model run [-]. See [airGR::CreateRunOptions] for details
#'
#' @return a [data.frame] containing the simulated flows (in m3/time step) structured with the following columns:
#' - 'DatesR' vector of dates  of the time series
#' - one column by node with the simulated flows
#' @noRd
OutputsModelQsim <- function(InputsModel, OutputsModel, IndPeriod_Run) {
  griwrm <- attr(InputsModel, "GRiwrm")
  # Get simulated flow for each node
  # Flow for each node is available in OutputsModel except for Direct Injection
  # nodes where it is stored in InputsModel$Qupstream of the downstream node
  QsimRows <- getDiversionRows(griwrm, TRUE)
  lQsim <- lapply(
    QsimRows,
    function(i) {
      x <- griwrm[i, ]
      if (is.na(x$model)) {
        InputsModel[[x$down]]$Qupstream[IndPeriod_Run, x$id]
      } else {
        OutputsModel[[x$id]]$Qsim_m3
      }
    }
  )
  names(lQsim) <- griwrm$id[QsimRows]
  dfQsim <- cbind(data.frame(DatesR = as.POSIXct(InputsModel[[1]]$DatesR[IndPeriod_Run])),
                  do.call(cbind,lQsim) / attr(InputsModel, "TimeStep"))
  class(dfQsim) <- c("Qm3s", class(dfQsim)) # For S3 methods
  return(dfQsim)
}


#' Convert IniStates list into a vector
#'
#' @param IniStates see [CreateIniStates]
#'
#' @return A vector as in `RunOptions$IniStates`
#' @noRd
#'
serializeIniStates <- function(IniStates) {
  unlist(IniStates)
}


#' Check if a node is downstream another one
#'
#' @param InputsModel \[`GRiwrmInputsModel` object\] see [CreateInputsModel.GRiwrm] for details
#' @param current_node [character] with the id of the current node
#' @param down_node [character] with the id of the node for which we want to know if it is downstream `current_node`
#'
#' @return [logical] `TRUE` if the node with the id `down_node` is downstream the node with the id `current_node`
#' @export
#'
isNodeDownstream <- function(InputsModel, current_node, down_node) {
  current_down_node <- InputsModel[[current_node]]$down
  if (is.na(current_down_node)) return(FALSE)
  if (current_down_node == down_node) return(TRUE)
  return(isNodeDownstream(InputsModel, current_down_node, down_node))
}
