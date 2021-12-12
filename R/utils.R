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
    node <- sv$griwrm$down[sv$griwrm$id == loc]
    if(is.na(node)) {
      # Downstream node: simulated flow at last supervision time step (bug #40)
      sv$OutputsModel[[loc]]$Qsim_m3
    } else {
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
    sv$InputsModel[[node]]$Qupstream[sv$ts.index0 + sv$ts.index, ctrlr$Unames[i]] <- U
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
  if(!inherits(InputsModel, "GRiwrmInputsModel")) stop("`InputsModel` parameter must of class 'GRiwrmRunoptions' (See ?CreateRunOptions.GRiwrmInputsModel)")
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
  # Flow for each node is available in InputsModel$Qupstream except for the downstream node
  upperNodes <- griwrm$id[!is.na(griwrm$down)]
  lQsim <- lapply(
    upperNodes,
    function(x, griwrm, IndPeriod_Run) {
      node <- griwrm$down[griwrm$id == x]
      InputsModel[[node]]$Qupstream[IndPeriod_Run, x]
    },
    griwrm = griwrm, IndPeriod_Run = IndPeriod_Run
  )
  names(lQsim) <- upperNodes
  # Flow of the downstream node is only available in OutputsModel[[node]]$Qsim
  downNode <- names(InputsModel)[length(InputsModel)]
  lQsim[[downNode]] <- OutputsModel[[downNode]]$Qsim_m3

  names(lQsim) <- c(upperNodes, downNode)
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
  IniStates <- unlist(IniStates)
  IniStates[is.na(IniStates)] <- 0
  return(IniStates)
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
