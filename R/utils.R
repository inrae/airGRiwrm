#' Id of sub-basins using SD model
#'
#' @param InputsModel `GRiwrmInputsModel` object
#'
#' @return [character] IDs of the sub-basins using SD model
#'
getSD_Ids <- function(InputsModel) {
  if (!inherits(InputsModel, "GRiwrmInputsModel")) {
    stop("Argument `InputsModel` should be of class GRiwrmInputsModel")
  }
  bSDs <- sapply(InputsModel, function (IM) {
    inherits(IM, "SD")
  })
  names(InputsModel)[bSDs]
}

#' Id of sub-basins not using SD model
#'
#' @param InputsModel `GRiwrmInputsModel` object
#'
#' @return [character] IDs of the sub-basins not using SD model
#'
getNoSD_Ids <- function(InputsModel) {
  if (!inherits(InputsModel, "GRiwrmInputsModel")) {
    stop("Argument `InputsModel` should be of class GRiwrmInputsModel")
  }
  bSDs <- sapply(InputsModel, function (IM) {
    !inherits(IM, "SD")
  })
  names(InputsModel)[bSDs]
}


#' Retrieve data in the model for the current time steps
#'
#' This function should be call inside a Supervisor
#'
#' @param loc location of the data
#' @param sv a `Supervisor` (See [CreateSupervisor])
#'
#' @return [numeric] retrieved data at the location
getDataFromLocation <- function(loc, sv) {
  if (length(grep("\\[[0-9]+\\]$", loc)) > 0) {
    stop("Reaching output of other controller is not implemented yet")
  } else {
    node <- sv$griwrm$down[sv$griwrm$id == loc]
    sv$InputsModel[[node]]$Qupstream[sv$ts.index0 + sv$ts.previous, loc]
  }
}


#' Write data to model input for the current time step
#'
#' @param ctrlr a `Controller` object (See [CreateController])
#' @param sv `Supervisor` (See [CreateSupervisor])
#'
#' @return [NULL]
setDataToLocation <- function(ctrlr, sv) {
  l <- lapply(seq(length(ctrlr$Unames)), function(i) {
    node <- sv$griwrm$down[sv$griwrm$id == ctrlr$Unames[i]]
    # limit U size to the number of simulation time steps of the current supervision time step
    U <- ctrlr$U[seq.int(length(sv$ts.index)),i]
    # ! Qupstream contains warm up period and run period => the index is shifted
    sv$InputsModel[[node]]$Qupstream[sv$ts.index0 + sv$ts.index, ctrlr$Unames[i]] <- U
  })
}


#' Do the supervision for the current time step
#'
#' @param supervisor `Supervisor` (See [CreateSupervisor])
#'
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
    # Write U to locations in the model
    setDataToLocation(supervisor$controllers[[id]], sv = supervisor)
  }
}


#' Check the parameters of RunModel methods
#'
#' Stop the execution if an error is detected.
#'
#' @param InputsModel a `GRiwrmInputsModel` object (See [CreateInputsModel.GRiwrm])
#' @param RunOptions a `GRiwrmRunOptions` object (See [CreateRunOptions.GRiwrmInputsModel])
#' @param Param a [list] of [numeric] containing model parameters of each node of the network
#'
checkRunModelParameters <- function(InputsModel, RunOptions, Param) {
  if(!inherits(InputsModel, "GRiwrmInputsModel")) stop("`InputsModel` parameter must of class 'GRiwrmRunoptions' (See ?CreateRunOptions.GRiwrmInputsModel)")
  if(!inherits(RunOptions, "GRiwrmRunOptions")) stop("Argument `RunOptions` parameter must of class 'GRiwrmRunOptions' (See ?CreateRunOptions.GRiwrmInputsModel)")
  if(!is.list(Param) || !all(names(InputsModel) %in% names(Param))) stop("Argument `Param` must be a list with names equal to nodes IDs")
}


#' Create a data.frame with simulated flows at each nodes of the [GRiwrm] object
#'
#' @details
#' This function can only be called inside [RunModel.GRiwrmInputsModel] or [RunModel.Supervisor]
#' because it needs a `GRiwrmInputsModel` object internally modified by these functions
#' (`Qupstream` updated with simulated flows).
#'
#' @param InputsModel a `GRiwrmInputsModel` object created by [CreateInputsModel.GRiwrm]
#' @param OutputsModel a `GRiwrmOutputsModel` object created by [RunModel.GRiwrmInputsModel] or [RunModel.Supervisor]
#' @param IndPeriod_Run an [integer] vector (See [airGR::CreateRunOptions])
#'
#' @return a [data.frame] containing the simulated flows (in m3/time step) structured with the following columns:
#' - 'DatesR' containing the timestamps of the time series
#' - one column by node with the simulated flows
#'
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
  lQsim[[downNode]] <- OutputsModel[[downNode]]$Qsim

  # Conversion to m3/s
  lQsim <- lapply(
    names(lQsim),
    function(x) {
      i <- which(griwrm$id == x)
      if(is.na(griwrm$area[i])) { # m3/time step => m3/s
        return(lQsim[[x]] / attr(InputsModel, "TimeStep"))
      } else { # mm/time step => m3/s
        return(lQsim[[x]] * griwrm$area[i] * 1E3 / attr(InputsModel, "TimeStep"))
      }
    }
  )
  names(lQsim) <- c(upperNodes, downNode)
  dfQsim <- cbind(data.frame(DatesR = as.POSIXct(InputsModel[[1]]$DatesR[IndPeriod_Run])),
                  do.call(cbind,lQsim))
  class(dfQsim) <- c("Qm3s", class(dfQsim)) # For S3 methods
  return(dfQsim)
}
