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
    sv$InputsModel[[sv$griwrm$down[sv$griwrm$id == loc]]]$Qupstream[sv$ts.index0 + sv$ts.index - 1, loc]
  }
}


#' Write data to model input for the current time step
#'
#' @param control [vector] A row of the `U` [data.frame] from a `Controller`
#' @param sv `Supervisor` (See [CreateSupervisor])
#'
#' @return [NULL]
setDataToLocation <- function(control, sv) {
  node <- sv$griwrm$down[sv$griwrm$id == control[1]]
  # ! Qupstream contains warm up period and run period => the index is shifted
  sv$InputsModel[[node]]$Qupstream[sv$ts.index0 + sv$ts.index, control[1]] <-
    as.numeric(control[2])
}


#' Do the supervision for the current time step
#'
#' @param supervisor `Supervisor` (See [CreateSupervisor])
#'
#' @return [NULL]
doSupervision <- function(supervisor) {
  for (id in names(supervisor$controllers)) {
    # Read Y from locations in the model
    supervisor$controllers[[id]]$Y$v <-
      sapply(supervisor$controllers[[id]]$Y$loc, getDataFromLocation, sv = supervisor)
    # Run logic
    supervisor$controllers[[id]]$U$v <-
      sapply(supervisor$controllers[[id]]$Y$v, supervisor$controllers[[id]]$FUN)
    # Write U to locations in the model
    apply(supervisor$controllers[[id]]$U, 1, setDataToLocation, sv = supervisor)
  }
  return()
}

#' Check the parameters of RunModel methods
#'
#' Stop the execution if an error is detected.
#'
#' @param InputsModel a `GRiwrmInputsModel` object (See [CreateInputsModel.GRiwrm])
#' @param RunOptions a `GRiwrmRunOptions` object (See [CreateRunOptions.GRiwrmInputsModel])
#' @param Param a [list] of [numeric] containing model parameters of each node of the network
#'
#' @return [NULL]
#'
checkRunModelParameters <- function(InputsModel, RunOptions, Param) {
  if(!inherits(InputsModel, "GRiwrmInputsModel")) stop("`InputsModel` parameter must of class 'GRiwrmRunoptions' (See ?CreateRunOptions.GRiwrmInputsModel)")
  if(!inherits(RunOptions, "GRiwrmRunOptions")) stop("Argument `RunOptions` parameter must of class 'GRiwrmRunOptions' (See ?CreateRunOptions.GRiwrmInputsModel)")
  if(!is.list(Param) || !all(names(InputsModel) %in% names(Param))) stop("Argument `Param` must be a list with names equal to nodes IDs")
  return()
}
