#' Id of sub-basins using SD model
#'
#' @param InputsModel `GRiwrmInputsModel` object
#'
#' @return [character] IDs of the sub-basins using SD model
#'
getSD_Ids <- function(InputsModel) {
  if(!inherits(InputsModel, "GRiwrmInputsModel")) {
    stop("Argument `InputsModel` should be of class GRiwrmInputsModel")
  }
  bSDs <- sapply(InputsModel, function (IM) {
    inherits(IM, "SD")
  })
  names(InputsModel)[bSDs]
}


#' Retrieve data in the model for the current time steps
#'
#' This function should be call inside a Supervisor
#'
#' @param loc location of the data
#'
#' @return [numeric] retrieved data at the location
getDataFromLocation <- function(loc) {
  if(grep("\\[[0-9]+\\]$", loc)) {
    stop("Reaching output of other controller is not implemented yet")
  } else {
    supervisor$OutputsModel[[loc]]$Qsim[supervisor$ts.index - 1]
  }
}


#' Write data to model input for the current time step
#'
#' @param control [list] A row of the `U` [data.frame] from a `Controller`
#'
#' @return [NULL]
setDataToLocation <- function(control) {
  node <- InputsModel[[control$loc]]$down
  # ! Qupstream contains warm up period and run period => the index is shifted
  supervisor$InputsModel[[node]]$Qupstream[ts.index0[node] + ts.index, control$loc] <- control$v
}


#' Do the supervision for the current time step
#'
#' @param supervisor `Supervisor` (See [CreateSupervisor])
#'
#' @return [NULL]
doSupervision <- function(controllers) {
  for(id in names(controllers)) {
    ctrlr <- controllers[[id]]
    # Read Y from locations in the model
    supervisor$controllers[[id]]$Y$v <- sapply(controllers[[id]]$Y$loc, getDataFromLocation)
    # Run logic
    supervisor$controllers[[id]]$U$v <- sapply(controllers[[id]]$Y$v, controllers[[id]]$FUN)
    # Write U to locations in the model
    sapply(controllers[[id]]$U, setDataToLocation)
  }
  return()
}
