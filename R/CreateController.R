#' Creation and adding of a controller in a supervisor
#'
#' @details
#' The `ctrl.id` is a unique id for finding the controller in the supervisor.
#' If a controller with the same id already exists, it is overwritten by this new one.
#'
#' `FUN` should be a function with one [numeric] parameter.
#' This parameter will receive the measured values of at `Y` locations as input
#' for the previous time step and returns calculated `U`. These `U` will then be applied
#' at their location for the current time step of calculation of the model.
#'
#' @param supervisor `Supervisor` object, see [CreateSupervisor]
#' @param ctrl.id [character] id of the controller (see Details)
#' @param Y [character] location of the controlled and/or measured variables in the model.
#' @param U [character] location of the command variables in the model.
#' @param FUN [function] controller logic which calculates `U` from `Y` (see Details)
#'
#' @return a `Controller` object which is a list with the following items:
#' - `id` [character]: the controller identifier
#' - `U` [matrix]: the list of controls for command variables with each column being the location of the variables and the rows being
#' the values of the variable for the current time steps (empty by default)
#' - `Unames` [character]: location of the command variables
#' - `Y` [matrix]: the lists of controls for controlled variables with each column being the location of the variables and the rows being
#' the values of the variable for the current time steps (empty by default)
#' - `Ynames` [character]: location of the controlled variables
#' - `FUN` [function]: controller logic which calculates `U` from `Y`
#' @export
#'
#' @example man-examples/RunModel.Supervisor.R
CreateController <- function(supervisor, ctrl.id, Y, U, FUN){

  if(!is.character(ctrl.id)) stop("Parameter `ctrl.id` should be character")

  FUN <- match.fun(FUN)

  ctrlr <- list(
    id = ctrl.id,
    U = CreateControl(U),
    Unames = U,
    Y = CreateControl(Y),
    Ynames = Y,
    FUN = FUN
  )
  class(ctrlr) <- c("Controller", class(ctrlr))

  # Function called from Supervisor environment
  #environment(ctrlr$FUN) <- supervisor
  if(!is.null(supervisor$controllers[[ctrl.id]])) {
    warning("The existing controller '", ctrl.id, "' has been overwritten in the supervisor")
  } else {
    message("The controller has been added to the supervisor")
  }
  supervisor$controllers[[ctrl.id]] <- ctrlr
  invisible(ctrlr)
}

#' Creation of a list of controls for command (U) and controlled variables (Y)
#'
#' @param locations [character] containing the location of the variable in the model (see details)
#'
#' @return [matrix] with each column being the location of the variables and the rows being
#' the values of the variable for the current time steps (empty by default)
#' @noRd
#'
#' @examples
#' # For pointing the discharge at the oulet of basins "54095" and "54002"
#' CreateControl(c("54095", "54002"))
CreateControl <- function(locations) {
  if(!is.character(locations)) {
    stop("Parameter `locations` should be character")
  }
  m <- matrix(NA, ncol = length(locations), nrow = 0)
  return(m)
}
