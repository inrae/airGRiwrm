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
#' See [RunModel.Supervisor] and vignettes for examples of use.
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
#' @seealso [RunModel.Supervisor()], [CreateSupervisor()]
#'
CreateController <- function(supervisor, ctrl.id, Y, U, FUN){

  if (!is.character(ctrl.id)) stop("Parameter `ctrl.id` should be character")
  if (length(ctrl.id) != 1) stop("Parameter `ctrl.id` should be of length 1")
  stopifnot(is.Supervisor(supervisor))

  FUN <- match.fun(FUN)

  ctrlr <- list(
    id = ctrl.id,
    U = CreateControl(U, supervisor, TRUE),
    Unames = U,
    Y = CreateControl(Y, supervisor, FALSE),
    Ynames = Y,
    FUN = FUN
  )
  class(ctrlr) <- c("Controller", class(ctrlr))

  # Function called from Supervisor environment
  #environment(ctrlr$FUN) <- supervisor
  if (!is.null(supervisor$controllers[[ctrl.id]])) {
    warning("The existing controller '", ctrl.id, "' has been overwritten in the supervisor")
  } else {
    message("The controller '", ctrl.id, "' has been added to the supervisor")
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
CreateControl <- function(locations, sv, isU) {
  if (!is.character(locations)) {
    stop("Parameters `Y` and `U` should be character")
  }
  if (isU) {
    if (!all(locations %in% sv$griwrm4U$id)) {
      stop("Ids defined in `U` must be chosen from DirectInjection, Diversion or Reservoir nodes: ",
           paste(sv$griwrm4U$id, collapse = ", "))
    }
  } else {
    if (!all(locations %in% sv$griwrm$id)) {
      stop("Ids defined in `Y` must be chosen from available Ids in the GRiwrm object")
    }
  }
  m <- matrix(NA, ncol = length(locations), nrow = 0)
  return(m)
}
