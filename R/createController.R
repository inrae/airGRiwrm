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
#' @param Y [character] location of the controlled and/or measured variables in the model. See [createControl]
#' @param U [character] location of the command variables in the model. See [createControl]
#' @param FUN [function] controller logic which calculates `U` from `Y` (see Details)
#'
#' @return `Controller`
#' @export
#'
#' @examples
#' # First create a Supervisor from a model
#' example("CreateSupervisor")
#' # A controller which usually releases 0.1 m3/s and provides
#' # extra release if the downstream flow is below 0.5 m3/s
#' logicDamRelease <- function(Y) max(0.5 - Y[1], 0.1)
#' CreateController(sv, "DamRelease", Y = c("54001"), U = c("54095"), FUN = logicDamRelease)
CreateController <- function(supervisor, ctrl.id, Y, U, FUN){

  if(!is.character(ctrl.id)) stop("Parameter `ctrl.id` should be character")

  FUN <- match.fun(FUN)

  ctrlr <- list(
    id = ctrl.id,
    U = createControl(U),
    Unames = U,
    Y = createControl(Y),
    Ynames = Y,
    FUN = FUN
  )
  class(ctrlr) <- c("Controller", class(ctrlr))

  # Function called from Supervisor environment
  #environment(ctrlr$FUN) <- supervisor
  if(!is.null(supervisor$controllers[[ctrl.id]])) {
    warning("Controller '", ctrl.id, "' already exists in the supervisor: overwriting")
  }
  supervisor$controllers[[ctrl.id]] <- ctrlr
  message("The controller has been added to the supervisor")
  invisible(ctrlr)
}

#' Creation of a list of controls for command (U) and controlled variables (Y)
#'
#' @param locations [character] containing the location of the variable in the model (see details)
#'
#' @return [matrix] with each column being the location of the variables and the rows being
#' the values of the variable for the current time steps (empty by default)
#' @export
#'
#' @examples
#' # For pointing the discharge at the oulet of basins "54095" and "54002"
#' createControl(c("54095", "54002"))
createControl <- function(locations) {
  if(!is.character(locations)) {
    stop("Parameter `locations` should be character")
  }
  m <- matrix(NA, ncol = length(locations), nrow = 0)
  return(m)
}
