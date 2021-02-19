#' Create a controller
#'
#' @details
#' `ctrl.id` parameter is a unique id for finding the controller in the supervisor.
#' If a controller with the same id already exists, it is overwritten by this new one.
#'
#' `FUN` parameter should be a function with one [numeric] parameter.
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
#' createController(sv, "DamRelease", Y = c("54001"), U = c("54095"), FUN = logicDamRelease)
createController <- function(supervisor, ctrl.id, Y, U, FUN){

  if(!is.character(ctrl.id)) stop("Parameter `ctrl.id` should be character")

  FUN <- match.fun(FUN)

  ctrlr <- list(
    id = ctrl.id,
    U = createControl(U),
    Y = createControl(Y),
    FUN = FUN
  )
  class(ctrlr) <- c("Controller", class(ctrlr))

  # Function called from Supervisor environment
  environment(ctrlr$FUN) <- supervisor
  if(!is.null(supervisor$controllers[[ctrl.id]])) {
    warning("Controller '", ctrl.id, "' already exists in the supervisor: overwriting")
  }
  supervisor$controllers[[ctrl.id]] <- ctrlr
  message("The controller has been added to the supervisor")
  invisible(ctrlr)
}

#' Create a list of controls for command (U) and controlled variables (Y)
#'
#' @param locations vector of [character] containing the location of the variable in the model (see details)
#'
#' @return [data.frame] of two columns:
#' - 'loc' [character]: the locations of the variables
#' - 'v' [numeric]: the value of the variable for the current time step which is [NA] at its creation
#' @export
#'
#' @examples
#' # For pointing the discharge at the oulet of basins "54095" and "54002"
#' createControl(c("54095", "54002"))
createControl <- function(locations) {
  if(!is.character(locations)) {
    stop("Parameter `locations` should be character")
  }
  data.frame(loc = locations, v = rep(NA, length(locations)))
}
