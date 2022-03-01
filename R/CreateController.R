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
#' @examples
#' # First create a Supervisor from a model
#' data(Severn)
#' nodes <- Severn$BasinsInfo[, c("gauge_id", "downstream_id", "distance_downstream", "area")]
#' nodes$model <- "RunModel_GR4J"
#' griwrm <- CreateGRiwrm(nodes,
#'                  list(id = "gauge_id",
#'                       down = "downstream_id",
#'                       length = "distance_downstream"))
#' BasinsObs <- Severn$BasinsObs
#' DatesR <- BasinsObs[[1]]$DatesR
#' PrecipTot <- cbind(sapply(BasinsObs, function(x) {x$precipitation}))
#' PotEvapTot <- cbind(sapply(BasinsObs, function(x) {x$peti}))
#' Qobs <- cbind(sapply(BasinsObs, function(x) {x$discharge_spec}))
#' Precip <- ConvertMeteoSD(griwrm, PrecipTot)
#' PotEvap <- ConvertMeteoSD(griwrm, PotEvapTot)
#' InputsModel <- CreateInputsModel(griwrm, DatesR, Precip, PotEvap, Qobs)
#' sv <- CreateSupervisor(InputsModel)
#'
#' # A controller which usually releases 0.1 m3/s and provides
#' # extra release if the downstream flow is below 0.5 m3/s
#' logicDamRelease <- function(Y) max(0.5 - Y[1], 0.1)
#' CreateController(sv, "DamRelease", Y = c("54001"), U = c("54095"), FUN = logicDamRelease)
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
