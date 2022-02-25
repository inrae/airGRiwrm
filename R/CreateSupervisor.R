#' Creation of a Supervisor for handling regulation in a model
#'
#' @param InputsModel \[object of type `GRiwrmInputsModel`\] inputs of the model
#' @param TimeStep [numeric] number of time steps between each supervision
#'
#' @return A `Supervisor` object which is an [environment] containing all the necessary variables to run a supervised simulation, such as:
#' - `DatesR` [POSIXct]: vector of date from `InputsModel`
#' - `InputsModel`: a copy of `InputsModel` provided by [CreateInputsModel.GRiwrm]
#' - `griwrm`: a copy of `griwrm` provided by [CreateGRiwrm]
#' - `Controllers` [list]: list of the controllers used in the supervised simulation (See [CreateController])
#' - some internal state variables updated during simulation (`ts.index`, `ts.previous`, `ts.date`, `ts.index0`, `controller.id`)
#' @export
#'
#' @examples
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
CreateSupervisor <- function(InputsModel, TimeStep = 1L) {
  if(!inherits(InputsModel, "GRiwrmInputsModel")) {
    stop("`InputsModel` parameter must of class 'GRiwrmInputsModel' (See ?CreateInputsModel.GRiwrm)")
  }
  if(!is.integer(TimeStep)) stop("`TimeStep` parameter must be an integer")

  # Create Supervisor environment from the parent of GlobalEnv
  e <- new.env(parent = parent.env(globalenv()))
  class(e) <- c("Supervisor", class(e))

  # Hidden variable to detect which environment it is
  e$.isSupervisor <- "3FJKmDcJ4snDbVBg"

  # Add pointer to itself in order to assign variable from function environment
  e$supervisor <- e

  # Copy of InputsModel, griwrm and prepare OutputsModel
  e$DatesR <- InputsModel[[1]]$DatesR
  e$InputsModel <- InputsModel
  e$griwrm <- attr(InputsModel, "GRiwrm")
  e$OutputsModel <- list()
  e$.TimeStep <- TimeStep

  # Controller list
  e$controllers <- list()
  class(e$controllers) <- c("Controllers", class(e$controllers))

  # Copy functions to be used enclosed in the Supervisor environment
  e$CreateController <- CreateController
  environment(e$CreateController) <- e

  # Time steps handling: these data are provided by RunModel
  # Index of the current time steps in the modeled time series between 1 and length(RunOptions$Ind_Period)
  e$ts.index <- NA
  # Index of the previous time steps in the modeled time series
  e$ts.previous <- NA
  # Index of the time step preceding RunOptions$Ind_Period
  e$ts.index0 <- NA
  # Date/Time of the current time step (For controller calculations based on date)
  e$ts.date <- NULL

  # Current Controller ID (Updated in doSupervision)
  e$controller.id <- NULL

  return(e)
}
