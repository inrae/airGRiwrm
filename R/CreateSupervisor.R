#' Create a Supervisor for handling regulation in a model
#'
#' @param InputsModel `GRiwrmInputsModel` The inputs of the basin model
#'
#' @return `Supervisor` object
#' @export
#'
#' @examples
#' data(Severn)
#' nodes <- Severn$BasinsInfo[, c("gauge_id", "downstream_id", "distance_downstream", "area")]
#' nodes$distance_downstream <- nodes$distance_downstream * 1000 # Conversion km -> m
#' nodes$model <- "RunModel_GR4J"
#' griwrm <- GRiwrm(nodes,
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
CreateSupervisor <- function(InputsModel) {
  # Create Supervisor environment in the parent of GlobalEnv
  e <- new.env(parent = parent.env(globalenv()))
  class(e) <- c("Supervisor", class(e))

  # Hidden variable to detect which environment it is
  e$.isSupervisor <- "3FJKmDcJ4snDbVBg"

  # Add pointer to itself in order to assign variable from function environment
  e$supervisor <- e

  # Copy of the InputsModel
  e$InputsModel <- InputsModel
  e$OutputsModel <- list()

  # Controller list
  e$controllers <- list()
  class(e$controllers) <- c("Controllers", class(e$controllers))

  # Copy functions to be used enclosed in the Supervisor environment
  e$createController <- createController
  environment(e$createController) <- e

  # Time steps handling: these data are provided by RunModel
  # Index of the current time steps in the modelled time series between 1 and length(RunOptions$Ind_Period)
  e$ts.index <- NA
  # Index of the time step preceding RunOptions$Ind_Period
  e$ts.index0 <- NA
  # Date/Time of the current time step (For controller calculations based on date)
  e$ts.date <- NULL

  return(e)
}
