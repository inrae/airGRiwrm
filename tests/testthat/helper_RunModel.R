#' Prepare useful variables for GRiwrm tests
#'
#' @return [environment] with the variables (See examples section)
#' @noRd
#'
#' @examples
#' # data set up
#' e <- setupRunModel()
# variables are copied from environment 'e' to the current environment
# https://stackoverflow.com/questions/9965577/r-copy-move-one-environment-to-another
#' for(x in ls(e)) assign(x, get(x, e))
#'
setupRunModel <- function() {
  data(Severn)

  # Format observation
  BasinsObs <- Severn$BasinsObs
  DatesR <- BasinsObs[[1]]$DatesR
  PrecipTot <- cbind(sapply(BasinsObs, function(x) {x$precipitation}))
  PotEvapTot <- cbind(sapply(BasinsObs, function(x) {x$peti}))
  Qobs <- cbind(sapply(BasinsObs, function(x) {x$discharge_spec}))

  # Set network
  nodes <- Severn$BasinsInfo[, c("gauge_id", "downstream_id", "distance_downstream", "area")]
  nodes$distance_downstream <- nodes$distance_downstream
  nodes$model <- "RunModel_GR4J"
  griwrm <- CreateGRiwrm(nodes, list(id = "gauge_id", down = "downstream_id", length = "distance_downstream"))

  # Convert meteo data to SD (remove upstream areas)
  Precip <- ConvertMeteoSD(griwrm, PrecipTot)
  PotEvap <- ConvertMeteoSD(griwrm, PotEvapTot)

  # Calibration parameters
  ParamMichel <- list(
    `54057` = c(0.781180650559296, 74.4247133147015, -1.26590474908235, 0.96012365697022, 2.51101785373787),
    `54032` = c(0.992743594649893, 1327.19309205366, -0.505902143697436, 7.91553615210291, 2.03604525989572),
    `54001` = c(1.03, 24.7790862245877, -1.90430150145153, 21.7584023961971, 1.37837837837838),
    `54095` = c(256.844150254651, 0.0650458497009288, 57.523675209819, 2.71809513102128),
    `54002` = c(419.437754485522, 0.12473266292168, 13.0379482833606, 2.12230907892238),
    `54029` = c(219.203385553954, 0.389211590110934, 48.4242150713452, 2.00300300300301)
  )

  # set up inputs
  InputsModel <- suppressWarnings(CreateInputsModel(griwrm, DatesR, Precip, PotEvap, Qobs))

  # RunOptions
  nTS <- 365
  IndPeriod_Run <- seq(
    length(InputsModel[[1]]$DatesR) - nTS + 1,
    length(InputsModel[[1]]$DatesR)
  )
  IndPeriod_WarmUp = seq(IndPeriod_Run[1]-365,IndPeriod_Run[1]-1)
  RunOptions <- CreateRunOptions(
    InputsModel,
    IndPeriod_WarmUp = IndPeriod_WarmUp,
    IndPeriod_Run = IndPeriod_Run
  )

  # RunModel.GRiwrmInputsModel
  OM_GriwrmInputs <- RunModel(
    InputsModel,
    RunOptions = RunOptions,
    Param = ParamMichel
  )
  return(environment())
}
