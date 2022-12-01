#' Run with a reservoir model
#'
#' The reservoir model is a model combining a lag model and the calculation of
#' the water storage time series according to the released flow time series
#' from the `Qobs` parameter of [CreateInputsModel.GRiwrm].
#'
#' @details
#' The simulated flow corresponds to the released flow except when the reservoir
#' is empty (release flow is limited) or full (release flow is completed by inflows
#' excess).
#'
#' By default, the initial reservoir volume at the beginning of the warm-up period
#' is equal to the half of the maximum reservoir capacity.
#'
#' @inheritParams airGR::RunModel
#' @param Param [numeric] vector of length 2 containing (1) the capacity of the
#' reservoir and (2) the celerity in m/s of the upstream inflows.
#'
#' @return An OutputsModel object like the one return by [airGR::RunModel] but
#' completed with an item `Vsim` representing the water volume time series in m3.
#' @export
#'
#' @example man-examples/RunModel_Reservoir.R
#'
RunModel_Reservoir <- function(InputsModel, RunOptions, Param) {

  # Input checks
  stopifnot(InputsModel$isReservoir,
            is.numeric(Param),
            length(Param) == 2)

  # Model parameter
  Vmax <- Param[1]
  celerity <- Param[2]

  # Compute inflows with RunModel_Lag
  OutputsModel <- RunModel(InputsModel, RunOptions, celerity, FUN_MOD = "RunModel_Lag")
  names(OutputsModel)[names(OutputsModel) == "Qsim_m3"] <- "Qinflows_m3"
  Qinflows_m3 <- c(OutputsModel$RunOptions$WarmUpQsim_m3,
                   OutputsModel$Qinflows_m3)

  # Reservoir initial conditions
  V0 <- RunOptions$IniStates["ReservoirCapacity"]
  if (is.na(V0)) {
    V0 <- Vmax / 2
  }

  # Initiation of output variables
  IndPerTot   <- c(RunOptions$IndPeriod_WarmUp, RunOptions$IndPeriod_Run)
  iPerTot <- seq(length(IndPerTot))
  Vsim <- rep(0, length(IndPerTot))
  Qsim_m3 <- Vsim

  # Time series volume and release calculation
  for(i in iPerTot) {
    Vsim[i] <- V0 + Qinflows_m3[i]
    Qsim_m3[i] <- min(Vsim[i], InputsModel$Qrelease[IndPerTot[i]])
    Vsim[i] <- Vsim[i] - Qsim_m3[i]
    if (Vsim[i] > Vmax) {
      Qsim_m3[i] <- Qsim_m3[i] + Vsim[i] - Vmax
      Vsim[i] <- Vmax
    }
    V0 <- Vsim[i]
  }

  # Format OutputsModel
  iWarmUp <- seq(length(RunOptions$IndPeriod_WarmUp))
  OutputsModel$RunOptions$WarmUpQsim_m3 <- Qsim_m3[iWarmUp]
  OutputsModel$RunOptions$WarmUpVsim <- Vsim[iWarmUp]
  iRun <- length(RunOptions$IndPeriod_WarmUp) + seq(length(RunOptions$IndPeriod_Run))
  OutputsModel$Qsim_m3 <- Qsim_m3[iRun]
  OutputsModel$Vsim <- Vsim[iRun]
  class(OutputsModel) <- c("OutputsModelReservoir", class(OutputsModel))
  return(OutputsModel)
}
