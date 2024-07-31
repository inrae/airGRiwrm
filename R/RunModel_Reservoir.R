#' Run with a reservoir model
#'
#' The reservoir model is a model combining a lag model and the calculation of
#' the water storage time series according to the released flow time series
#' from the `Qrelease` parameter of [CreateInputsModel.GRiwrm].
#'
#' @details
#' The simulated flow corresponds to the released flow except when the reservoir
#' is empty (release flow is limited) or full (release flow is completed by inflows
#' excess).
#'
#' By default, the initial reservoir volume at the beginning of the warm-up period
#' is equal to the half of the maximum reservoir capacity.
#'
#' The parameters of the model can't be calibrated and must be fixed during the
#' calibration process by using this instruction after the call to
#' [CreateCalibOptions]:
#'
#' `CalibOptions[[id_of_the_reservoir]]$FixedParam <- c(Vmax, celerity)`
#'
#' Initial states of the model consists in the initial volume storage in the
#' reservoir and can be defined with the following instruction after the call to
#' [CreateRunOptions.GRiwrmInputsModel]:
#'
#' `RunOptions[[id_of_the_reservoir]]$IniStates <- c("Reservoir.V" = initial_volume_m3)`
#'
#' The final state of the reservoir is stored in `OutputsModel$StateEnd` and
#' can be reused for starting a new simulation with the following instruction:
#'
#' `RunOptions[[id_of_the_reservoir]]$IniStates <- unlist(OutputsModel[[id_of_the_reservoir]]$StateEnd)`
#'
#' Direct injection nodes connected to a reservoir nodes act as water injections
#' or withdrawals directly in the reservoir volume (whatever the length between
#' the direct injection nodes and the reservoir node). The abstraction volumes
#' that cannot be operated due to an empty reservoir are notified by the
#' item `Qover_m3` in the returned *OutputsModel* object.
#'
#' @inheritParams airGR::RunModel
#' @param Param [numeric] vector of length 2 containing (1) the maximum capacity
#' of the reservoir and (2) the celerity in m/s of the upstream inflows.
#'
#' @return An *OutputsModel* object like the one return by [airGR::RunModel] but
#' completed with the items:
#' - `Vsim`: representing the water volume time series in m3
#' - `Qsim_m3`: flow released by the reservoir in cubic meters by time step
#' (see Details)
#' - `Qdiv_m3`: only present in case of Diversion in the node, diverted flow in
#' cubic meters per time step. The latter differs from the flows time series provided
#' in argument `Qinf` of [CreateInputsModel.GRiwrm] by the limitation due to an
#' empty reservoir
#' - `Qover_m3`: only present in case of Diversion in the node, diverted volumes
#' that cannot be operated due to an empty reservoir
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

  # Time parameters
  IndPerWarmUp <- RunOptions$IndPeriod_WarmUp[RunOptions$IndPeriod_WarmUp > 0]
  IndPerTot   <- c(IndPerWarmUp, RunOptions$IndPeriod_Run)
  iPerTot <- seq(length(IndPerTot))

  # Relocate upstream direct injection into the reservoir
  Qdirect <- InputsModel$Qupstream[IndPerTot, !InputsModel$UpstreamIsModeled, drop = FALSE]
  InputsModel$Qupstream <-  InputsModel$Qupstream[, InputsModel$UpstreamIsModeled, drop = FALSE]
  InputsModel$LengthHydro <- InputsModel$LengthHydro[InputsModel$UpstreamIsModeled]
  InputsModel$BasinAreas <- InputsModel$BasinAreas[c(InputsModel$UpstreamIsModeled, TRUE)]
  InputsModel$UpstreamNodes <- InputsModel$UpstreamNodes[InputsModel$UpstreamIsModeled]
  InputsModel$UpstreamVarQ <- InputsModel$UpstreamVarQ[InputsModel$UpstreamIsModeled]
  InputsModel$UpstreamIsModeled <- InputsModel$UpstreamIsModeled[InputsModel$UpstreamIsModeled]

  # Compute inflows with RunModel_Lag
  if (ncol(InputsModel$Qupstream) > 0) {
    OutputsModel <- RunModel.SD(InputsModel,
                                RunOptions,
                                Param = celerity)
    names(OutputsModel)[names(OutputsModel) == "Qsim_m3"] <- "Qinflows_m3"
    OutputsModel$Qsim <- NULL
    OutputsModel$RunOptions$WarmUpQsim <- NULL
    Qinflows_m3 <- c(OutputsModel$RunOptions$WarmUpQsim_m3,
                     OutputsModel$Qinflows_m3)
  } else {
    OutputsModel <- list(
      DatesR = InputsModel$DatesR[RunOptions$IndPeriod_Run]
    )
    class(OutputsModel) <- c("OutputsModel", class(OutputsModel))
    Qinflows_m3 <- rep(0, length(IndPerTot))
  }
  if (ncol(Qdirect) > 0) {
    if (ncol(Qdirect) > 1) Qdirect <- rowSums(Qdirect)
    Qinflows_m3 <- Qinflows_m3 + Qdirect
  } else {
    Qdirect <- NULL
  }

  # Reservoir initial conditions
  V0 <- RunOptions$IniStates["Reservoir.V"]
  if (is.na(V0)) {
    V0 <- Vmax / 2
  }

  # Initiation of output variables
  Vsim <- rep(0, length(IndPerTot))
  Qover_m3 <- rep(0, length(IndPerTot))
  Qsim_m3 <- Vsim
  if (InputsModel$hasDiversion) {
    Qdiv_m3 <- Vsim
  }

  # Time series volume and release calculation
  for (i in iPerTot) {
    Vsim[i] <- V0 + Qinflows_m3[i]
    if (Vsim[i] < 0) {
      Qover_m3[i] <- -Vsim[i]
      Vsim[i] <- 0
    }
    if (InputsModel$hasDiversion) {
      Qdiv_m3[i] <- min(Vsim[i] + InputsModel$Qmin[IndPerTot[i]], InputsModel$Qdiv[IndPerTot[i]])
      Vsim[i] <- Vsim[i] - Qdiv_m3[i]
    }
    Qsim_m3[i] <- min(Vsim[i], InputsModel$Qrelease[IndPerTot[i]])
    Vsim[i] <- Vsim[i] - Qsim_m3[i]
    if (Vsim[i] > Vmax) {
      Qsim_m3[i] <- Qsim_m3[i] + Vsim[i] - Vmax
      Vsim[i] <- Vmax
    }
    V0 <- Vsim[i]
  }

  # Format OutputsModel
  if (length(IndPerWarmUp) > 0) {
    iWarmUp <- seq(length(RunOptions$IndPeriod_WarmUp))
    OutputsModel$RunOptions$WarmUpQsim_m3 <- Qsim_m3[iWarmUp]
    OutputsModel$RunOptions$WarmUpVsim <- Vsim[iWarmUp]
    if (InputsModel$hasDiversion) {
      OutputsModel$RunOptions$WarmUpQdiv_m3 <- Qdiv_m3[iWarmUp]
    }
  }
  iRun <- length(IndPerWarmUp) + seq(length(RunOptions$IndPeriod_Run))
  OutputsModel$Qsim_m3 <- Qsim_m3[iRun]
  OutputsModel$Vsim <- Vsim[iRun]
  if (!is.null(Qdirect)) {
    OutputsModel$Qover_m3 <- Qover_m3[iRun]
  }
  if (InputsModel$hasDiversion) {
    OutputsModel$Qdiv_m3 <- Qdiv_m3[iRun]
  }
  OutputsModel$StateEnd$Reservoir <- list(V = Vsim[length(Vsim)])
  class(OutputsModel) <- c("OutputsModelReservoir", class(OutputsModel))
  return(OutputsModel)
}
