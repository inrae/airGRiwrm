#######################################################
# Daily time step simulation of a reservoir filled by #
# one catchment supplying a constant released flow    #
#######################################################

library(airGRiwrm)
data(L0123001)

# Inflows comes from a catchment of 360 km² modeled with GR4J
# The reservoir receives directly the inflows
db <- data.frame(id = c(BasinInfo$BasinCode, "Reservoir"),
                 length = c(0, NA),
                 down = c("Reservoir", NA),
                 area = c(BasinInfo$BasinArea, NA),
                 model = c("RunModel_GR4J", "RunModel_Reservoir"),
                 stringsAsFactors = FALSE)
griwrm <- CreateGRiwrm(db)
plot(griwrm)

# Formatting of GR4J inputs for airGRiwrm (matrix or data.frame with one
# column by sub-basin and node IDs as column names)
Precip <- matrix(BasinObs$P, ncol = 1)
colnames(Precip) <- BasinInfo$BasinCode
PotEvap <- matrix(BasinObs$E, ncol = 1)
colnames(PotEvap) <- BasinInfo$BasinCode

# We propose to compute the constant released flow from
# the median of the natural flow
# The value is in m3 by time step (day)
(Qrelease <- median(BasinObs$Qls, na.rm = TRUE) / 1000 * 86400)

# Formatting of reservoir released flow inputs for airGRiwrm (matrix or data.frame
# with one column by node and node IDs as column names)
Qrelease <- data.frame(Reservoir = rep(Qrelease, length(BasinObs$DatesR)))

InputsModel <- CreateInputsModel(griwrm, DatesR = BasinObs$DatesR,
                                 Precip = Precip,
                                 PotEvap = PotEvap,
                                 Qrelease = Qrelease)

## run period selection
Ind_Run <- seq(which(format(BasinObs$DatesR, format = "%Y-%m-%d")=="1990-01-01"),
               which(format(BasinObs$DatesR, format = "%Y-%m-%d")=="1999-12-31"))

# Creation of the GRiwmRunOptions object
RunOptions <- CreateRunOptions(
  InputsModel,
  IndPeriod_Run = Ind_Run,
  IndPeriod_WarmUp = seq.int(Ind_Run[1] - 365, length.out = 365)
)

# Initial states of the reservoir can be provided by the user
# For example for starting with an empty reservoir...
RunOptions[["Reservoir"]]$IniStates <- c("Reservoir.V" = 0)

# calibration criterion: preparation of the InputsCrit object
Qobs <- data.frame("L0123001" = BasinObs$Qmm[Ind_Run])
InputsCrit <- CreateInputsCrit(InputsModel,
                               ErrorCrit_KGE2,
                               RunOptions = RunOptions,
                               Obs = Qobs)

# preparation of CalibOptions object with fixed parameters for the reservoir
Vmax <- 30E6
CalibOptions <-
  CreateCalibOptions(InputsModel,
                     FixedParam = list(Reservoir = c(Vmax = 30E6, celerity = 0.5)))

OC <- Calibration(
  InputsModel = InputsModel,
  RunOptions = RunOptions,
  InputsCrit = InputsCrit,
  CalibOptions = CalibOptions
)

# Model parameters
Param <- lapply(OC, "[[", "ParamFinalR")

# Running simulation
OutputsModel <- RunModel(InputsModel, RunOptions, Param)

# Plot the simulated flows and volumes on all nodes
Qobs <- cbind(BasinObs$Qmm[Ind_Run], Qrelease[Ind_Run, ])
colnames(Qobs) <- griwrm$id
plot(OutputsModel, Qobs = Qobs)
# N.B. "Observed releases" should be considered as "Target releases" here

# The plot for the reservoir can also be plotted alone
plot(OutputsModel$Reservoir)

#######################################################
# Daily time step simulation of a reservoir tracking  #
# an objective filling curve using a local regulation #
#######################################################

# The objective here is to simulate the same reservoir as above
# but with new rules:
# - A minimum flow downstream the reservoir defined as:
(Qmin <- Qrelease[1,] / 2)
# - A maximum release flow due to reservoir outlet limitation
(Qmax <- Qrelease[1,] * 5)
# - An annual objective filling curve managing floods and droughts by
# trying to keep the reservoir volume between 10 and 20 Mm3:
Vobj <- approx(c(1, 120, 300, 366),
               c(20E6, 20E6, 10E6, 20E6),
               seq(366))
plot(Vobj, type = "l", col = "red", lty = 2)

# The regulation function takes InputsModel of the reservoir node and the
# global GRiwrm OutputsModel as arguments and returns a modified
# InputsModel used by RunModel_Reservoir afterward
fun_factory_Regulation_Reservoir <- function(Vini, Vobj, Qmin, Qmax, Vmax) {
  function(InputsModel, RunOptions, OutputsModel) {
    # Release flow time series initialisation
    Qrelease <- rep(0, length(InputsModel$DatesR))
    # Build inflows time series from upstream Qsim (warmup & run)
    Qinflows <- Qrelease
    IPR_all <- c(RunOptions$IndPeriod_WarmUp, RunOptions$IndPeriod_Run)
    Qinflows[IPR_all] <- c(OutputsModel$L0123001$RunOptions$WarmUpQsim_m3,
                           OutputsModel$L0123001$Qsim_m3)
    # Reservoir volume initialisation
    V <- Vini
    # Loop over simulation time steps (warmup & run periods)
    for(ts in IPR_all) {
      # Update reservoir volume with inflows
      V <- V + Qinflows[ts]
      # Rule #1: follow the objective filling curve (lower priority)
      j <- as.numeric(format(InputsModel$DatesR[ts], "%j"))
      Vobj_ts <- approx(Vobj, xout = j)$y
      Qrelease[ts] <- V - Vobj_ts
      # Rule #2: Release cannot be less than Qmin
      Qrelease[ts] <- max(Qmin, Qrelease[ts])
      # Rule #3: Release cannot be more than Qmax
      Qrelease[ts] <- min(Qmax, Qrelease[ts])
      # Update reservoir volume after release
      V <- V - Qrelease[ts]
      # Rule #4: hard constraints on the reservoir (full or empty?)
      if (V < 0) {
        Qrelease[ts] <- Qrelease[ts] + V
        V <- 0
      }
      V <- min(V, Vmax)
    }
    InputsModel$Qrelease <- Qrelease
    return(InputsModel)
  }
}
# A call to fun_factory_Regulation_Reservoir returns the regulation
# function with the parameters Qmin, Qmax, Vobj enclosed in the environment
# of the function
Regulation_Reservoir <-
  fun_factory_Regulation_Reservoir(RunOptions$Reservoir$IniStates, Vobj, Qmin, Qmax, Vmax)

# Then we need to update InputsModel in order to take into account the regulation
# function instead of predefined Qrelease in the previous study case
IM_reg <- CreateInputsModel(griwrm,
                            DatesR = BasinObs$DatesR,
                            Precip = Precip,
                            PotEvap = PotEvap,
                            Qrelease = Qrelease,
                            FUN_REGUL = list(Reservoir = Regulation_Reservoir))

# And we can finally run the simulation!
OM_reg <- RunModel(IM_reg, RunOptions, Param)

# And plot the new result
plot(OM_reg$Reservoir)
