% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.OutputsModelReservoir.R
\name{plot.OutputsModelReservoir}
\alias{plot.OutputsModelReservoir}
\title{Plot simulated reservoir volume, inflows and released flows time series on a reservoir node}
\usage{
\method{plot}{OutputsModelReservoir}(x, Qobs = NULL, ...)
}
\arguments{
\item{x}{Object returned by \link{RunModel_Reservoir}}

\item{Qobs}{(optional) \link{numeric} time series of observed released flow [m3/time step]}

\item{...}{Further arguments passed to \link{plot.Qm3s}}
}
\value{
Function used for side effect.
}
\description{
Plot simulated reservoir volume, inflows and released flows time series on a reservoir node
}
\examples{
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
\dontrun{
plot(griwrm)
}

# Formatting of GR4J inputs for airGRiwrm (matrix or data.frame with one
# column by sub-basin and node IDs as column names)
Precip <- matrix(BasinObs$P, ncol = 1)
colnames(Precip) <- BasinInfo$BasinCode
PotEvap <- matrix(BasinObs$E, ncol = 1)
colnames(PotEvap) <- BasinInfo$BasinCode

# We propose to compute the constant released flow from
# the median of the natural flow
# The value is in m3 by time step (day)
Qrelease <- median(BasinObs$Qls, na.rm = TRUE) / 1000 * 86400

# Formatting of reservoir released flow inputs for airGRiwrm (matrix or data.frame
# with one column by node and node IDs as column names)
Qrelease <- data.frame(Reservoir = rep(Qrelease, length(BasinObs$DatesR)))

InputsModel <- CreateInputsModel(griwrm, DatesR = BasinObs$DatesR,
                                 Precip = Precip,
                                 PotEvap = PotEvap,
                                 Qinf = Qrelease)

## run period selection
Ind_Run <- seq(which(format(BasinObs$DatesR, format = "\%Y-\%m-\%d")=="1990-01-01"),
               which(format(BasinObs$DatesR, format = "\%Y-\%m-\%d")=="1999-12-31"))

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

# preparation of CalibOptions object
CalibOptions <- CreateCalibOptions(InputsModel)

# Parameters of RunModel_Reservoir must be fixed
CalibOptions[["Reservoir"]]$FixedParam <- c(Vmax = 30E6, celerity = 0.5)

OC <- Calibration(
  InputsModel = InputsModel,
  RunOptions = RunOptions,
  InputsCrit = InputsCrit,
  CalibOptions = CalibOptions
)

# Model parameters
Param <- extractParam(OC)
str(Param)

# Running simulation
OutputsModel <- RunModel(InputsModel, RunOptions, Param)

# Plot the simulated flows and volumes on all nodes
Qobs <- cbind(BasinObs$Qmm[Ind_Run], Qrelease[Ind_Run, ])
colnames(Qobs) <- griwrm$id
plot(OutputsModel, Qobs = Qobs)
# N.B. "Observed releases" should be considered as "Target releases" here

# The plot for the reservoir can also be plotted alone
plot(OutputsModel$Reservoir, Qobs = Qobs[, "Reservoir"])
}
