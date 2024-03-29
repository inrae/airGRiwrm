% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CreateRunOptions.GRiwrmInputsModel.R,
%   R/CreateRunOptions.R
\name{CreateRunOptions.GRiwrmInputsModel}
\alias{CreateRunOptions.GRiwrmInputsModel}
\alias{CreateRunOptions}
\alias{CreateRunOptions.InputsModel}
\alias{CreateRunOptions.character}
\alias{CreateRunOptions.function}
\title{Creation of the RunOptions object}
\usage{
\method{CreateRunOptions}{GRiwrmInputsModel}(x, IniStates = NULL, ...)

CreateRunOptions(x, ...)

\method{CreateRunOptions}{InputsModel}(x, ...)

\method{CreateRunOptions}{character}(x, ...)

\method{CreateRunOptions}{`function`}(x, ...)
}
\arguments{
\item{x}{For a single catchment, it can be an object of class \emph{InputsModel} or a \link{function} or a \link{character} corresponding to \code{FUN_MOD} (compliant with \strong{airGR} call). For a network, it should be an object of class \emph{GRiwrmInputsModel}. See \link{CreateInputsModel} for details}

\item{IniStates}{(optional) \link{numeric} object or \link{list} of \link{numeric} object of class \emph{IniStates}, see \link[airGR:CreateIniStates]{airGR::CreateIniStates} for details}

\item{...}{arguments passed to \link[airGR:CreateRunOptions]{airGR::CreateRunOptions}, see details}
}
\value{
Depending on the class of \code{InputsModel} argument (respectively \emph{InputsModel} and \emph{GRiwrmInputsModel} object), the returned value is respectively:
\itemize{
\item a \code{RunOptions} object (See \link[airGR:CreateRunOptions]{airGR::CreateRunOptions})
\item a \code{GRiwrmRunOptions} object which is a \link{list} of \code{RunOptions} objects with one item per modeled sub-catchment
}
}
\description{
This function can be used either for a catchment (with an \emph{InputsModel} object) or for a network (with a \emph{GRiwrmInputsModel} object)
}
\details{
See \link[airGR:CreateRunOptions]{airGR::CreateRunOptions} documentation for a complete list of arguments.

If \code{InputsModel} argument is a \emph{GRiwrmInputsModel} object, \code{IniStates} must be a list of \link{numeric} object of class \emph{IniStates} with one item per modeled sub-catchment.

With a \emph{GRiwrmInputsModel} object, all arguments are applied on each sub-catchments of the network.
}
\examples{
###################################################################
# Run the `airGR::RunModel_Lag` example in the GRiwrm fashion way #
# Simulation of a reservoir with a purpose of low-flow mitigation #
###################################################################

## ---- preparation of the InputsModel object

## loading package and catchment data
library(airGRiwrm)
data(L0123001)

## ---- specifications of the reservoir

## the reservoir withdraws 1 m3/s when it's possible considering the flow observed in the basin
Qupstream <- matrix(-sapply(BasinObs$Qls / 1000 - 1, function(x) {
  min(1, max(0, x, na.rm = TRUE))
}), ncol = 1)

## except between July and September when the reservoir releases 3 m3/s for low-flow mitigation
month <- as.numeric(format(BasinObs$DatesR, "\%m"))
Qupstream[month >= 7 & month <= 9] <- 3
Qupstream <- Qupstream * 86400 ## Conversion in m3/day

## the reservoir is not an upstream subcachment: its areas is NA
BasinAreas <- c(NA, BasinInfo$BasinArea)

## delay time between the reservoir and the catchment outlet is 2 days and the distance is 150 km
LengthHydro <- 150
## with a delay of 2 days for 150 km, the flow velocity is 75 km per day
Velocity <- (LengthHydro * 1e3 / 2) / (24 * 60 * 60) ## Conversion km/day -> m/s

# This example is a network of 2 nodes which can be describe like this:
db <- data.frame(id = c("Reservoir", "GaugingDown"),
                 length = c(LengthHydro, NA),
                 down = c("GaugingDown", NA),
                 area = c(NA, BasinInfo$BasinArea),
                 model = c(NA, "RunModel_GR4J"),
                 stringsAsFactors = FALSE)

# Create GRiwrm object from the data.frame
griwrm <- CreateGRiwrm(db)
str(griwrm)

# Formatting observations for the hydrological models
# Each input data should be a matrix or a data.frame with the good id in the name of the column
Precip <- matrix(BasinObs$P, ncol = 1)
colnames(Precip) <- "GaugingDown"
PotEvap <- matrix(BasinObs$E, ncol = 1)
colnames(PotEvap) <- "GaugingDown"

# Observed flows contain flows that are directly injected in the model
Qobs = matrix(Qupstream, ncol = 1)
colnames(Qobs) <- "Reservoir"

# Creation of the GRiwrmInputsModel object (= a named list of InputsModel objects)
InputsModels <- CreateInputsModel(griwrm,
                            DatesR = BasinObs$DatesR,
                            Precip = Precip,
                            PotEvap = PotEvap,
                            Qobs = Qobs)
str(InputsModels)

## run period selection
Ind_Run <- seq(which(format(BasinObs$DatesR, format = "\%Y-\%m-\%d")=="1990-01-01"),
               which(format(BasinObs$DatesR, format = "\%Y-\%m-\%d")=="1999-12-31"))

# Creation of the GriwmRunOptions object
RunOptions <- CreateRunOptions(InputsModels,
                                IndPeriod_Run = Ind_Run)
str(RunOptions)

# Parameters of the SD models should be encapsulated in a named list
ParamGR4J <- c(X1 = 257.238, X2 = 1.012, X3 = 88.235, X4 = 2.208)
Param <- list(`GaugingDown` = c(Velocity, ParamGR4J))

# RunModel for the whole network
OutputsModels <- RunModel(InputsModels,
                          RunOptions = RunOptions,
                          Param = Param)
str(OutputsModels)

# Compare Simulation with reservoir and observation of natural flow
plot(OutputsModels, data.frame(GaugingDown = BasinObs$Qmm[Ind_Run]))
}
