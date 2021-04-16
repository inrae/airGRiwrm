#' Create InputsModel object for a **airGRiwrm** network
#'
#' @param x GRiwrm object describing the diagram of the semi-distributed model, see \code{[GRiwrm]}.
#' @param DatesR Vector of POSIXt observation time steps.
#' @param Precip Matrix or data frame of numeric containing precipitation in mm. Column names correspond to node IDs.
#' @param PotEvap Matrix or data frame of numeric containing potential evaporation in mm. Column names correspond to node IDs.
#' @param Qobs Matrix or data frame of numeric containing potential observed flow in mm. Column names correspond to node IDs.
#' @param ... further arguments passed to \code{\link[airGR]{CreateInputsModel}}.
#'
#' @return GRiwrmInputsModel object equivalent to **airGR** InputsModel object for a semi-distributed model (See \code{\link[airGR]{CreateInputsModel}})
#' @export
#' @examples
#' #################################################################
#' # Run the `airGRRunModel_Lag` example in the GRiwrm fashion way #
#' #################################################################
#'
#' # Run airGR RunModel_Lag example for harvesting necessary data
#' library(airGR)
#' example(RunModel_Lag)
#' # detach the package because airGR overwrite airGRiwrm functions here
#' detach("package:airGR")
#'
#' # This example is a network of 2 nodes which can be describe like this:
#' db <- data.frame(id = c("Reservoir", "GaugingDown"),
#'                  length = c(LengthHydro, NA),
#'                  down = c("GaugingDown", NA),
#'                  area = c(NA, BasinInfo$BasinArea),
#'                  model = c(NA, "RunModel_GR4J"),
#'                  stringsAsFactors = FALSE)
#'
#' # Create GRiwrm object from the data.frame
#' griwrm <- GRiwrm(db)
#' str(griwrm)
#'
#' # Formatting observations for the hydrological models
#' # Each input data should be a matrix or a data.frame with the good id in the name of the column
#' Precip <- matrix(BasinObs$P, ncol = 1)
#' colnames(Precip) <- "GaugingDown"
#' PotEvap <- matrix(BasinObs$E, ncol = 1)
#' colnames(PotEvap) <- "GaugingDown"
#'
#' # Observed flows are integrated now because we mix:
#' #  - flows that are directly injected in the model
#' #  - flows that could be used for the calibration of the hydrological models
#' Qobs = matrix(c(Qupstream, BasinObs$Qmm), ncol = 2)
#' colnames(Qobs) <- griwrm$id
#' str(Qobs)
#'
#' InputsModels <- CreateInputsModel(griwrm,
#'                             DatesR = BasinObs$DatesR,
#'                             Precip = Precip,
#'                             PotEvap = PotEvap,
#'                             Qobs = Qobs)
#' str(InputsModels)
#'
CreateInputsModel.GRiwrm <- function(x, DatesR, Precip, PotEvap, Qobs, ...) {

  InputsModel <- CreateEmptyGRiwrmInputsModel(x)
  Qobs[is.na(Qobs)] <- -99 # airGR::CreateInputsModel doesn't accept NA values

  for(id in getNodeRanking(x)) {
    message("CreateInputsModel.GRiwrm: Treating sub-basin ", id, "...")
    InputsModel[[id]] <- CreateOneGRiwrmInputsModel(
      id, x, DatesR,Precip[,id], PotEvap[,id], Qobs, ...
    )
  }
  attr(InputsModel, "TimeStep") <- getModelTimeStep(InputsModel)
  return(InputsModel)
}


#' Create an empty InputsModel object for **airGRiwrm** nodes
#'
#' @param griwrm a `GRiwrm` object (See [GRiwrm])
#'
#' @return \emph{GRiwrmInputsModel} empty object
CreateEmptyGRiwrmInputsModel <- function(griwrm) {
  InputsModel <- list()
  class(InputsModel) <- c("GRiwrmInputsModel", class(InputsModel))
  attr(InputsModel, "GRiwrm") <- griwrm
  return(InputsModel)
}


#' Create one InputsModel for a **airGRiwrm** node
#'
#' @param id string of the node identifier
#' @param griwrm See \code{[GRiwrm]}.
#' @param DatesR vector of dates required to create the GR model and CemaNeige module inputs.
#' @param Precip time series of potential evapotranspiration (catchment average) (mm/time step).
#' @param PotEvap time series of potential evapotranspiration (catchment average) (mm/time step).
#' @param Qobs Matrix or data frame of numeric containing observed flow (mm/time step). Column names correspond to node IDs.
##'
#' @return \emph{InputsModel} object for one.
CreateOneGRiwrmInputsModel <- function(id, griwrm, DatesR, Precip, PotEvap, Qobs) {
  node <- griwrm[griwrm$id == id,]
  FUN_MOD <- griwrm$model[griwrm$id == id]

  # Set hydraulic parameters
  UpstreamNodes <- griwrm$id[griwrm$down == id & !is.na(griwrm$down)]
  Qupstream <- NULL
  LengthHydro <- NULL
  BasinAreas <- NULL

  if(length(UpstreamNodes) > 0) {
    # Sub-basin with hydraulic routing
    Qupstream <- as.matrix(Qobs[ , UpstreamNodes, drop=FALSE])
    LengthHydro <- griwrm$length[griwrm$id %in% UpstreamNodes]
    names(LengthHydro) <- UpstreamNodes
    BasinAreas <- c(
        griwrm$area[griwrm$id %in% UpstreamNodes],
        node$area - sum(griwrm$area[griwrm$id %in% UpstreamNodes], na.rm = TRUE)
    )
    names(BasinAreas) <- c(UpstreamNodes, id)
  }

  # Set model inputs with the **airGR** function
  InputsModel <- CreateInputsModel(
    FUN_MOD,
    DatesR = DatesR,
    Precip = Precip,
    PotEvap = PotEvap,
    Qupstream = Qupstream,
    LengthHydro = LengthHydro,
    BasinAreas = BasinAreas
  )

  # Add Identifiers of connected nodes in order to be able to update them with simulated flows
  InputsModel$id <- id
  InputsModel$down <- node$down
  if(length(UpstreamNodes) > 0) {
    InputsModel$UpstreamNodes <- UpstreamNodes
    InputsModel$UpstreamIsRunoff <- !is.na(griwrm$model[match(UpstreamNodes, griwrm$id)])
  } else {
    InputsModel$BasinAreas <- node$area
  }

  # Add the model function
  InputsModel$FUN_MOD <- FUN_MOD

  return(InputsModel)
}


#' Check time steps of the model of all the nodes and return the time step in seconds
#'
#' This function is called inside [CreateInputsModel.GRiwrm] for defining the time step of the big model.
#'
#' @param InputsModel a `GRiwrmInputsModel`
#'
#' @return A [numeric] representing the time step in seconds
#'
getModelTimeStep <- function(InputsModel) {
  TS <- sapply(InputsModel, function(x) {
    if (inherits(x, "hourly")) {
      TimeStep <- 60 * 60
    } else if (inherits(x, "daily")) {
      TimeStep <- 60 * 60 * 24
    } else {
      stop("All models should be at hourly or daily time step")
    }
  })
  if(length(unique(TS)) != 1) {
    stop("Time steps of the model of all nodes should be identical")
  }
  return(unique(TS))
}
