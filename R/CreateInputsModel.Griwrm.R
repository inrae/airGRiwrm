#' Create InputsModel object for a GRIWRM network
#'
#' @param x Griwrm object describing the diagram of the semi-distributed model, see \code{[Griwrm]}.
#' @param DateR Vector of POSIXlt observation time steps.
#' @param Precip Matrix or data frame of numeric containing precipitation in mm. Column names correspond to node IDs.
#' @param PotEvap Matrix or data frame of numeric containing potential evaporation in mm. Column names correspond to node IDs.
#' @param Qobs Matrix or data frame of numeric containing potential observed flow in mm. Column names correspond to node IDs.
#' @param verbose (optional) boolean indicating if the function is run in verbose mode or not, default = \code{TRUE}
#' @param ... further arguments passed to \code{\link[airGR]{CreateInputsModel}}.
#'
#' @return GriwrmInputsModel object equivalent to airGR InputsModel object for a semi-distributed model (See \code{\link[airGR]{CreateInputsModel}})
#' @export
CreateInputsModel.Griwrm <- function(x, DatesR, Precip, PotEvap, Qobs, verbose = TRUE, ...) {

  InputsModel <- CreateEmptyGriwrmInputsModel()
  Qobs[is.na(Qobs)] <- -99 # airGRCreateInputsModel doesn't accept NA values

  for(id in getNodeRanking(x)) {
    if(verbose) cat("CreateInputsModel.griwrm: Treating sub-basin", id, "...\n")
    InputsModel[[id]] <- CreateOneGriwrmInputsModel(
      id, x, DatesR,Precip[,id], PotEvap[,id], Qobs, ...
    )
  }
  return(InputsModel)
}


#' Create an empty InputsModel object for GRIWRM nodes
#'
#' @return \emph{GriwrmInputsModel} empty object
CreateEmptyGriwrmInputsModel <- function() {
  InputsModel <- list()
  class(InputsModel) <- append(class(InputsModel), "GriwrmInputsModel")
  return(InputsModel)
}


#' Create one InputsModel for a GRIWRM node
#'
#' @param id string of the node identifier
#' @param griwrm See \code{[Griwrm]}.
#' @param DatesR vector of dates required to create the GR model and CemaNeige module inputs.
#' @param Precip time series of potential evapotranspiration (catchment average) (mm/time step).
#' @param PotEvap time series of potential evapotranspiration (catchment average) (mm/time step).
#' @param Qobs Matrix or data frame of numeric containing observed flow (mm/time step). Column names correspond to node IDs.
##'
#' @return \emph{InputsModel} object for one.
CreateOneGriwrmInputsModel <- function(id, griwrm, DatesR, Precip, PotEvap, Qobs) {
  node <- griwrm[griwrm$id == id,]
  FUN_MOD <- griwrm$model[griwrm$id == id]

  # Set hydraulic parameters
  UpstreamNodes <- griwrm$id[griwrm$down == id & !is.na(griwrm$down)]
  Qupstream <- NULL
  LengthHydro <- NULL
  BasinAreas <- NULL

  if(length(UpstreamNodes) > 0) {
    # Sub-basin with hydraulic routing
    for(idUpstrNode in UpstreamNodes) {
      Qupstream1 <- matrix(Qobs[,idUpstrNode], ncol = 1)
      if(is.null(Qupstream)) {
        Qupstream <- Qupstream1
      } else {
        Qupstream <- cbind(Qupstream, Qupstream1)
      }
    }
    LengthHydro <- griwrm$length[griwrm$id %in% UpstreamNodes]
    BasinAreas <- c(
        griwrm$area[griwrm$id %in% UpstreamNodes],
        node$area - sum(griwrm$area[griwrm$id %in% UpstreamNodes])
    )
  }

  # Set model inputs with the airGR function
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
  if(length(UpstreamNodes) > 0) {
    InputsModel$UpstreamNodes <- UpstreamNodes
  }
  # Add the model function
  InputsModel$FUN_MOD <- FUN_MOD

  return(InputsModel)
}
