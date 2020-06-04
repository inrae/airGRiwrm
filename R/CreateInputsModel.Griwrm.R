#' Create InputsModel object for a GRIWRM network
#'
#' @param x Ginet object describing the diagram of the semi-distributed model, see \code{[Ginet]}.
#' @param girop Girop object giving the run-off model parameters, see \code{[Girop]}.
#' @param gits Gits object giving the observation time series, see \code{[Gits]}.
#' @param verbose (optional) boolean indicating if the function is run in verbose mode or not, default = \code{TRUE}
#' @param ... further arguments passed to \code{\link[airGR]{CreateInputsModel}}.
#'
#' @return GriwrmInputsModel object equivalent to airGR InputsModel object for a semi-distributed model (See \code{\link[airGR]{CreateInputsModel}})
#' @export
CreateInputsModel.Griwrm <- function(x, girop, gits, verbose = TRUE, ...) {

  InputsModel <- CreateEmptyGriwrmInputsModel()

  for(id in getNodeRanking(x)) {
    if(verbose) cat("CreateInputsModel.griwrm: Treating sub-basin", id, "...\n")
    InputsModel[[id]] <- CreateOneGriwrmInputsModel(id, x, girop, gits, ...)
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
#' @param ginet See \code{[Ginet]}.
#' @param girop See \code{[Girop]}.
#' @param gits See \code{[Gits]}.
#'
#' @return \emph{InputsModel} object for one.
CreateOneGriwrmInputsModel <- function(id, ginet, girop, gits) {
  node <- ginet[ginet$id == id,]
  FUN_MOD <- girop$model[girop$id == id]

  # Set hydraulic parameters
  UpstreamNodes <- ginet$id[ginet$down == id & !is.na(ginet$down)]
  Qupstream <- NULL
  LengthHydro <- NULL
  BasinAreas <- NULL

  if(length(UpstreamNodes) > 0) {
    # Sub-basin with hydraulic routing
    for(idUpstrNode in UpstreamNodes) {
      Qupstream1 <- matrix(gits[[idUpstrNode]]$Qobs, ncol = 1)
      if(is.null(Qupstream)) {
        Qupstream <- Qupstream1
      } else {
        Qupstream <- cbind(Qupstream, Qupstream1)
      }
    }
    LengthHydro <- matrix(ginet$length[girop$id %in% UpstreamNodes] , nrow = 1)
    BasinAreas <- matrix(
      c(
        girop$area[girop$id %in% UpstreamNodes],
        girop$area[girop$id == id] - sum(girop$area[girop$id %in% UpstreamNodes])
      ),
      nrow = 1
    )
  }

  # Set model inputs with the airGR function
  InputsModel <- CreateInputsModel(
    FUN_MOD,
    DatesR = gits$date,
    Precip = gits[[id]]$Precip,
    PotEvap = gits[[id]]$PotEvap,
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
