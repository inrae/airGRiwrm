#' Create InputsModel object for a GRIWRM network
#'
#' @param ginet
#' @param girop
#' @param gits
#'
#' @return
#' @export
#'
#' @examples
CreateInputsModel.Griwrm <- function(ginet, girop, gits, verbose = TRUE) {

  InputsModel <- CreateEmptyGriwrmInputsModel()

  for(id in getNodeRanking(ginet)) {
    if(verbose) cat("CreateInputsModel.griwrm: Treating sub-basin", id, "...\n")
    InputsModel[[id]] <- CreateOneGriwrmInputsModel(id, ginet, girop, gits)
  }
  return(InputsModel)
}


#' Create an empty InputsModel object for GRIWRM nodes
#'
#' @return
#'
#' @examples
CreateEmptyGriwrmInputsModel <- function() {
  InputsModel <- list()
  class(InputsModel) <- append(class(InputsModel), "GriwrmInputsModel")
  return(InputsModel)
}


#' Create one InputsModel for a GRIWRM node
#'
#' @param ginet
#' @param girop
#' @param gits
#' @param id
#'
#' @return
#'
#' @examples
CreateOneGriwrmInputsModel <- function(id, ginet, girop, gits) {
  node <- ginet[ginet$id == id,]
  FUN_MOD <- girop$model[girop$id == id]

  # Set hydraulic parameters
  UpstreamNodes <- ginet$id[ginet$down == id & !is.na(ginet$down)]
  QobsUpstr <- NULL
  LengthHydro <- NULL
  BasinAreas <- NULL

  if(length(UpstreamNodes) > 0) {
    # Sub-basin with hydraulic routing
    for(idUpstrNode in UpstreamNodes) {
      QobsUpstr1 <- matrix(gits[[idUpstrNode]]$Qobs, ncol = 1)
      if(is.null(QobsUpstr)) {
        QobsUpstr <- QobsUpstr1
      } else {
        QobsUpstr <- cbind(QobsUpstr, QobsUpstr1)
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
    QobsUpstr = QobsUpstr,
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
