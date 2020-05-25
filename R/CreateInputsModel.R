#' Create InputsModel object for either airGR or GRIWRM
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
CreateInputsModel <- function(x, ...) {
  UseMethod("CreateInputsModel", x)
}

#' Wrapper for the airGR::CreateInputsModel function
#'
#' @param FUN_MOD
#' @param DatesR
#' @param Precip
#' @param PrecipScale
#' @param PotEvap
#' @param TempMean
#' @param TempMin
#' @param TempMax
#' @param ZInputs
#' @param HypsoData
#' @param NLayers
#' @param QobsUpstr
#' @param LengthHydro
#' @param BasinAreas
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples
CreateInputsModel.default <- function(FUN_MOD,
                              DatesR,
                              Precip, PrecipScale = TRUE,
                              PotEvap = NULL,
                              TempMean = NULL, TempMin = NULL, TempMax = NULL,
                              ZInputs = NULL, HypsoData = NULL, NLayers = 5,
                              QobsUpstr = NULL, LengthHydro = NULL, BasinAreas = NULL,
                              verbose = TRUE) {
  airGR::CreateInputsModel(FUN_MOD, DatesR, Precip, PrecipScale, PotEvap,
                           TempMean, TempMin, TempMax, ZInputs, HypsoData, NLayers,
                           QobsUpstr, LengthHydro, BasinAreas, verbose)
}


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
}


#' Create an empty InputsModel object for GRIWRM
#'
#' @return
#' @export
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
#' @export
#'
#' @examples
CreateOneGriwrmInputsModel <- function(id, ginet, girop, gits) {
  node <- ginet[ginet$id == id,]
  FUN_MOD <- girop$model[girop$id == id]

  # Set hydraulic parameters
  UpstrNodes <- ginet$id[ginet$down == id & !is.na(ginet$down)]
  QobsUpstr <- NULL
  LengthHydro <- NULL
  BasinAreas <- NULL

  if(length(UpstrNodes) > 0) {
    # Sub-basin with hydraulic routing
    for(idUpstrNode in UpstrNodes) {
      QobsUpstr1 <- matrix(gits[[idUpstrNode]]$Qobs, ncol = 1)
      if(is.null(QobsUpstr)) {
        QobsUpstr <- QobsUpstr1
      } else {
        QobsUpstr <- cbind(QobsUpstr, QobsUpstr1)
      }
    }
    LengthHydro <- matrix(ginet$length[girop$id %in% UpstrNodes] , nrow = 1)
    BasinAreas <- matrix(
      c(
        girop$area[girop$id %in% UpstrNodes],
        girop$area[girop$id == id] - sum(girop$area[girop$id %in% UpstrNodes])
      ),
      nrow = 1
    )
  }

  # Set model inputs
  CreateInputsModel(
    FUN_MOD,
    DatesR = gits$date,
    Precip = gits[[id]]$Precip,
    PotEvap = gits[[id]]$PotEvap,
    QobsUpstr = QobsUpstr,
    LengthHydro = LengthHydro,
    BasinAreas = BasinAreas
  )
}
